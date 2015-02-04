open Ast
open Type
open Common
open Llvm
open Genc_shared

type field_lut = (tclass_field * int) list

type module_extra = {
	m_path : path;
	m_class : tclass;
	mutable m_field_lut : field_lut;
	mutable m_num_closures : int;
	mutable m_dependencies : (path,bool) PMap.t;
}

type haxe_types = {
	box_int8 : lltype;
	box_int16 : lltype;
	box_int32 : lltype;
	box_int64 : lltype;
	null_float : lltype;
	null_bool : lltype;
	type_parameter : lltype;
	var_args : lltype;
}

type context = {
	com : Common.context;
	llvm : llcontext;
	t : haxe_types;
	mutable modules : (path,module_extra) PMap.t;
	mutable named_structs : (string,lltype) PMap.t;
	mutable static_inits : (tclass * tclass_field) list;
	(* per module *)
	mutable curmod_extra : module_extra;
	mutable curmod : llmodule;
	(* per field *)
	mutable vars : (int,llvalue) PMap.t;
	mutable merge_block : llbasicblock option;
	mutable cond_block : llbasicblock option;
	mutable vthis : llvalue;
	mutable builder : llbuilder;
	mutable landing_pad : llbasicblock option;
	mutable verification_failures : (string * string) list;
}

let rec follow t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow t
		| _ -> t)
	| TLazy f ->
		follow (!f())
	| TType (t,tl) ->
		follow (apply_params t.t_params tl t.t_type)
	| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) && not (Meta.has Meta.NoFollow a.a_meta) ->
		follow (Abstract.get_underlying_type a pl)
	| _ -> t

let is_main p = p.pfile = "src/Main.hx"

let cf_name_vtable = mk_runtime_prefix "v_table"
let cf_name_init = mk_runtime_prefix "init"
let cf_name_static_init = mk_runtime_prefix "static_init"
let cf_name_typeref = mk_runtime_prefix "typeref"

let debug b s f fr =
	if b then begin
		print_endline s;
		let r = f() in
		print_endline ("OK " ^ (fr r) ^ "\n");
		r
	end else
		f()

let create_module_extra ctx c =
	let path = c.cl_path in
	let m_extra = {
		m_path = path;
		m_field_lut = [];
		m_class = c;
		m_num_closures = 0;
		m_dependencies = PMap.empty;
	} in
	ctx.modules <- PMap.add path m_extra ctx.modules;
	m_extra

let close_module ctx =
	let path = ctx.curmod_extra.m_path in
	Common.mkdir_recursive ctx.com.file (fst path);
	let dir = ctx.com.file :: fst path in
	let file_path_no_ext = String.concat "/" dir ^ "/" ^ (snd path) in
	let write_if_changed filepath content =
		try
			let cur = Std.input_file ~bin:true filepath in
			if cur <> content then raise Not_found
		with Not_found | Sys_error _ ->
			let ch_h = open_out_bin filepath in
			print_endline ("Writing " ^ filepath);
			output_string ch_h content;
			close_out ch_h;
	in
	write_if_changed (file_path_no_ext ^ ".ll") (string_of_llmodule ctx.curmod);
	begin match Llvm_analysis.verify_module ctx.curmod with
		| None -> ()
		| Some s -> ctx.verification_failures <- (s_type_path path,s) :: ctx.verification_failures;
	end;
	dispose_module ctx.curmod

let begin_field ctx =
	let old_vars,old_merge,old_cond,old_vthis,old_builder,old_landing_pad = ctx.vars,ctx.merge_block,ctx.cond_block,ctx.vthis,ctx.builder,ctx.landing_pad in
	ctx.vars <- PMap.empty;
	ctx.merge_block <- None;
	ctx.cond_block <- None;
	ctx.landing_pad <- None;
	(fun () ->
		ctx.vars <- old_vars;
		ctx.merge_block <- old_merge;
		ctx.cond_block <- old_cond;
		ctx.vthis <- old_vthis;
		ctx.builder <- old_builder;
		ctx.landing_pad <- old_landing_pad;
	)

let begin_loop ctx bb_cond bb_merge =
	let old_merge = ctx.merge_block in
	let old_cond = ctx.cond_block in
	ctx.merge_block <- Some bb_merge;
	ctx.cond_block <- Some bb_cond;
	(fun () ->
		ctx.merge_block <- old_merge;
		ctx.cond_block <- old_cond;
	)

let ll_value_info v = (string_of_llvalue v) ^ ":" ^ (string_of_lltype (type_of v))

(* Paths and names *)

let path_to_name (pack,name) = match pack with [] -> name | _ -> String.concat "_" pack ^ "_" ^ name

let get_native_class_name c =
	path_to_name c.cl_path

let get_native_name meta =
	try begin
		match Meta.get Meta.Native meta with
			| _,[EConst (String s),_],_ -> Some s
			| _,_,_ -> None
	end with Not_found ->
		None

let full_field_name c cf =
	if Meta.has Meta.Plain cf.cf_meta then cf.cf_name
	else match get_native_name cf.cf_meta with
		| Some n -> n
		| None -> (path_to_name c.cl_path) ^ "_" ^ cf.cf_name

(* Types *)

let default_pointer ctx =
	pointer_type (i32_type ctx.llvm)

let null ctx =
	const_null (default_pointer ctx)

let named_struct_type ctx name =
	try
		PMap.find name ctx.named_structs
	with Not_found ->
		let t = named_struct_type ctx.llvm name in
		ctx.named_structs <- PMap.add name t ctx.named_structs;
		t

let get_anon_fields an =
	PMap.fold (fun cf acc -> cf :: acc) an.a_fields []

let haxe_string_type ctx =
	(pointer_type (named_struct_type ctx "String"))

let get_type_path t p = match follow t with
		| TInst(c,_) -> c.cl_path
		| TEnum(en,_) -> en.e_path
		| TAbstract(a,_) -> a.a_path
		| _ -> error (Printf.sprintf "Type %s has no path" (s_type (print_context()) t)) p

let get_typeref_name ctx path p =
	(path_to_name path) ^ "_" ^ cf_name_typeref

let get_type_postfix t = match follow t with
	| TInst({cl_path=[],"String"},_) -> "_string"
	| TAbstract(a,tps) when Meta.has (Meta.Custom ":int") a.a_meta ->
		let (meta,el,epos) = Meta.get (Meta.Custom ":int") a.a_meta in
		begin match el with
			| [(EConst (String s),_)] ->
				begin match s with
				| "int64" -> "_64"
				| "int32" -> "_62"
				| "int16" -> "_16"
				| "int8"  -> "_8"
				| _ -> assert false
				end
			| _ ->
				assert false
		end
	| TAbstract(a,tl) ->
		begin match a.a_path with
			| [],"Int" -> "_32"
			| [],"Float" -> "_double"
			| [],"Bool" -> "_1"
			| _ -> "_dyn"
		end
	| _ ->
		"_dyn"

let rec convert_type ctx t =
	match follow t with
	| TInst({cl_kind = KTypeParameter _},_) ->
		ctx.t.type_parameter
	| TInst(c,tl) ->
		let ll_t = named_struct_type ctx (get_native_class_name c) in
		pointer_type ll_t
	| TAbstract(a,tps) when Meta.has (Meta.Custom ":int") a.a_meta ->
		let (meta,el,epos) = Meta.get (Meta.Custom ":int") a.a_meta in
		begin match el with
			| [(EConst (String s),_)] ->
				begin match s with
				| "int64" | "uint64" -> i64_type ctx.llvm
				| "int32" -> i32_type ctx.llvm
				| "int16" -> i16_type ctx.llvm
				| "int8"  -> i8_type ctx.llvm
				| _ -> prerr_endline ("Unknown :int type: " ^ s); assert false
				end
			| _ ->
				assert false
		end
	| TAbstract(a,tl) ->
		begin match a.a_path with
			| [],"Int" ->
				if Type.is_null t then
					ctx.t.box_int32
				else
					i32_type ctx.llvm
			| [],"Float" ->
				if Type.is_null t then
					ctx.t.null_float
				else
					double_type ctx.llvm
			| [],"Bool" ->
				if Type.is_null t then
					ctx.t.null_bool
				else
					i1_type ctx.llvm
			| [],"Void" ->
				void_type ctx.llvm
			| ["c"],("ConstPointer" | "Pointer") ->
				let t0 = List.hd tl in
				if ExtType.is_void_type t0 then
					default_pointer ctx
				else
					pointer_type (convert_type ctx t0)
			| ["c"],"FunctionPointer" ->
				begin match follow (List.hd tl) with
				| TFun(tl,tr) -> pointer_type (plain_function_type ctx tl tr)
				| _ -> pointer_type (function_type (default_pointer ctx) [||])
				end
			| ["c"],"VarArg" ->
				ctx.t.var_args
			| [],"hx_char" ->
				i8_type ctx.llvm
			| [],"size_t" ->
				i32_type ctx.llvm
			| _ -> default_pointer ctx
		end
	| TAnon an ->
		let cfl = get_anon_fields an in
		let tl = List.map (fun cf -> convert_type ctx cf.cf_type) cfl in
		pointer_type (struct_type ctx.llvm (Array.of_list tl))
	| TFun(tl,tr) ->
		let tl = tl @ ["env",false,t_dynamic] in
		let ll_tf = pointer_type (plain_function_type ctx tl tr) in
		pointer_type (struct_type ctx.llvm [|ll_tf;default_pointer ctx|])
	| _ ->
		default_pointer ctx

and plain_function_type ctx tl tr =
	let f,tl = match List.rev tl with
		| (_,_,t) :: tl when ExtType.is_var_args_type t -> var_arg_function_type,List.rev tl
		| _ -> function_type,tl
	in
	f (convert_type ctx tr) (Array.of_list (List.map (fun (_,_,t) -> convert_type ctx t) tl))

(* Helper *)

let i32_const ctx i =
	const_int (i32_type ctx.llvm) i

let make_bool ctx b =
	const_int (i1_type ctx.llvm) (if b then 1 else 0)

let resolve_module ctx path p =
	try PMap.find path ctx.modules
	with Not_found -> error (Printf.sprintf "Could not resolve module %s" (s_type_path path)) p

let resolve_static_field ctx path name p =
	let c = (resolve_module ctx path p).m_class in
	c,PMap.find name c.cl_statics

let forward_declare_field ctx c cf =
	let name = full_field_name c cf in
	match cf.cf_kind,follow cf.cf_type with
		| Method _,TFun(tl,tr) ->
			let t = plain_function_type ctx tl tr in
			declare_function name t ctx.curmod
		| _ ->
			declare_global (convert_type ctx cf.cf_type) name ctx.curmod

let generate_phi ctx vl t =
	if ExtType.is_void_type t then
		null ctx
	else
		build_phi vl "phi" ctx.builder

let unless_terminated ctx f =
	match block_terminator (insertion_block ctx.builder) with
		| None -> f()
		| _ -> null ctx

let maybe_branch_to ctx bb_from bb_to =
	ignore(unless_terminated ctx (fun () -> build_br bb_to ctx.builder))

let rec get_param_types llt p = match classify_type llt with
	| TypeKind.Pointer -> get_param_types (element_type llt) p
	| TypeKind.Struct -> get_param_types (struct_element_types llt).(0) p (* assume closure *)
	| TypeKind.Function -> param_types llt
	| _ -> error (Printf.sprintf "Cannot get the parameter types of %s" (string_of_lltype llt)) p

let branch ctx fcond fthen felse b p =
	let patend b = ignore (position_at_end b ctx.builder) in
	let ll_cond = fcond() in
	let bb_cur = insertion_block ctx.builder in
	let ll_f = block_parent bb_cur in
	let bb_then = append_block ctx.llvm "then" ll_f in
	patend bb_then;
	let ll_then = fthen() in
	let bb_then_new = insertion_block ctx.builder in
	begin match felse with
		| None ->
			let bb_merge = append_block ctx.llvm "merge" ll_f in
			patend bb_cur;
			ignore(build_cond_br ll_cond bb_then bb_merge ctx.builder);
			patend bb_then_new;
			maybe_branch_to ctx bb_then_new bb_merge;
			patend bb_merge;
			null ctx
		| Some felse ->
			let bb_else = append_block ctx.llvm "else" ll_f in
			patend bb_else;
			let ll_else = felse() in
			let bb_else_new = insertion_block ctx.builder in
			let bb_merge = append_block ctx.llvm "merge" ll_f in
			patend bb_merge;
			let ll_phi = if b then build_phi [(ll_then,bb_then_new);(ll_else,bb_else_new)] "phi" ctx.builder else null ctx in
			patend bb_cur;
			ignore(build_cond_br ll_cond bb_then bb_else ctx.builder);
			patend bb_then_new;
			maybe_branch_to ctx bb_then_new bb_merge;
			patend bb_else_new;
			maybe_branch_to ctx bb_else_new bb_merge;
			patend bb_merge;
			ll_phi
	end

(* Type op *)

module TypeOp = struct
	open TypeKind

	let ptoi ctx v =
		build_ptrtoint v (i32_type ctx.llvm) "ptoi" ctx.builder

	let pointer_struct_name t = match classify_type t with
		| Pointer ->
			let t = element_type t in
			begin match classify_type t with
				| Struct -> struct_name t
				| _ -> None
			end
		| _ ->
			None

	let default_value ctx llt p =
		let no_default () = error (Printf.sprintf "I do not know a default value for %s" (string_of_lltype llt)) p in
		match classify_type llt with
		| Double -> const_float (double_type ctx.llvm) 0.0
		| Integer -> const_int (integer_type ctx.llvm (integer_bitwidth llt)) 0
		| Pointer ->
			begin match pointer_struct_name llt with
			| None -> no_default();
			| Some s -> match s with
				| "Null<Int8>" -> const_int (i8_type ctx.llvm) 0
				| "Null<Int16>" -> const_int (i16_type ctx.llvm) 0
				| "Null<Int32>" -> const_int (i32_type ctx.llvm) 0
				| "Null<Int64>" -> const_int (i64_type ctx.llvm) 0
				| "Null<Bool>" -> const_int (i1_type ctx.llvm) 0
				| "Null<Float>" -> const_float (double_type ctx.llvm) 0.0
				(* | "TypeParameter" -> const_null llt *)
				| _ -> no_default();
			end
		| _ ->
			no_default()

	let box ctx t v =
		let t0 = element_type t in
		let ll_malloc = build_malloc t0 "box" ctx.builder in
		let ll_vi = build_struct_gep ll_malloc 0 "box.gep" ctx.builder in
		ignore(build_store v ll_vi ctx.builder);
		build_bitcast ll_malloc t "box.cast" ctx.builder

	let unbox ctx v p =
		let v = build_load v "load" ctx.builder in
		let ll_v = build_extractvalue v 0 "unbox" ctx.builder in
		ll_v

	let unbox_or_default ctx v t p =
		let fcond() = build_icmp Icmp.Eq (ptoi ctx v) (const_int (i32_type ctx.llvm) 0) "compare.cond" ctx.builder in
		let fthen() = default_value ctx t p in
		let felse() = unbox ctx v p in
		branch ctx fcond fthen (Some felse) true p

	let is_type_parameter_pointer t =
		match pointer_struct_name t with
			| Some "TypeParameter" -> true
			| _ -> false

	let is_var_args_pointer t =
		match pointer_struct_name t with
			| Some "VarArg" -> true
			| _ -> false

	let is_pointer_to_haxe_null t =
		match pointer_struct_name t with
			| Some ("Null<Int8>" | "Null<Int16>" | "Null<Int32>" | "Null<Int64>" | "Null<Bool>" | "Null<Float>") -> true
			| _ -> false

	let itof ctx v t =
		build_sitofp v t "itof" ctx.builder

	let rec build_binop ctx op v1 v2 p =
		let t1 = type_of v1 in
		let t2 = type_of v2 in

		let f = match op,classify_type t1,classify_type t2 with
 			| OpAdd,Pointer,Pointer -> build_inttoptr (build_add (ptoi ctx v1) (ptoi ctx v2) "inttoptr" ctx.builder) t1
			| OpSub,Pointer,Pointer -> build_inttoptr (build_sub (ptoi ctx v1) (ptoi ctx v2) "inttoptr" ctx.builder) t1
			| OpAdd,Pointer,Integer -> build_inttoptr (build_add (ptoi ctx v1) v2 "add" ctx.builder) t1
			| OpSub,Pointer,Integer -> build_inttoptr (build_sub (ptoi ctx v1) v2 "sub" ctx.builder) t1

			| OpAdd,Double,Double -> build_fadd v1 v2
			| OpAdd,Integer,Double -> build_fadd (itof ctx v1 t2) v2
			| OpAdd,Double,Integer -> build_fadd v1 (itof ctx v2 t1)
			| OpAdd,Integer,Integer -> build_add v1 v2
			| OpAdd,Integer,Pointer -> build_add v1 (ptoi ctx v2)

			| OpSub,Double,Double -> build_fsub v1 v2
			| OpSub,Integer,Double -> build_fsub (itof ctx v1 t2) v2
			| OpSub,Double,Integer -> build_fsub v1 (itof ctx v2 t1)
			| OpSub,Integer,Integer -> build_sub v1 v2

			| OpMult,Double,Double -> build_fmul v1 v2
			| OpMult,Integer,Double -> build_fmul (itof ctx v1 t2) v2
			| OpMult,Double,Integer -> build_fmul v1 (itof ctx v2 t1)
			| OpMult,Integer,Integer -> build_mul v1 v2

			| OpMod,Double,Double -> build_frem v1 v2
			| OpMod,Integer,Double -> build_frem (itof ctx v1 t2) v2
			| OpMod,Double,Integer -> build_frem v1 (itof ctx v2 t1)
			| OpMod,Integer,Integer -> build_srem v1 v2

			| OpDiv,Double,Double -> build_fdiv v1 v2
			| OpDiv,Integer,Double -> build_fdiv (itof ctx v1 t2) v2
			| OpDiv,Double,Integer -> build_fdiv v1 (itof ctx v2 t1)
			| OpDiv,Integer,Integer -> build_fdiv (itof ctx v1 (double_type ctx.llvm)) (itof ctx v2 (double_type ctx.llvm))

			| OpShl,Integer,Integer -> build_shl v1 v2
			| OpShr,Integer,Integer -> build_lshr v1 v2
			| OpUShr,Integer,Integer -> build_ashr v1 v2
			| OpOr,Integer,Integer -> build_or v1 v2
			| OpAnd,Integer,Integer -> build_and v1 v2
			| OpXor,Integer,Integer -> build_xor v1 v2

			| OpLt,Double,Double -> build_fcmp Fcmp.Ult v1 v2
			| OpLt,Integer,Double -> build_fcmp Fcmp.Ult (itof ctx v1 t2) v2
			| OpLt,Double,Integer -> build_fcmp Fcmp.Ult v1 (itof ctx v2 t1)
			| OpLt,Integer,Integer -> build_icmp Icmp.Slt v1 v2

			| OpGt,Double,Double -> build_fcmp Fcmp.Ugt v1 v2
			| OpGt,Integer,Double -> build_fcmp Fcmp.Ugt (itof ctx v1 t2) v2
			| OpGt,Double,Integer -> build_fcmp Fcmp.Ugt v1 (itof ctx v2 t1)
			| OpGt,Integer,Integer -> build_icmp Icmp.Sgt v1 v2

			| OpLte,Double,Double -> build_fcmp Fcmp.Ule v1 v2
			| OpLte,Integer,Double -> build_fcmp Fcmp.Ule (itof ctx v1 t2) v2
			| OpLte,Double,Integer -> build_fcmp Fcmp.Ule v1 (itof ctx v2 t1)
			| OpLte,Integer,Integer -> build_icmp Icmp.Sle v1 v2

			| OpGte,Double,Double -> build_fcmp Fcmp.Uge v1 v2
			| OpGte,Integer,Double -> build_fcmp Fcmp.Uge (itof ctx v1 t2) v2
			| OpGte,Double,Integer -> build_fcmp Fcmp.Uge v1 (itof ctx v2 t1)
			| OpGte,Integer,Integer -> build_icmp Icmp.Sge v1 v2

			| OpEq,Double,Double -> build_fcmp Fcmp.Ueq v1 v2
			| OpEq,Integer,Double -> build_fcmp Fcmp.Ueq (itof ctx v1 t2) v2
			| OpEq,Double,Integer -> build_fcmp Fcmp.Ueq v1 (itof ctx v2 t1)
			| OpEq,Integer,Integer -> build_icmp Icmp.Eq v1 v2
			| OpEq,Pointer,Pointer -> build_icmp Icmp.Eq (ptoi ctx v1) (ptoi ctx v2)

			| OpNotEq,Double,Double -> build_fcmp Fcmp.Une v1 v2
			| OpNotEq,Integer,Double -> build_fcmp Fcmp.Une (itof ctx v1 t2) v2
			| OpNotEq,Double,Integer -> build_fcmp Fcmp.Une v1 (itof ctx v2 t1)
			| OpNotEq,Integer,Integer -> build_icmp Icmp.Ne v1 v2
			| OpNotEq,Pointer,Pointer -> build_icmp Icmp.Ne (ptoi ctx v1) (ptoi ctx v2)

			| OpEq,Pointer,(Integer | Double) -> (fun a b -> safe_compare ctx v1 v2 true false p)
			| OpEq,(Integer | Double),Pointer -> (fun a b -> safe_compare ctx v1 v2 false true p)
			| _,Pointer,(Integer | Double) -> (fun a b -> build_binop ctx op (unbox ctx v1 p) v2 p)
			| _,(Integer | Double),Pointer -> (fun a b -> build_binop ctx op v1 (unbox ctx v2 p) p)

			| _ ->
				error (Printf.sprintf "Unsupported binop: %s %s %s" (ll_value_info v1) (s_binop op) (ll_value_info v2)) p;
		in
		debug false (Printf.sprintf "binop %s %s %s" (s_binop op) (ll_value_info v1) (ll_value_info v2)) (fun () -> f (s_binop op) ctx.builder) ll_value_info

	and safe_compare ctx v1 v2 v1_is_ptr v2_is_ptr p =
		match v1_is_ptr,v2_is_ptr with
		| true,false ->
			let fcond() = build_icmp Icmp.Eq (ptoi ctx v1) (const_int (i32_type ctx.llvm) 0) "compare.cond" ctx.builder in
			let fthen() = const_int (i1_type ctx.llvm) 0 in
			let felse() = build_binop ctx OpEq (unbox ctx v1 p) v2 p in
			branch ctx fcond fthen (Some felse) true p
		| false,true ->
			let fcond() = build_icmp Icmp.Eq (ptoi ctx v2) (const_int (i32_type ctx.llvm) 0) "compare.cond" ctx.builder in
			let fthen() = const_int (i1_type ctx.llvm) 0 in
			let felse() = build_binop ctx OpEq v1 (unbox ctx v2 p) p in
			branch ctx fcond fthen (Some felse) true p
		| _ ->
			assert false

	let box_type ctx llt p = match classify_type llt with
		| Integer ->
			begin match integer_bitwidth llt with
				| 1 -> ctx.t.null_bool
				| 8 -> ctx.t.box_int8
				| 16 -> ctx.t.box_int16
				| 32 -> ctx.t.box_int32
				| 64 -> ctx.t.box_int64
				| i -> error (Printf.sprintf "I don't know an appropriate boxing type for int%i" i) p
			end
		| Double ->
			ctx.t.null_float
		| _ ->
			error (Printf.sprintf "I don't know an appropriate boxing type for %s" (string_of_lltype llt)) p

	let rec pointer_cast ctx llv ll_t p =
		let llt_1 = element_type (type_of llv) in
		let llt_2 = element_type ll_t in
		match classify_type llt_1,classify_type llt_2 with
		| _ when is_pointer_to_haxe_null (type_of llv) && is_pointer_to_haxe_null ll_t ->
			let t1 = (struct_element_types llt_1).(0) in
			let t2 = (struct_element_types llt_2).(0) in
			begin match classify_type t1,classify_type t2 with
			| Integer,Double ->
				let bt = (box_type ctx t2 p) in
				let fcond() = build_icmp Icmp.Eq (ptoi ctx llv) (const_int (i32_type ctx.llvm) 0) "compare.cond" ctx.builder in
				let fthen() = const_null bt in
				let felse() =
					let llv = unbox_or_default ctx llv (type_of llv) p in
					let llv = cast ctx false llv t2 p in
					box ctx bt llv
				in
				branch ctx fcond fthen (Some felse) true p
			| Integer,Integer ->
				llv
			| Double,Double ->
				llv
			| Double,Integer ->
				error "Invalid cast from Float to Int" p
			| _  ->
				error "Invalid cast" p
			end
		| _ ->
			build_pointercast llv ll_t "pointer" ctx.builder

	and cast ctx explicit ll_v ll_t p =
		let t = type_of ll_v in
		debug (false) (Printf.sprintf "CAST %s\nTO %s" (ll_value_info ll_v) (string_of_lltype ll_t)) (fun () ->
			let ll_v = match classify_type t,classify_type ll_t with
				| Integer,Integer ->
					let i1 = integer_bitwidth t in
					let i2 = integer_bitwidth ll_t in
					if i1 = i2 then
						build_bitcast ll_v ll_t "bit" ctx.builder
					else if i1 < i2 then
						build_zext ll_v ll_t "zext" ctx.builder
					else
						build_trunc ll_v ll_t "trunc" ctx.builder
				| Integer,Double ->
					itof ctx ll_v ll_t
				| Double,Integer when explicit ->
					build_fptosi ll_v ll_t "fptosi" ctx.builder
				| (Integer | Double),Pointer when is_type_parameter_pointer ll_t ->
					let box = box ctx (box_type ctx t p) ll_v in
					build_bitcast box ll_t "type param" ctx.builder
				| Pointer,(Integer | Double) when is_type_parameter_pointer t ->
					unbox_or_default ctx ll_v ll_t p
				| (Integer | Double),Pointer when is_pointer_to_haxe_null ll_t ->
					let ll_v = cast ctx explicit ll_v (struct_element_types (element_type ll_t)).(0) p in
					box ctx (box_type ctx (type_of ll_v) p) ll_v
				| Pointer,Pointer when (is_pointer_to_haxe_null t) && (is_var_args_pointer ll_t) ->
					unbox_or_default ctx ll_v t p
				| (Integer | Double),Pointer when is_var_args_pointer ll_t ->
					ll_v
				| Integer,Pointer when explicit ->
					build_inttoptr ll_v ll_t "inttoptr" ctx.builder
				| Pointer,(Integer | Double) when is_pointer_to_haxe_null t ->
					unbox_or_default ctx ll_v (type_of ll_v) p
				| Pointer,Integer ->
					build_ptrtoint ll_v ll_t "ptrtoint" ctx.builder
				| Pointer,Pointer ->
					pointer_cast ctx ll_v ll_t p
				| Void,Void
				| Double,Double ->
					build_bitcast ll_v ll_t "void" ctx.builder
				| _ ->
					error (Printf.sprintf "I don't know how to cast %s to %s" (ll_value_info ll_v) (string_of_lltype ll_t)) p;
			in
			ll_v
		) ll_value_info

end

(* Assertions *)

let assert_is_integer ll p = match classify_type (type_of ll) with
	| TypeKind.Integer _ -> ()
	| _ -> error "Integer expected" p

let assert_is_function_pointer v p =
	let t = pointer_type (type_of v) in
	match classify_type t with
	| TypeKind.Function -> ()
	| _ -> error (Printf.sprintf "Expected function type, found %s" (string_of_lltype t)) p

(* Store & Load *)

let field_access ctx c cf p =
	forward_declare_field ctx c cf

let get_field_index ctx t cf p =
	match follow t with
		| TAnon an ->
			let cfl = get_anon_fields an in
			let rec loop i cfl = match cfl with
				| cf' :: cfl when cf.cf_name = cf'.cf_name -> i
				| _ :: cfl -> loop (i + 1) cfl
				| [] -> error ("Unknown anon field " ^ cf.cf_name) p
			in
			loop 0 cfl
		| TInst(c,_) ->
			(* TODO: use assq? *)
			snd (List.find (fun (cf',_) -> cf.cf_name = cf'.cf_name) (resolve_module ctx c.cl_path p).m_field_lut)
		| _ ->
			if Type.is_null t then
				0
			else
				error ("Invalid structure type " ^ (s_type (print_context()) t)) p

let structure_access ctx ll_v1 cf t p =
	let i = try
		get_field_index ctx t cf p
	with Not_found ->
		error (Printf.sprintf "Could not resolve %s on %s" cf.cf_name (s_type (print_context()) t)) p
	in
	let ll_gep = build_struct_gep ll_v1 i ("obj." ^ cf.cf_name) ctx.builder in
	ll_gep

let array_access ctx ll_v1 ll_v2 p =
	assert_is_integer ll_v2 p;
	build_in_bounds_gep ll_v1 [|ll_v2|] "tarray" ctx.builder

let get_var ctx v p =
	try PMap.find v.v_id ctx.vars
	with Not_found -> error (Printf.sprintf "Unbound variable: %s" v.v_name) p

let set_var ctx v ll_v =
	ctx.vars <- PMap.add v.v_id ll_v ctx.vars

let declare_var ctx v =
	let t = convert_type ctx v.v_type in
	let ll_v = build_alloca t v.v_name ctx.builder in
	set_var ctx v ll_v;
	ll_v

let store ctx ll_value ll_store p =
	let ll_value = TypeOp.cast ctx false ll_value (element_type (type_of ll_store)) p in
	debug (false) (Printf.sprintf "STORE %s\nAT %s" (ll_value_info ll_value) (ll_value_info ll_store)) (fun () -> build_store ll_value ll_store ctx.builder) ll_value_info

let load ctx ll_store s =
	build_load ll_store s ctx.builder

module Closure = struct
	let wrap_closure ctx ll_f ll_env p =
		let ll_env = build_bitcast ll_env (default_pointer ctx) "closure.env.cast" ctx.builder in
		let ll_t = struct_type ctx.llvm [|type_of ll_f;type_of ll_env|] in
		let ll_malloc = build_malloc ll_t "closure" ctx.builder in
		let ll_vi = build_gep ll_malloc [|i32_const ctx 0; i32_const ctx 0|] "closure.f" ctx.builder in
		ignore(store ctx ll_f ll_vi p);
		let ll_vi = build_gep ll_malloc [|i32_const ctx 0; i32_const ctx 1|] "closure.env" ctx.builder in
		ignore(store ctx ll_env ll_vi p);
		build_bitcast ll_malloc (pointer_type ll_t) "closure.cast" ctx.builder

	let lift_closure ctx ll_f ll_env p =
		let ll_t = element_type (type_of ll_f) in
		let params = param_types ll_t in
		let new_params = Array.of_list ((Array.to_list params) @ [type_of ll_env]) in
		let ll_t = pointer_type (function_type (return_type ll_t) new_params) in
		let ll_f = build_bitcast ll_f ll_t "closure.f.cast" ctx.builder in
		wrap_closure ctx ll_f ll_env p
end

(* Expression *)

let annotate ctx ll_v p =
	let md = mdnode ctx.llvm [| mdstring ctx.llvm p.pfile |] in
	let kind = mdkind_id ctx.llvm (Printf.sprintf "line%i" (Lexer.get_error_line p)) in
	set_metadata ll_v kind md;
	ll_v

let emit_native_string ctx s =
	let name = (s_type_path ctx.curmod_extra.m_path) ^ s in
	let v = match lookup_global name ctx.curmod with
		| None ->
			let v = define_global name (const_stringz ctx.llvm s) ctx.curmod in
			set_global_constant true v;
			v
		| Some v ->
			v
	in
	(* build_pointercast v (string_type ctx) "string" ctx.builder *)
	const_pointercast v (pointer_type (i8_type ctx.llvm))

let emit_const ctx t = function
	| TInt i32 -> const_int (i32_type ctx.llvm) (Int32.to_int i32)
	| TFloat f -> const_float (double_type ctx.llvm) (float_of_string f)
	| TString s -> emit_native_string ctx s
	| TBool true -> const_int (i1_type ctx.llvm) 1
	| TBool false -> const_int (i1_type ctx.llvm) 0
	| TNull -> const_null (convert_type ctx t)
	| TThis -> ctx.vthis
	| TSuper -> ctx.vthis

let rec write ctx e =
	(* print_endline ("write " ^ s_expr_ast true "" (s_type (print_context())) e); *)
	match e.eexpr with
	| TLocal v ->
		get_var ctx v e.epos
	| TArray(e1,e2) ->
		array_access ctx (emit ctx e1) (emit ctx e2) e.epos
	| TField(_,FStatic(c,cf)) ->
		debug false (Printf.sprintf "field_access static %s.%s" (s_type_path c.cl_path) cf.cf_name) (fun() -> field_access ctx c cf e.epos) ll_value_info
	| TField(e1,(FAnon cf | FInstance(_,_,cf))) ->
		structure_access ctx (emit ctx e1) cf e1.etype e.epos
	| _ ->
		error (Printf.sprintf "I don't know how to write to %s" (s_expr_pretty "" (s_type (print_context())) e)) e.epos

and emit_unop ctx op flag e1 p = match op with
	| Not ->
		build_not (emit ctx e1) "not" ctx.builder
	| Neg ->
		let v1 = emit ctx e1 in
		let f = match classify_type (type_of v1) with
			| TypeKind.Double -> build_fneg (* TODO: single? *)
			| _ -> build_neg
		in
		f v1 "neg" ctx.builder
	| NegBits ->
		build_not (emit ctx e1) "not" ctx.builder
	| Decrement | Increment ->
		let ll_store = write ctx e1 in
		let ll_v1 = load ctx ll_store (if op = Decrement then "dec" else "inc") in
		let ll_value = TypeOp.build_binop ctx (if op = Decrement then OpSub else OpAdd) ll_v1 (i32_const ctx 1) p in
		ignore(store ctx ll_value ll_store p);
		if flag = Prefix then
			ll_value
		else
			ll_v1

and emit_binop ctx op e1 e2 p =
	match op with
		| OpEq | OpNotEq when ExtType.is_runtime_string_type e1.etype ->
			let e_call = ExprBuilder.make_library_call ctx.com ([],"String") "compare" [e1;e2] p in
			let e_zero = ExprBuilder.make_int ctx.com 0 e_call.epos in
			let e_op = mk (TBinop(op,e_call,e_zero)) ctx.com.basic.tbool e1.epos in
			emit ctx e_op
		| OpAdd when ExtType.is_runtime_string_type e1.etype ->
			let e_call = ExprBuilder.make_library_call ctx.com ([],"String") "concat" [e1;e2] p in
			emit ctx e_call
		| OpAssignOp OpAdd when ExtType.is_runtime_string_type e1.etype ->
			let llv_1 = write ctx e1 in
			let llv_2 = emit ctx e2 in
			let llv_call = make_library_call_native ctx true e1 ([],"String") "concat" None [(load ctx llv_1 "assignOp");llv_2] p in
			ignore(store ctx llv_call llv_1 p);
			llv_1
		| OpAdd | OpSub | OpMult | OpMod | OpDiv | OpShl | OpShr | OpUShr | OpOr | OpAnd | OpXor | OpLte | OpLt | OpGt | OpGte | OpEq | OpNotEq ->
			TypeOp.build_binop ctx op (emit ctx e1) (emit ctx e2) p
		| OpAssign ->
			let ll_v2 = emit ctx e2 in
			let ll_v1 = write ctx e1 in
			ignore(store ctx ll_v2 ll_v1 p);
			ll_v2
		| OpAssignOp op ->
			let ll_v1 = write ctx e1 in
			let ll_v2 = emit ctx e2 in
			let ll_v = TypeOp.build_binop ctx op (load ctx ll_v1 "assignOp") ll_v2 p in
			ignore(store ctx ll_v ll_v1 p);
			ll_v
		| OpBoolAnd ->
			let fcond() = emit ctx e1 in
			let fthen() = emit ctx e2 in
			let felse() = make_bool ctx false in
			branch ctx fcond fthen (Some felse) true p
		| OpBoolOr ->
			let fcond() = emit ctx e1 in
			let fthen() = make_bool ctx true in
			let felse() = emit ctx e2 in
			branch ctx fcond fthen (Some felse) true p
		| OpArrow | OpInterval ->
			assert false

and call_or_invoke ctx ll_f ll_ea s p =
	let array_string = String.concat ", " (List.map (fun v -> string_of_lltype (type_of v)) (Array.to_list ll_ea)) in
	debug (false) (Printf.sprintf "CALL_OR_INVOKE %s\nARGS: %s\n" (ll_value_info ll_f) array_string) (fun () ->
		match ctx.landing_pad with
		| None ->
			build_call ll_f ll_ea s ctx.builder
		| Some bb ->
			let bb_cur = insertion_block ctx.builder in
			let f = block_parent bb_cur in
			let bb_cont = append_block ctx.llvm "cont" f in
			let ll_v = build_invoke ll_f ll_ea bb_cont bb s ctx.builder in
			position_at_end bb_cont ctx.builder;
			ll_v
	) ll_value_info

(* llvm func, llvm args *)
and emit_call3 ctx e ll_v1 ll_el =
	let ll_v1_t = element_type (type_of ll_v1) in
	let ll_v = match classify_type ll_v1_t with
		| TypeKind.Function ->
			let ll_ea = Array.of_list ll_el in
			call_or_invoke ctx ll_v1 ll_ea (if ExtType.is_void_type e.etype then "" else "call") e.epos
		| TypeKind.Struct ->
			let ll_f = load ctx (build_struct_gep ll_v1 0 "f" ctx.builder) "closure.f" in
			let ll_env = load ctx (build_struct_gep ll_v1 1 "env" ctx.builder) "closure.env" in
			let ll_ea = Array.of_list (ll_el @ [ll_env]) in
			call_or_invoke ctx ll_f ll_ea (if ExtType.is_void_type e.etype then "" else "call") e.epos
		| _ ->
			error (Printf.sprintf "I don't know how to call %s" (ll_value_info ll_v1)) e.epos
	in
	ll_v

(* llvm func, haxe args *)
and emit_call2 ctx e ll_v1 el =
	let emit_args el =
		let params = get_param_types (type_of ll_v1) e.epos in
		let ll_el = ExtList.List.mapi (fun i e ->
			if i >= Array.length params then error (Printf.sprintf "Not enough arguments %s" (s_expr_ast true "" (s_type (print_context())) e)) e.epos;
			let t = params.(i) in
			let llv = emit ctx e in
			let v = TypeOp.cast ctx false llv t e.epos in
			v
		) el in
		ll_el
	in
	let ll_el = match List.rev el with
		| {eexpr = TArrayDecl el1} as e :: el2 when ExtType.is_var_args_type e.etype ->
			let el2 = emit_args (List.rev el2) in
			let el1 = List.map (emit ctx) el1 in
			el2 @ el1
		| _ ->
			emit_args el
	in
	emit_call3 ctx e ll_v1 ll_el

(* haxe func, haxe args *)
and emit_call ctx e e1 el = match e1.eexpr with
	| TField(_,FStatic({cl_path = ["c"],"Lib"},cf)) ->
		begin match cf.cf_name with
		| "sizeof" ->
			let t = match follow (List.hd el).etype with
				| TInst({cl_path = ["c"],"TypeReference"},[t]) -> follow t
				| t -> t
			in
			(* TODO: 32/64 bit issue? *)
			const_trunc (size_of (convert_type ctx t)) (i32_type ctx.llvm)
		| "callMain" ->
			begin match ctx.com.main with
			| Some e -> emit ctx e
			| None -> null ctx
			end
		| "initializeStatics" ->
			List.iter (fun (c,cf) ->
				let ethis = ExprBuilder.make_static_this c e.epos in
				let ef = mk (TField(ethis, FStatic(c,cf))) cf.cf_type e.epos in
				let ec = mk (TCall(ef,[])) ctx.com.basic.tvoid e.epos in
				ignore(emit ctx ec)
			) ctx.static_inits;
			null ctx
		| "dereference" ->
			let e1 = List.hd el in
			let ll_v = emit ctx e1 in
			(* load ctx ll_v "load" *)
			ll_v
		| s ->
			error ("Unsupported lib call to " ^ s) e.epos
		end
	| TField(ef,FInstance(c,[t1],cf)) when Specializer.has_spec_meta cf.cf_meta ->
		let cf' = Specializer.find_specialized_field c cf false (get_type_postfix t1) in
		emit_call ctx e {e1 with eexpr = TField({ef with etype = cf'.cf_type},FInstance(c,[t1],cf'))} el
	| TField(ef,FStatic(c,cf)) when Specializer.has_spec_meta cf.cf_meta ->
		let cf' = Specializer.find_specialized_field c cf true (get_type_postfix e1.etype) in
		emit_call ctx e {e1 with eexpr = TField({ef with etype = cf'.cf_type},FStatic(c,cf'))} el
	| TField(e1,FInstance(c,tl,cf)) when (Meta.has (Meta.Custom ":overridden") cf.cf_meta) ->
		let cf_vTable = PMap.find cf_name_vtable c.cl_fields in
		let e_vTable = mk (TField(e1,FInstance(c,tl,cf_vTable))) cf_vTable.cf_type e.epos in
		let e_field = mk (TField(e_vTable,FAnon cf)) cf.cf_type e.epos in
		let e_cast = mk (TCast(e1,None)) (TInst(c,tl)) e.epos in
		emit_call ctx e e_field (el @ [e_cast])
	| TField(e1,FInstance(c,tl,({cf_kind = Method _} as cf))) ->
		let e_cast = mk (TCast(e1,None)) (TInst(c,tl)) e.epos in
		emit_call2 ctx e (field_access ctx c cf e.epos) (el @ [e_cast])
	| TConst TSuper ->
		let c,tl = match ctx.curmod_extra.m_class.cl_super with
			| Some (c,tl) ->
				c,tl
			| _ -> assert false
		in
		let cf = try
			PMap.find cf_name_init c.cl_fields
		with Not_found ->
			error "Super class has no initializer" e.epos
		in
		let ll_store = field_access ctx c cf e.epos in
		let e_this = mk (TConst TThis) e.etype e.epos in
		let e_cast = mk (TCast(e_this,None)) (TInst(c,tl)) e.epos in
		emit_call2 ctx e ll_store (el @ [e_cast])
	| TField(e1,(FStatic(c,cf))) ->
		let ll_store = field_access ctx c cf e.epos in
		let ll_f = match cf.cf_kind with
			| Method _ -> ll_store
			| Var _ -> load ctx ll_store ("obj." ^ cf.cf_name)
		in
		emit_call2 ctx e ll_f el
	| _ ->
		emit_call2 ctx e (emit ctx e1) el

and make_library_call_native ctx stat e path name tspec ll_el p =
	let c = (resolve_module ctx path p).m_class in
	let cf = PMap.find name (if stat then c.cl_statics else c.cl_fields) in
	let cf = match tspec with
		| None -> cf
		| Some t -> Specializer.find_specialized_field c cf stat (get_type_postfix t)
	in
	let ll_v = forward_declare_field ctx c cf in
	emit_call3 ctx e ll_v ll_el

and emit_function ctx tf =
	emit ctx tf.tf_expr

and emit ctx e =
	(* print_endline ("emit " ^ s_expr_ast true "" (s_type (print_context())) e); *)
	match e.eexpr with
	| TBlock [] ->
		null ctx
	| TBlock el ->
		let rec loop el = match el with
			| e :: [] ->
				emit ctx e
			| e :: el ->
				ignore(emit ctx e);
				loop el
			| [] ->
				assert false
		in
		loop el
	(* values *)
	| TVar(v,eo) ->
		let ll_v = declare_var ctx v in
		begin match eo with
			| None ->
				ll_v
			| Some e' ->
				let ll_vi = emit ctx e' in
				store ctx ll_vi ll_v e.epos
		end;
	| TLocal v ->
		let ll_v = get_var ctx v e.epos in
		load ctx ll_v v.v_name
	| TConst ct ->
		emit_const ctx e.etype ct
	| TTypeExpr mt ->
		let name = get_typeref_name ctx ((t_infos mt).mt_path) e.epos in
		let m = resolve_module ctx (["c"],"TypeReference") e.epos in
		let t = type_of_module_type mt in
		declare_global (convert_type ctx (TInst(m.m_class,[t]))) name ctx.curmod
	(* ops *)
	| TUnop((Increment | Decrement as op),flag,({eexpr = TArray(e1,e2)} as e_op)) ->
		begin match follow e1.etype with
			| TInst({cl_path=[],"Array"},[t1]) ->
				let ll_v1 = emit ctx e1 in
				let ll_v2 = emit ctx e2 in
				let ll_v3 = i32_const ctx 1 in
				let ll_get = make_library_call_native ctx false e ([],"Array") "__get" (Some t1) [ll_v2;ll_v1] e.epos in
				let ll_op = TypeOp.build_binop ctx (if op = Increment then OpAdd else OpSub) ll_get ll_v3 e.epos in
				let ll_res = make_library_call_native ctx false e ([],"Array") "__set" (Some t1) [ll_v2;ll_op;ll_v1] e.epos in
				if flag = Prefix then ll_res else ll_get
			| _ ->
				emit_unop ctx op flag e_op e.epos
		end
	| TUnop(op,flag,e1) ->
		emit_unop ctx op flag e1 e.epos
	| TBinop(OpAssign, ({eexpr = TArray(e1,e2)} as e_lhs), e_rhs) ->
		begin match follow e1.etype with
			| TInst({cl_path=[],"Array"} as c,[t1]) ->
				let cf = PMap.find "__set" c.cl_fields in
				let ef = mk (TField(e1,FInstance(c,[t1],cf))) cf.cf_type e.epos in
				let ec = mk (TCall(ef,[e2;e_rhs])) t1 e.epos in
				emit ctx ec
			| TInst({cl_path=["c"],"FixedArray"} as c,[t1]) ->
				let cf = PMap.find "__set" c.cl_fields in
				let ef = mk (TField(e1,FInstance(c,[t1],cf))) cf.cf_type e.epos in
				let ec = mk (TCall(ef,[e2;e_rhs])) t1 e.epos in
				TypeOp.cast ctx false (emit ctx ec) (convert_type ctx e_rhs.etype) e.epos
			| _ ->
				emit_binop ctx OpAssign e_lhs e_rhs e.epos
		end
	| TBinop(OpAssignOp op, ({eexpr = TArray(e1,e2)} as e_lhs), e_rhs) ->
		begin match follow e1.etype with
			| TInst({cl_path=[],"Array"},[t1]) ->
				let ll_v1 = emit ctx e1 in
				let ll_v2 = emit ctx e2 in
				let ll_v3 = emit ctx e_rhs in
				let ll_get = make_library_call_native ctx false e ([],"Array") "__get" (Some t1) [ll_v2;ll_v1] e.epos in
				let ll_op = TypeOp.build_binop ctx op ll_get ll_v3 e.epos in
				make_library_call_native ctx false e ([],"Array") "__set" (Some t1) [ll_v2;ll_op;ll_v1] e.epos
			| _ ->
				emit_binop ctx (OpAssignOp op) e_lhs e_rhs e.epos
		end
	| TBinop(op,e1,e2) ->
		emit_binop ctx op e1 e2 e.epos
	(* review *)
	| TObjectDecl [_,e1] when (match follow e.etype with TAnon an -> an.a_status == null_wrap_const | _ -> false) ->
		let ll_v = emit ctx e1 in
		TypeOp.box ctx (TypeOp.box_type ctx (type_of ll_v) e.epos) ll_v
	| TObjectDecl fl ->
		let t,cfl = match follow e.etype with
			| TAnon an ->
				let cfl = get_anon_fields an in
				let tl = List.map (fun cf -> convert_type ctx cf.cf_type) cfl in
				struct_type ctx.llvm (Array.of_list tl),cfl
			| _ ->
				error ("Dynamic TObjectDecl is not supported") e.epos
		in
		let ll_malloc = build_malloc t "obj" ctx.builder in
		ExtList.List.iteri (fun i cf ->
			let e = List.assoc cf.cf_name fl in
			let ll_v = emit ctx e in
			let ll_vi = build_gep ll_malloc [|i32_const ctx 0; i32_const ctx i|] ("obj." ^ cf.cf_name) ctx.builder in
			store ctx ll_v ll_vi e.epos
		) cfl;
		build_bitcast ll_malloc (convert_type ctx e.etype) "obj.cast" ctx.builder
	| TArrayDecl [] ->
		begin match follow e.etype with
		| TInst(({cl_path = [],"Array"} as c),[t1]) ->
			let e = mk (TNew(c,[t1],[])) (TInst(c,[t1])) e.epos in
			emit ctx e
		| _ ->
			assert false
		end
	| TArrayDecl (e1 :: el) ->
		let te = convert_type ctx e1.etype in
		let i = List.length el + 1 in
		let ta = array_type te i in
		let ll_malloc = build_malloc ta "array" ctx.builder in
		ExtList.List.iteri (fun i e ->
			let ll_v = emit ctx e in
			let ll_vi = build_gep ll_malloc [|i32_const ctx 0; i32_const ctx i|] ("array." ^ (string_of_int i)) ctx.builder in
			store ctx ll_v ll_vi e.epos
		) (e1 :: el);
		let ll_s = declare_function "Array_ofNative_dyn" (function_type (convert_type ctx e.etype) [|default_pointer ctx;i32_type ctx.llvm|]) ctx.curmod in
		emit_call3 ctx e ll_s [TypeOp.cast ctx false ll_malloc (default_pointer ctx) e.epos;i32_const ctx i]
	(* structural access *)
	| TArray(e1,e2) ->
		begin match follow e1.etype with
			| TInst({cl_path=[],"Array"},[t1]) ->
				let ll_v1 = emit ctx e1 in
				let ll_v2 = emit ctx e2 in
				let llv = make_library_call_native ctx false e ([],"Array") "__get" (Some t1) [ll_v2;ll_v1] e.epos in
				(* TODO: the explicit = true is not really correct, but we have to deal with the Int return *)
				TypeOp.cast ctx true llv (convert_type ctx e.etype) e.epos
			| TInst({cl_path=["c"],"FixedArray"} as c,[t1]) ->
				let cf = PMap.find "__get" c.cl_fields in
				let ef = mk (TField(e1,FInstance(c,[t1],cf))) cf.cf_type e.epos in
				let ec = mk (TCall(ef,[e2])) t1 e.epos in
				TypeOp.cast ctx false (emit ctx ec) (convert_type ctx e.etype) e.epos
			| _ ->
				let ll_v1 = emit ctx e1 in
				let ll_v2 = emit ctx e2 in
				load ctx (array_access ctx ll_v1 ll_v2 e.epos) "tarray"
		end
	| TField(e1,(FAnon cf)) ->
		let ll_gep = structure_access ctx (emit ctx e1) cf e1.etype e.epos in
		let ll_v = load ctx ll_gep ("obj." ^ cf.cf_name) in
		TypeOp.cast ctx false ll_v (convert_type ctx e.etype) e.epos
	| TField(e1,(FInstance(c,_,cf))) ->
		begin match cf.cf_kind with
			| Method _ ->
				field_access ctx c cf e.epos
			| Var _ ->
				let ll_gep = structure_access ctx (emit ctx e1) cf e1.etype e.epos in
				let ll_v = load ctx ll_gep ("obj." ^ cf.cf_name) in
				TypeOp.cast ctx false ll_v (convert_type ctx e.etype) e.epos
		end
	| TField(e1,(FStatic(c,cf))) ->
		let ll_store = field_access ctx c cf e.epos in
		begin match cf.cf_kind with
			| Method _ ->
				if c.cl_extern then (* TODO: use some hxGen distinction *)
					ll_store
				else
					Closure.lift_closure ctx ll_store (null ctx) e.epos
			| Var _ ->
				load ctx ll_store ("obj." ^ cf.cf_name)
		end
	| TField(e1,FClosure(Some(c,_),cf)) ->
		begin match cf.cf_kind with
			| Var _ ->
				(* This can happen if the field used to be Method MethDynamic
				   but was changed during preprocessing. Let's read it like a
				   variable. *)
				let ll_gep = structure_access ctx (emit ctx e1) cf e1.etype e.epos in
				load ctx ll_gep ("obj." ^ cf.cf_name)
			| Method _ ->
				let ll_store = field_access ctx c cf e.epos in
				let ll_v = emit ctx e1 in
				Closure.lift_closure ctx ll_store ll_v e.epos
		end
	| TField(_) ->
		const_null (convert_type ctx e.etype)
	(* control flow *)
	| TCall(e1,el) ->
		emit_call ctx e e1 el
	| TNew({cl_path=["c"],"TypeReference"} as c,[t],[]) ->
		let name = get_typeref_name ctx (get_type_path t e.epos) e.epos in
		let ll_g = declare_global (convert_type ctx (TInst(c,[t]))) name ctx.curmod in
		ll_g
	| TNew(c,tl,el) ->
		let cf = match c.cl_constructor with Some cf -> cf | None -> assert false in
		let ll_store = field_access ctx c cf e.epos in
		emit_call2 ctx e ll_store el
	| TReturn (Some e1) ->
		let ll_v = emit ctx e1 in
		let ll_v = TypeOp.cast ctx false ll_v (convert_type ctx e1.etype) e.epos in
		unless_terminated ctx (fun () -> build_ret ll_v ctx.builder)
	| TReturn None ->
		build_ret_void ctx.builder
	| TIf(e1,e2,eo) ->
		branch ctx (fun () -> emit ctx e1) (fun () -> emit ctx e2) (match eo with None -> None | Some e -> Some (fun() -> emit ctx e)) (not (ExtType.is_void_type e.etype)) e.epos
	| TSwitch(e1,el,eo) ->
		let v1 = emit ctx e1 in
		let i = (List.length el) in
		let bb_cur = insertion_block ctx.builder in
		let f = block_parent bb_cur in
		let bb_merge = append_block ctx.llvm "merge" f in
		let bb_default,v_default = match eo with
			| None ->
				bb_merge,None
			| Some e ->
				let bb = append_block ctx.llvm "default" f in
				ignore(position_at_end bb ctx.builder);
				let v = emit ctx e in
				maybe_branch_to ctx bb bb_merge;
				bb,Some v
		in
		ignore(position_at_end bb_cur ctx.builder);
		let switch = build_switch v1 bb_default i ctx.builder in
		let vl = List.map (fun (el,e) ->
			let bb = append_block ctx.llvm "case" f in
			ignore(position_at_end bb ctx.builder);
			let v = emit ctx e in
			maybe_branch_to ctx bb bb_merge;
			List.iter (fun e ->
				let v = emit ctx e in
				add_case switch v bb
			) el;
			(v,bb)
		) el in
		ignore(position_at_end bb_merge ctx.builder);
		generate_phi ctx (match v_default with None -> vl | Some v -> (v,bb_default) :: vl) e.etype
	| TWhile(e1,e2,flag) ->
		let bb_cur = insertion_block ctx.builder in
		let f = block_parent bb_cur in
		let bb_cond = append_block ctx.llvm "cond" f in
		let bb_body = append_block ctx.llvm "body" f in
		let bb_merge = append_block ctx.llvm "merge" f in
		let _ = build_br (if flag = NormalWhile then bb_cond else bb_body) ctx.builder in
		position_at_end bb_cond ctx.builder;
		let v1 = emit ctx e1 in
		let _ = build_cond_br v1 bb_body bb_merge ctx.builder in
		position_at_end bb_body ctx.builder;
		let close = begin_loop ctx bb_cond bb_merge in
		let _ = emit ctx e2 in
		let _ = build_br bb_cond ctx.builder in
		close();
		position_at_end bb_merge ctx.builder;
		null ctx
	| TBreak ->
		begin match ctx.merge_block with
			| None -> error "Break outside loop" e.epos
			| Some bb -> build_br bb ctx.builder
		end
	| TContinue ->
		begin match ctx.cond_block with
			| None -> error "Continue outside loop" e.epos
			| Some bb -> build_br bb ctx.builder
		end
	| TTry(e1,catches) ->
		(* TODO *)
		emit ctx e1
	| TThrow e1 ->
		build_unreachable ctx.builder
(* 	| TTry(e1,catches) ->
		let e_push = ExprBuilder.make_library_call ctx.com (["c"],"Exception") "push" [] e.epos in
		let e_push_deref = ExprBuilder.make_library_call ctx.com (["c"],"Lib") "dereference" [e_push] e.epos in
		let e_setjmp = ExprBuilder.make_library_call ctx.com (["c"],"CSetjmp") "setjmp" [e_push_deref] e.epos in
		let e_def = ref None in
		let el = ExtList.List.filter_map (fun (v,e) ->
			if v.v_type == t_dynamic then begin
				e_def := Some e;
				None
			end else
				error ("Only Dynamic catches are currently supported") e.epos
		) catches in
		let e = mk (TSwitch(e_setjmp,el,!e_def)) e.etype e.epos in
		emit ctx e
	| TThrow e1 ->
		let e_pop = ExprBuilder.make_library_call ctx.com (["c"],"Exception") "pop" [] e.epos in
		let e_longjmp = ExprBuilder.make_library_call ctx.com (["c"],"CSetjmp") "longjmp" [e_pop] e.epos in
		emit ctx e_longjmp *)
(* 	| TTry(e1,catches) ->
		(* TODO *)
		let old = ctx.landing_pad in
		let bb_cur = insertion_block ctx.builder in
		let f = block_parent bb_cur in
		let bb_lpad = append_block ctx.llvm "lpad" f in
		let bb_merge = append_block ctx.llvm "merge" f in
		ctx.landing_pad <- Some bb_lpad;
		let ll_v = emit ctx e1 in
		maybe_branch_to ctx bb_cur bb_merge;
		ctx.landing_pad <- old;
		position_at_end bb_lpad ctx.builder;
		let v,e2 = List.hd catches in
		let pers_arg = i32_type ctx.llvm in
		let c,cf_pers = resolve_static_field ctx ([],"hxc") "personality" e.epos in
		let ll_pers = forward_declare_field ctx c cf_pers in
		let ll_lp = build_landingpad (pers_arg) ll_pers 1 "lpad" ctx.builder in
		add_clause ll_lp (null ctx);
		declare_var ctx v;
		ignore(emit ctx e2);
		maybe_branch_to ctx bb_lpad bb_merge;
		position_at_end bb_merge ctx.builder;
		ll_v *)
	| TFunction tf ->
		let e_body,v_env,env_obj = ExtFunction.get_closure_environment tf in
		let tf = {tf with tf_expr = e_body; tf_args = tf.tf_args @ [v_env,None]} in
		let field_name = Printf.sprintf "closure_%i" ctx.curmod_extra.m_num_closures in
		ctx.curmod_extra.m_num_closures <- ctx.curmod_extra.m_num_closures + 1;
		let tl = List.map (fun (v,_) -> v.v_name,false,v.v_type) tf.tf_args in
		let cf = mk_field field_name (TFun(tl,tf.tf_type)) e.epos in
		cf.cf_kind <- Method MethNormal;
		cf.cf_expr <- Some (mk (TFunction tf) cf.cf_type e.epos);
		let name = full_field_name ctx.curmod_extra.m_class cf in
		ignore(declare_function name (plain_function_type ctx tl tf.tf_type) ctx.curmod);
		let ll_f = define_field ctx true ctx.curmod_extra.m_class cf in
		let rec map_f_type ll_v =
			let ll_t = element_type (type_of ll_v) in
			let params = param_types ll_t in
			params.(Array.length params - 1) <- default_pointer ctx;
			let ll_tf = pointer_type (function_type (return_type ll_t) params) in
			build_bitcast ll_v ll_tf "cast" ctx.builder
		in
		let ll_f = map_f_type ll_f in
		let ll_env = emit ctx env_obj in
		Closure.wrap_closure ctx ll_f ll_env e.epos;
	| TCast(e1,None) ->
		let ll_v = emit ctx e1 in
		TypeOp.cast ctx true ll_v (convert_type ctx e.etype) e.epos
	(* skip *)
	| TParenthesis e1 | TMeta(_,e1) ->
		emit ctx e1
	| _ ->
		error (Printf.sprintf "Unimplemented expression: %s" (s_expr_pretty "" (s_type (print_context())) e)) e.epos

(* Module type level *)

and define_field ctx stat c cf =
(* 	Printf.printf "define_field %s%s.%s" (if stat then "static " else "") (s_type_path c.cl_path) cf.cf_name;
	print_endline ""; *)
	let name = full_field_name c cf in
	let save = begin_field ctx in
	let set_init_or_define llv = match lookup_global name ctx.curmod with
		| Some g ->
			set_initializer llv g;
			g
		| None ->
			define_global name llv ctx.curmod
	in
	let ll_field = match cf.cf_kind,cf.cf_expr with
	| Method _,Some {eexpr = TFunction tf}->
		let ll_f = forward_declare_field ctx c cf in
		ctx.builder <- builder ctx.llvm;
		let ll_bb = append_block ctx.llvm "entry" ll_f in
		position_at_end ll_bb ctx.builder;
		let params = params ll_f in
		ExtList.List.iteri (fun i (v,_) ->
			if i >= Array.length params then error (Printf.sprintf "Not enough arguments %s %s %b" cf.cf_name (s_type (print_context()) cf.cf_type) stat) cf.cf_pos;
			let arg = params.(i) in
			set_value_name v.v_name arg;
			let value = declare_var ctx v in
			store ctx arg value cf.cf_pos;
		) tf.tf_args;
		if cf.cf_name = "new" then begin
			let vthis = build_malloc (named_struct_type ctx (get_native_class_name c)) "new" ctx.builder in
			ctx.vthis <- vthis;
		end else if not stat then
			ctx.vthis <- params.(Array.length params - 1);
		begin match tf.tf_expr.eexpr with
			| TBlock [] -> ()
			| _ -> ignore(emit_function ctx tf)
		end;
		if ExtType.is_void_type tf.tf_type && cf.cf_name <> "new" then (* Haxe constructors are typed as -> Void *)
			ignore(build_ret_void ctx.builder);
		if not (Llvm_analysis.verify_function ll_f) then
			prerr_endline (Printf.sprintf "Could not verify function %s.%s" (s_type_path c.cl_path) cf.cf_name);
		ll_f
	| Var _,Some ({eexpr = TConst _} as e) ->
		set_init_or_define (emit ctx e)
	| Var _,Some e when stat ->
		let ethis = ExprBuilder.make_static_this c e.epos in
		let ef = mk (TField(ethis, FStatic(c,cf))) cf.cf_type e.epos in
		let e_assign = mk (TBinop(OpAssign,ef,e)) e.etype e.epos in
		ExtClass.add_static_init c e_assign;
		set_init_or_define (const_null (convert_type ctx cf.cf_type))
	| Var _,None when stat ->
		set_init_or_define (const_null (convert_type ctx cf.cf_type))
	| _ ->
		const_null (convert_type ctx cf.cf_type)
	in
	save();
	ll_field

module Define = struct

	let define_class ctx c =
		let define_field stat cf =
			begin match cf.cf_expr with
				| Some e ->
					let e = StringHandler.run ctx.com e in
					let e = Codegen.UnificationCallback.run (TypeChecker.run ctx.com) e in
					cf.cf_expr <- Some e;
				| None ->
					()
			end;
			let save = begin_field ctx in
			ignore(define_field ctx stat c cf);
			save()
		in
		List.iter (define_field true) c.cl_ordered_statics;
		List.iter (define_field false) c.cl_ordered_fields;
		begin match c.cl_constructor with
			| None -> ()
			| Some cf -> define_field true cf
		end;
		begin match c.cl_init with
			| None -> ()
			| Some e ->
				let t = tfun [] ctx.com.basic.tvoid in
				let cf = mk_field cf_name_static_init t c.cl_pos in
				let tf = {
					tf_args = [];
					tf_type = ctx.com.basic.tvoid;
					tf_expr = mk_block e;
				} in
				cf.cf_kind <- Method MethNormal;
				cf.cf_expr <- Some (mk (TFunction tf) t c.cl_pos);
				ctx.static_inits <- (c,cf) :: ctx.static_inits;
				define_field true cf
		end

	let create_typeref ctx c =
		let ll_name = emit_native_string ctx (s_type_path c.cl_path) in
		let ll_default = const_null (convert_type ctx (TInst(c,List.map snd c.cl_params))) in
		let ll_ref_size = i32_const ctx 0 in (* TODO *)
		let ll_ctor = try forward_declare_field ctx c (match c.cl_constructor with Some cf -> cf | None -> raise Not_found) with Not_found -> null ctx in
		let ll_allocator = try forward_declare_field ctx c (PMap.find cf_name_init c.cl_fields) with Not_found -> null ctx in
		let ll_parent = match c.cl_super with
			| None -> null ctx
			| Some (c,tl) ->
				let m = resolve_module ctx (["c"],"TypeReference") c.cl_pos in
				let t = TInst(c,tl) in
				let name = get_typeref_name ctx (c.cl_path) c.cl_pos in
				declare_global (convert_type ctx (TInst(m.m_class,[t]))) name ctx.curmod
		in
		let ll_info = const_struct ctx.llvm [|ll_name;ll_default;ll_ref_size;ll_ctor;ll_allocator;ll_parent|] in
		let name = get_typeref_name ctx c.cl_path c.cl_pos in
		let _ = define_global name ll_info ctx.curmod in
		()

	let define_module ctx m_extra =
		let c = m_extra.m_class in
		let m = create_module ctx.llvm (snd c.cl_path) in
		ctx.curmod <- m;
		ctx.curmod_extra <- m_extra;
		create_typeref ctx c;
		define_class ctx c;
		close_module ctx
end

module Preprocess = struct
	let create_field_lut c =
		let rec loop c_cur =
			let fields1 = match c_cur.cl_super with
				| None -> []
				| Some (c,_) -> loop c
			in
			let fields2 = List.filter (fun cf -> match cf.cf_kind with
				| Var _ when not (is_extern_field cf) && (cf.cf_name <> cf_name_vtable) -> true
				| _ -> false
			) c_cur.cl_ordered_fields in
			fields1 @ fields2
		in
		let physical_fields = loop c in
		let physical_fields =
			if PMap.mem cf_name_vtable c.cl_fields
			then (PMap.find cf_name_vtable c.cl_fields) :: physical_fields
			else physical_fields
		in
		ExtList.List.mapi (fun i cf -> cf,i) physical_fields

	let prepare_module ctx m_extra =
		let c = m_extra.m_class in
		m_extra.m_field_lut <- create_field_lut c;
		(* MUST NOT ADD ANY MORE CLASS FIELDS AFTER THIS POINT *)
		let fill_struct () =
			let t = named_struct_type ctx (get_native_class_name c) in
			let tl = Array.of_list (List.map (fun (cf,_) -> convert_type ctx cf.cf_type) m_extra.m_field_lut) in
			struct_set_body t tl false
		in
		fill_struct();
		not c.cl_extern

	let prepare_module_type ctx = function
		| TClassDecl c ->
			if not c.cl_extern then begin
				Specializer.specialize_class c [ctx.com.basic.tint,"_32";t_dynamic,"_dyn";ctx.com.basic.tstring,"_string"];
				ClassPreprocessor.prepare_class ctx.com c;
			end;
			let m = create_module_extra ctx c in
			Some m
		| _ ->
			None
end

let generate_make_file ctx modules =
	let path_to_file_path (pack,name) = match pack with [] -> name | _ -> String.concat "/" pack ^ "/" ^ name in
	let relpath path = path_to_file_path path in
	let main_name = match ctx.com.main_class with Some path -> snd path | None -> "main" in
	let filepath = ctx.com.file ^ "/Makefile" in
	print_endline ("Writing " ^ filepath);
	let ch = open_out_bin filepath in
	output_string ch "ifndef MSVC\n";
	output_string ch ("\tOUT = " ^ main_name ^ "\n");
	output_string ch ("\tLDFLAGS += -lm -o $(OUT)\n");
	output_string ch ("else\n");
	output_string ch ("\tOUT = " ^ main_name ^ ".exe\n");
	output_string ch ("\tCC := llc.exe\n");
	output_string ch ("\tLINK := link.exe\n");
	output_string ch ("\tLDFLAGS += -defaultlib:libcmt -out:$(OUT)\n");
	output_string ch ("endif\n");
	output_string ch ("all: $(OUT)\n");
	List.iter (fun m ->
		output_string ch (Printf.sprintf "%s.obj: %s.ll " (relpath m.m_path) (relpath m.m_path));
		PMap.iter (fun path _ ->output_string ch (Printf.sprintf "%s.h " (relpath path))) m.m_dependencies;
		output_string ch (Printf.sprintf "\n\t$(CC) -filetype=obj %s.ll\n\n" (relpath m.m_path))
	) modules;
	output_string ch "OBJECTS = ";
	List.iter (fun m ->
		output_string ch (Printf.sprintf "%s.obj " (relpath m.m_path))
	) modules;
	output_string ch "\n\n$(OUT): $(OBJECTS)";
	output_string ch "\n\t$(LINK) $(OBJECTS) $(LDFLAGS)\n";
	output_string ch "\n\nclean:\n\t$(RM) $(OUT) $(OBJECTS)";
	close_out ch

let generate com =
	enable_pretty_stacktrace();
	let llvm = global_context() in
	let null_mod = {
		m_path = [],"";
		m_field_lut = [];
		m_class = null_class;
		m_num_closures = 0;
		m_dependencies = PMap.empty;
	} in
	let llt_int8 = Llvm.named_struct_type llvm "Null<Int8>" in
	let llt_int16 = Llvm.named_struct_type llvm "Null<Int16>" in
	let llt_int32 = Llvm.named_struct_type llvm "Null<Int32>" in
	let llt_int64 = Llvm.named_struct_type llvm "Null<Int64>" in
	let llt_float = Llvm.named_struct_type llvm "Null<Float>" in
	let llt_bool = Llvm.named_struct_type llvm "Null<Bool>" in
	let llt_tp = Llvm.named_struct_type llvm "TypeParameter" in
	let llt_vararg = Llvm.named_struct_type llvm "VarArg" in
	let types = {
		box_int8 = pointer_type llt_int8;
		box_int16 = pointer_type llt_int16;
		box_int32 = pointer_type llt_int32;
		box_int64 = pointer_type llt_int64;
		null_float = pointer_type llt_float;
		null_bool = pointer_type llt_bool;
		type_parameter = pointer_type llt_tp;
		var_args = pointer_type llt_vararg;
	} in
	let ctx = {
		com = com;
		llvm = llvm;
		t = types;
		modules = PMap.empty;
		named_structs = PMap.empty;
		curmod = Llvm.create_module llvm "";
		curmod_extra = null_mod;
		builder = builder llvm;
		vthis = const_null (i32_type llvm);
		vars = PMap.empty;
		merge_block = None;
		cond_block = None;
		landing_pad = None;
		static_inits = [];
		verification_failures = [];
	} in
	struct_set_body llt_int8 [|i8_type ctx.llvm|] false;
	struct_set_body llt_int16 [|i16_type ctx.llvm|] false;
	struct_set_body llt_int32 [|i32_type ctx.llvm|] false;
	struct_set_body llt_int64 [|i64_type ctx.llvm|] false;
	struct_set_body llt_float [|double_type ctx.llvm|] false;
	struct_set_body llt_bool [|i1_type ctx.llvm|] false;
	struct_set_body llt_tp [|i32_type ctx.llvm|] false;
	struct_set_body llt_vararg [|i32_type ctx.llvm|] false;
	(* Pass 1: Prepare & filter *)
	let types1,types2 = List.partition (fun mt -> match mt with
		| TClassDecl {cl_path = [],"hxc"} -> false
		| _ -> true
	) com.types in
	let modules = ExtList.List.filter_map (Preprocess.prepare_module_type ctx) (types1 @ types2) in
	(* Pass 2: Setup type structures & field lookup table *)
	let modules = List.filter (Preprocess.prepare_module ctx) modules in
	(* Pass 3: Emit *)
	let parts = Str.split_delim (Str.regexp "[\\/]+") com.file in
	mkdir_recursive "" parts;
	List.iter (Define.define_module ctx) modules;
	dispose_context llvm;
	generate_make_file ctx modules;
	begin match List.rev (ctx.verification_failures) with
		| [] -> ()
		| l ->
			List.iter (fun (path,s) ->
				prerr_endline (Printf.sprintf "Could not verify module %s. Reasons follow:" path);
				prerr_endline s
			) l;
			prerr_endline (Printf.sprintf "Found %i unverified modules" (List.length l));
			exit 1
	end;
