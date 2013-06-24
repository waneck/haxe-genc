open Ast
open Common
open Type

(*
	Naming conventions:
		e = Type.texpr
		t = Type.t
		p = Ast.pos
		c = Type.tclass
		cf = Type.tclass_field
		*l = ... list
		*o = ... option

	Function names:
		generate_ -> outputs content to buffer

*)

type function_context = {
	field : tclass_field;
	expr : texpr option;
	mutable local_vars : tvar list;
	mutable loop_stack : string option list;
}

type context = {
	com : Common.context;
	cvar : tvar;
	mutable num_temp_funcs : int;
	mutable num_labels : int;
	mutable num_anon_types : int;
	mutable num_identified_types : int;
	mutable anon_types : (string,string * tclass_field list) PMap.t;
	mutable type_ids : (string,int) PMap.t;
	mutable type_parameters : (path * texpr) list;
	mutable init_modules : path list;
	mutable generated_types : type_context list;
	mutable t_typeref : t -> t;
	mutable t_pointer : t -> t;
}

and type_context = {
	con : context;
	file_path_no_ext : string;
	buf_c : Buffer.t;
	buf_h : Buffer.t;
	type_path : path;
	mutable buf : Buffer.t;
	mutable tabs : string;
	mutable fctx : function_context;
	mutable dependencies : (path,bool) PMap.t;
}

module Expr = struct
	let mk_local v p =
		{ eexpr = TLocal v; etype = v.v_type; epos = p }

	let mk_ref ctx p e =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__adressof" t_dynamic); etype = t_dynamic; epos = p },
				[e]
			);
			etype = t_dynamic;
			epos = p;
		}

	let mk_comma_block ctx p el =
		let t = match List.rev el with
			| [] -> ctx.con.com.basic.tvoid
			| hd :: _ -> hd.etype
		in
		{
			eexpr = TMeta( (Meta.Comma,[],p),
			{
				eexpr = TBlock el;
				etype = t;
				epos = p;
			});
			etype = t;
			epos = p;
		}

	let mk_assign_ref ctx p local value =
		mk_comma_block ctx p [
			{
				eexpr = TBinop(Ast.OpAssign, local, value);
				etype = value.etype;
				epos = p;
			};
			mk_ref ctx p local
		]

	let mk_cast t e =
		{ e with eexpr = TCast(e, None); etype = t }

	let mk_deref ctx p e =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__deref" t_dynamic); etype = t_dynamic; epos = p },
				[e]
			);
			etype = t_dynamic;
			epos = p;
		}

	let mk_call ctx p name args =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__call" t_dynamic); etype = t_dynamic; epos = p },
				{ eexpr = TConst(TString name); etype = t_dynamic; epos = p } :: args
			);
			etype = t_dynamic;
			epos = p;
		}

	let mk_sizeof ctx p e =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__sizeof__" t_dynamic); etype = t_dynamic; epos = p },
				[e]
			);
			etype = ctx.con.com.basic.tint;
			epos = p;
		}

	let mk_type_param ctx pos t =
		let t = ctx.con.t_typeref t in
		let c,p = match follow t with
			| TInst(c,p) -> c,p
			| _ -> assert false
		in
		{ eexpr = TNew(c,p,[]); etype = t; epos = pos }

	let mk_stack_tp_init ctx t p =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__call" t_dynamic); etype = t_dynamic; epos = p },
				[
					{ eexpr = TConst (TString "malloc"); etype = t_dynamic; epos = p };
					{
						eexpr = TCall(
							{ eexpr = TLocal(alloc_var "__sizeof__" t_dynamic); etype = t_dynamic; epos = p },
							[mk_type_param ctx p t]
						);
						etype = ctx.con.com.basic.tint;
						epos = p;
					}
				]
			);
			etype = t;
			epos = p;
		}

	let mk_ccode ctx s =
		mk (TCall ((mk (TLocal ctx.con.cvar) t_dynamic Ast.null_pos), [mk (TConst (TString s)) t_dynamic Ast.null_pos])) t_dynamic Ast.null_pos

	let mk_int ctx i p =
		mk (TConst (TInt (Int32.of_int i))) ctx.con.com.basic.tint p

	let debug ctx e =
		Printf.sprintf "%s: %s" ctx.fctx.field.cf_name (s_expr (s_type (print_context())) e)
end

(* Output and context *)

let spr ctx s = Buffer.add_string ctx.buf s
let print ctx = Printf.kprintf (fun s -> Buffer.add_string ctx.buf s)

let newline ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '{' | ':' | ' '
	| '}' when Buffer.length ctx.buf > 1 && Buffer.nth ctx.buf (Buffer.length ctx.buf - 2) != '"' ->
		print ctx "\n%s" ctx.tabs
	| '\t' -> ()
	| _ ->
		print ctx ";\n%s" ctx.tabs

let rec concat ctx s f = function
	| [] -> ()
	| [x] -> f x
	| x :: l ->
		f x;
		spr ctx s;
		concat ctx s f l

let open_block ctx =
	let oldt = ctx.tabs in
	ctx.tabs <- "\t" ^ ctx.tabs;
	(fun() -> ctx.tabs <- oldt)

let mk_type_context con path =
	let rec create acc = function
		| [] -> ()
		| d :: l ->
			let pdir = String.concat "/" (List.rev (d :: acc)) in
			if not (Sys.file_exists pdir) then Unix.mkdir pdir 0o755;
			create (d :: acc) l
	in
	let dir = con.com.file :: fst path in
	create [] dir;
	let buf_c = Buffer.create (1 lsl 14) in
	let buf_h = Buffer.create (1 lsl 14) in
	{
		con = con;
		file_path_no_ext = String.concat "/" dir ^ "/" ^ (snd path);
		buf = buf_h;
		buf_c = buf_c;
		buf_h = buf_h;
		tabs = "";
		type_path = path;
		fctx = {
			local_vars = [];
			field = null_field;
			expr = None;
			loop_stack = [];
		};
		dependencies = PMap.empty;
	}

let path_to_name (pack,name) = match pack with [] -> name | _ -> String.concat "_" pack ^ "_" ^ name
let path_to_file_path (pack,name) = match pack with [] -> name | _ -> String.concat "/" pack ^ "/" ^ name

let get_relative_path source target =
	let rec loop pl1 pl2 acc = match pl1,pl2 with
		| s1 :: pl1,[] ->
			loop pl1 [] (".." :: acc)
		| [],s2 :: pl2 ->
			loop [] pl2 (s2 :: acc)
		| s1 :: pl1,s2 :: pl2 ->
			if s1 = s2 then loop pl1 pl2 acc
			else (List.map (fun _ -> "..") (s1 :: pl1)) @ [s2] @ pl2
		| [],[] ->
			List.rev acc
	in
	loop (fst source) (fst target) []

let close_type_context ctx =
	ctx.con.generated_types <- ctx :: ctx.con.generated_types;
	let buf = Buffer.create (Buffer.length ctx.buf_h) in
	let spr = Buffer.add_string buf in
	let n = "_h" ^ path_to_name ctx.type_path in
	spr (Printf.sprintf "#ifndef %s\n" n);
	spr (Printf.sprintf "#define %s\n" n);
	spr "#include <stdio.h>\n";
	spr "#include <stdlib.h>\n";
	spr "#include <string.h>\n";

	let relpath path = path_to_file_path ((get_relative_path ctx.type_path path),snd path) in
	PMap.iter (fun path b ->
		let name = path_to_name path in
		if b then begin
			if path = (["hxc"],"AnonTypes") || path = (["c";"hxc"],"Exception") then spr (Printf.sprintf "#include \"%s.h\"\n" (relpath path))
			else spr (Printf.sprintf "typedef struct %s %s;\n" name name);
		end else spr (Printf.sprintf "#include <%s.h>\n" (path_to_file_path path))
	) ctx.dependencies;
	Buffer.add_buffer buf ctx.buf_h;
	spr "\n#endif";

	let write_if_changed filepath content =
		try
			let cur = Std.input_file ~bin:true filepath in
			if cur <> content then raise Not_found
		with Not_found | Sys_error _ ->
			let ch_h = open_out_bin filepath in
			print_endline ("Writing to " ^ filepath);
			output_string ch_h content;
			close_out ch_h;
	in

	write_if_changed (ctx.file_path_no_ext ^ ".h") (Buffer.contents buf);

	let sc = Buffer.contents ctx.buf_c in
	if String.length sc > 0 then begin
		let buf = Buffer.create (Buffer.length ctx.buf_c) in
		Buffer.add_string buf ("#include \"" ^ (snd ctx.type_path) ^ ".h\"\n");
		PMap.iter (fun path b ->
			if b then Buffer.add_string buf (Printf.sprintf "#include \"%s.h\"\n" (relpath path))
		) ctx.dependencies;
		Buffer.add_string buf sc;
		write_if_changed (ctx.file_path_no_ext ^ ".c") (Buffer.contents buf);
	end

let add_dependency ctx path =
	if path <> ctx.type_path then ctx.dependencies <- PMap.add path true ctx.dependencies

let parse_include com s p =
	if s.[0] = '<' then begin
		if s.[String.length s - 1] <> '>' then com.error "Invalid include directive" p;
		(* take off trailing .h because it will be added back later *)
		let i = if String.length s > 4 && s.[String.length s - 2] = 'h' && s.[String.length s - 3] = '.' then
			String.length s - 4
		else
			String.length s - 2
		in
		([],String.sub s 1 i),false
	end else
		([],s),true

let check_include_meta ctx meta =
	try
		let _,el,p = get_meta Meta.Include meta in
		List.iter (fun e -> match fst e with
			| EConst(String s) when String.length s > 0 ->
				let path,b = parse_include ctx.con.com s p in
				ctx.dependencies <- PMap.add path b ctx.dependencies
			| _ ->
				()
		) el;
		true
	with Not_found ->
		false

let add_class_dependency ctx c =
	if not (check_include_meta ctx c.cl_meta) && not c.cl_extern then add_dependency ctx c.cl_path

let add_enum_dependency ctx en =
	if not (check_include_meta ctx en.e_meta) && not en.e_extern then add_dependency ctx en.e_path

let add_abstract_dependency ctx a =
	if not (check_include_meta ctx a.a_meta) then add_dependency ctx a.a_path

let add_type_dependency ctx t = match follow t with
	| TInst(c,_) ->
		add_class_dependency ctx c
	| TEnum(en,_) ->
		add_enum_dependency ctx en
	| TAnon _ ->
		add_dependency ctx (["hxc"],"AnonTypes");
	| TAbstract(a,_) ->
		add_abstract_dependency ctx a
	| TDynamic _ ->
		add_dependency ctx ([],"Dynamic")
	| _ ->
		(* TODO: that doesn't seem quite right *)
		add_dependency ctx ([],"Dynamic")

(* Helper *)

let rec is_value_type ctx t = match follow t with
	| TAbstract({ a_impl = None }, _) -> true
	(* | TInst(c,_) -> has_meta Meta.Struct c.cl_meta *)
	| TEnum(_,_) -> false (* TODO: define when a TEnum will be stack-allocated and when it won't *)
	| TAbstract(a,tl) ->
		if has_meta Meta.NotNull a.a_meta then
			true
		else
			is_value_type ctx (Codegen.Abstract.get_underlying_type a tl)
	| _ -> false

let begin_loop ctx =
	ctx.fctx.loop_stack <- None :: ctx.fctx.loop_stack;
	fun () ->
		match ctx.fctx.loop_stack with
		| ls :: l ->
			(match ls with None -> () | Some s -> print ctx "%s: {}" s);
			ctx.fctx.loop_stack <- l;
		| _ ->
			assert false

let full_field_name c cf = (path_to_name c.cl_path) ^ "_" ^ cf.cf_name
let full_enum_field_name en ef = (path_to_name en.e_path) ^ "_" ^ ef.ef_name

let monofy_class c = TInst(c,List.map (fun _ -> mk_mono()) c.cl_types)

let get_fun t = match follow t with | TFun(r1,r2) -> (r1,r2) | _ -> assert false

(* since we change the functions' signatures to add 'this' and type parameters,
we need a fuzzy map2 version which won't fail, and discard the remaining arguments *)
let rec fuzzy_map2 f l1 l2 acc = match l1, l2 with
	| v1 :: l1, v2 :: l2 -> fuzzy_map2 f l1 l2 ( (f v1 v2) :: acc )
	| _ :: _, [] -> (*(List.rev l1) @ acc*) assert false (* shouldn't happen *)
	| _ -> acc

let is_type_param ctx t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ },_) -> true
	| _ -> false

let function_has_type_parameter ctx t = match follow t with
	| TFun(args,ret) -> is_type_param ctx ret || List.exists (fun (_,_,t) -> is_type_param ctx t) args
	| _ -> false

let is_field_type_param ctx e = is_type_param ctx e.etype || match e.eexpr with
	| TField(_, (FInstance(_,cf) | FStatic(_,cf))) when is_type_param ctx cf.cf_type -> true
	| _ -> false

(* Type signature *)

let anon_signature ctx fields =
	let fields = PMap.fold (fun cf acc -> cf :: acc) fields [] in
	let fields = List.sort (fun cf1 cf2 -> compare cf1.cf_name cf2.cf_name) fields in
	let id = String.concat "," (List.map (fun cf -> cf.cf_name ^ (s_type (print_context()) (follow cf.cf_type))) fields) in
	try fst (PMap.find id ctx.con.anon_types)
	with Not_found ->
		ctx.con.num_anon_types <- ctx.con.num_anon_types + 1;
		let s = "_hx_anon_" ^ (string_of_int ctx.con.num_anon_types) in
		ctx.con.anon_types <- PMap.add id (s,fields) ctx.con.anon_types;
		s

let t_path t = match follow t with
	| TInst(c,_) -> c.cl_path
	| TEnum(e,_) -> e.e_path
	| TAbstract(a,_) -> a.a_path
	| _ -> [],"Dynamic"

let rec s_type ctx t =
	match follow t with
	| TAbstract({a_path = [],"Int"},[]) -> "int"
	| TAbstract({a_path = [],"Float"},[]) -> "double"
	| TAbstract({a_path = [],"Void"},[]) -> "void"
	| TAbstract({a_path = ["c"],"Pointer"},[t]) -> (match follow t with
		| TInst({cl_kind = KTypeParameter _},_) ->
			"char*" (* we will manipulate an array of type parameters like an array of bytes *)
		| _ -> s_type ctx t ^ "*")
	| TInst(({cl_path = [],"typeref"} as c),_) ->
		add_class_dependency ctx c;
		"const " ^ (path_to_name c.cl_path) ^ "*"
	| TAbstract({a_path = [],"Bool"},[]) -> "int"
	| TInst({cl_path = [],"String"},[]) -> "char*"
	| TInst({cl_kind = KTypeParameter _},_) -> "void*"
	| TInst(c,_) ->
		let ptr = if is_value_type ctx t then "" else "*" in
		add_class_dependency ctx c;
		(path_to_name c.cl_path) ^ ptr
	| TEnum(en,_) ->
		let ptr = if is_value_type ctx t then "" else "*" in
		if not en.e_extern then add_dependency ctx en.e_path;
		(path_to_name en.e_path) ^ ptr
	| TAnon a ->
		begin match !(a.a_status) with
		| Statics c -> "Class_" ^ (path_to_name c.cl_path) ^ "*"
		| EnumStatics en -> "Enum_" ^ (path_to_name en.e_path) ^ "*"
		| AbstractStatics a -> "Anon_" ^ (path_to_name a.a_path) ^ "*"
		| _ ->
			add_dependency ctx (["hxc"],"AnonTypes");
			(anon_signature ctx a.a_fields) ^ "*"
		end
	| _ -> "void*"

let get_type_id ctx t =
	let id = Type.s_type (print_context()) (follow t) in
	try
		PMap.find id ctx.con.type_ids
	with Not_found ->
		ctx.con.num_identified_types <- ctx.con.num_identified_types + 1;
		ctx.con.type_ids <- PMap.add id ctx.con.num_identified_types ctx.con.type_ids;
		ctx.con.num_identified_types

(* Some mess *)

let infer_params ctx pos (original_args:((string * bool * t) list * t)) (applied_args:((string * bool * t) list * t)) (params:(string * t) list) calls_parameters_explicitly : tparams =
	match params with
	| [] -> []
	| _ ->
		let args_list args = (if not calls_parameters_explicitly then t_dynamic else snd args) :: (List.map (fun (n,o,t) -> t) (fst args)) in

		let monos = List.map (fun _ -> mk_mono()) params in
		let original = args_list (get_fun (apply_params params monos (TFun(fst original_args,snd original_args)))) in
		let applied = args_list applied_args in

		(try
			List.iter2 (fun a o ->
				unify a o
				(* type_eq EqStrict a o *)
			) applied original
			(* unify applied original *)
		with | Unify_error el ->
				(* List.iter (fun el -> gen.gcon.warning (Typecore.unify_error_msg (print_context()) el) pos) el; *)
				ctx.con.com.warning ("This expression may be invalid") pos
		| Invalid_argument("List.map2") ->
				ctx.con.com.warning ("This expression may be invalid") pos
		);

		List.map (fun t ->
			match follow t with
				| TMono _ -> t_dynamic
				| t -> t
		) monos

(* Expr generation *)

let rec generate_call ctx e e1 el = match e1.eexpr,el with
	| TLocal({v_name = "__trace"}),[e1] ->
		spr ctx "printf(\"%s\\n\",";
		generate_expr ctx e1;
		spr ctx ")";
	| TLocal({v_name = "__c"}),[{eexpr = TConst(TString code)}] ->
		spr ctx code;
	| TLocal({v_name = "__call"}),{eexpr = TConst(TString name)} :: p ->
		print ctx "%s(" name;
		concat ctx "," (generate_expr ctx) p;
		spr ctx ")";
	(* pass by reference call *)
	| TLocal({v_name = "__adressof"}),[e1] ->
		spr ctx "&";
		generate_expr ctx e1;
	(* dereference operator *)
	| TLocal({v_name = "__deref"}),[e1] ->
		spr ctx "*";
		generate_expr ctx e1;
	(* sizeof *)
	| TLocal({v_name = "__sizeof__"}),[e1] ->
		(* get TypeReference's type *)
		let t = match follow e1.etype with
			| TInst({cl_path = [],"typeref"},[t]) -> t
			| _ -> ctx.con.com.error "This expression cannot be generated. Expected a TypeReference type" e1.epos; assert false
		in
		(match follow t with
		| TInst({cl_kind = KTypeParameter _},_) ->
			(* indirection *)
			spr ctx "(";
			generate_expr ctx e1;
			spr ctx ")->refSize"
		| _ ->
			print ctx "sizeof(%s)" (s_type ctx t));
	(* pointer functions *)
	| TField(_,FStatic({cl_path = ["c";"_Pointer"],"Pointer_Impl_"}, ({ cf_name = ("add"|"increment") } as cf))), p ->
		spr ctx "(";
		(match cf.cf_name, p with
		| "add", [a;o] ->
			generate_expr ctx a;
			spr ctx " + ";
			generate_expr ctx o
		| "increment", [a] ->
			generate_expr ctx a;
			spr ctx "++"
		| _ -> assert false);
		spr ctx ")";
	| TField(ef,FInstance(c,({cf_params = _::_} as cf))),el ->
		generate_tparam_call ctx e e1 ef c cf false el;
	| TField(ef,FStatic(c,({cf_params = _::_} as cf))),el ->
		generate_tparam_call ctx e e1 ef c cf true el;
	| TField(e1,FInstance(c,cf)),el ->
		add_class_dependency ctx c;
		spr ctx (full_field_name c cf);
		spr ctx "(";
		generate_expr ctx e1;
		List.iter (fun e ->
			spr ctx ",";
			generate_expr ctx e
		) el;
		spr ctx ")"
	| TField(_,FEnum(en,ef)),el ->
		print ctx "new_%s(" (full_enum_field_name en ef);
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")"
	| _ ->
		generate_expr ctx e1;
		spr ctx "(";
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")"

and generate_tparam_call ctx e ef e1 c cf static el =
	(* FIXME: cf.cf_type may inherit the type parameters of a superclass *)
	(* get type parameter-related function parameters *)
	let original, oret = get_fun cf.cf_type in
	let applied, aret = get_fun ef.etype in
	(* since we've messed with cf_type, we need to restore it now *)
	(* take off extra 'original' parameters *)
	let rec loop ro ra acc = match ro, ra with
		| o :: ro, _ :: ra -> loop ro ra (o :: acc)
		| _ -> acc
	in
	let original = loop (List.rev original) applied [] in
	(* normalization complete now *)
	let params = infer_params ctx e.epos (original,oret) (applied,aret) cf.cf_params true in
	(* get complete parameter list *)
	let el = (if static then [] else [e1]) @ List.map (Expr.mk_type_param ctx e.epos) params @ el in
	add_class_dependency ctx c;
	spr ctx (full_field_name c cf);
	spr ctx "(";
	concat ctx "," (generate_expr ctx) el;
	spr ctx ")"

and generate_expr ctx e = match e.eexpr with
	| TArray(e1, e2) ->
		generate_expr ctx e1;
		spr ctx "[";
		generate_expr ctx e2;
		spr ctx "]"
	| TMeta( (Meta.Comma,_,_), { eexpr = TBlock(el) } ) when el <> [] ->
		spr ctx "(";
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")"
	| TBlock([]) ->
		spr ctx "{ }"
	| TBlock(el) ->
		spr ctx "{";
		let b = open_block ctx in
		List.iter (fun e ->
			newline ctx;
			generate_expr ctx e;
		) el;
		b();
		newline ctx;
		spr ctx "}";
		newline ctx;
	| TConst(TString s) ->
		print ctx "\"%s\"" s
	| TConst(TInt i) ->
		print ctx "%ld" i
	| TConst(TFloat s) ->
		print ctx "%s" s
	| TConst TNull when is_type_param ctx e.etype ->
		generate_expr ctx (Expr.mk_type_param ctx e.epos e.etype);
		spr ctx "->nullval"
	| TConst(TNull) ->
		spr ctx "NULL"
	| TConst(TSuper) ->
		(* TODO: uhm... *)
		()
	| TConst(TBool true) ->
		spr ctx "1"
	| TConst(TBool false) ->
		spr ctx "0"
	| TConst(TThis) ->
		spr ctx "this"
	| TCall(e1,el) ->
		generate_call ctx e e1 el
	| TTypeExpr (TClassDecl c) ->
		spr ctx (path_to_name c.cl_path);
	| TTypeExpr (TEnumDecl e) ->
		add_dependency ctx e.e_path;
		spr ctx (path_to_name e.e_path);
	| TTypeExpr (TTypeDecl _ | TAbstractDecl _) ->
		(* shouldn't happen? *)
		assert false
	| TField(_,FStatic(c,cf)) ->
		add_class_dependency ctx c;
		spr ctx (full_field_name c cf)
	| TField(_,FEnum(en,ef)) ->
		add_dependency ctx en.e_path;
		print ctx "new_%s()" (full_enum_field_name en ef)
	| TField(e1,fa) ->
		let n = field_name fa in
		spr ctx "(";
		generate_expr ctx e1;
		if is_value_type ctx e1.etype then
			print ctx ").%s" n
		else
			print ctx ")->%s" n
	| TLocal v ->
		spr ctx v.v_name;
	| TObjectDecl fl ->
		let s = match follow e.etype with TAnon a -> anon_signature ctx a.a_fields | _ -> assert false in
		let fl = List.sort (fun (n1,_) (n2,_) -> compare n1 n2) fl in
		print ctx "new_%s(" s;
		concat ctx "," (generate_expr ctx) (List.map (fun (_,e) -> add_type_dependency ctx e.etype; e) fl);
		spr ctx ")";
	| TNew({cl_path = [],"typeref"},[p],[]) -> (match follow p with
		| TInst(({cl_kind = KTypeParameter _} as c),_) -> (try
			let expr = List.assoc c.cl_path ctx.con.type_parameters in
			generate_expr ctx expr
		with | Not_found ->
			ctx.con.com.error ("Cannot find type parameter called " ^ s_type_path c.cl_path) e.epos)
		| _ ->
			let path = t_path p in
			add_type_dependency ctx p;
			spr ctx ("&" ^ (path_to_name path ) ^ "__typeref"))
	| TNew(c,tl,el) ->
		let el = List.map (Expr.mk_type_param ctx e.epos) tl @ el in
		add_class_dependency ctx c;
		spr ctx (full_field_name c (match c.cl_constructor with None -> assert false | Some cf -> cf));
		spr ctx "(";
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")";
	| TReturn None ->
		spr ctx "return"
	| TReturn (Some e1) ->
		spr ctx "return (";
		generate_expr ctx e1;
		spr ctx ")"
	| TVars(vl) ->
		let f (v,eo) =
			print ctx "%s %s" (s_type ctx v.v_type) v.v_name;
			begin match eo with
				| None -> ()
				| Some e ->
					spr ctx " = ";
					generate_expr ctx e;
			end
		in
		concat ctx ";" f vl
	| TWhile(e1,e2,NormalWhile) ->
		spr ctx "while";
		generate_expr ctx e1;
		let l = begin_loop ctx in
		generate_expr ctx (mk_block e2);
		l()
	| TWhile(e1,e2,DoWhile) ->
		spr ctx "do";
		let l = begin_loop ctx in
		generate_expr ctx (mk_block e2);
		spr ctx " while";
		generate_expr ctx e1;
		l()
	| TContinue ->
		spr ctx "continue";
	| TBreak _ ->
		let label = match ctx.fctx.loop_stack with
			| (Some s) :: _ -> s
			| None :: l ->
				let s = Printf.sprintf "_hx_label%i" ctx.con.num_labels in
				ctx.con.num_labels <- ctx.con.num_labels + 1;
				ctx.fctx.loop_stack <- (Some s) :: l;
				s
			| [] ->
				assert false
		in
		print ctx "goto %s" label;
	| TIf(e1,e2,e3) ->
		spr ctx "if";
		generate_expr ctx e1;
		generate_expr ctx (mk_block e2);
		(match e3 with None -> () | Some e3 ->
			spr ctx " else ";
			generate_expr ctx (mk_block e3))
	| TSwitch(e1,cases,edef) ->
		spr ctx "switch";
		generate_expr ctx e1;
		spr ctx "{";
		let generate_case_expr e =
			let b = open_block ctx in
			List.iter (fun e ->
				newline ctx;
				generate_expr ctx e;
			) (match e.eexpr with TBlock el -> el | _ -> [e]);
			newline ctx;
			spr ctx "break";
			b();
		in
		let b = open_block ctx in
		newline ctx;
		List.iter (fun (el,e) ->
			spr ctx "case ";
			concat ctx "," (generate_expr ctx) el;
			spr ctx ":";
			generate_case_expr e;
			newline ctx;
		) cases;
		begin match edef with
			| None -> ()
			| Some e ->
				spr ctx "default:";
				generate_case_expr e;
		end;
		b();
		newline ctx;
		spr ctx "}";
	| TBinop((OpEq | OpNotEq) as op,e1,e2) when (match follow e1.etype with TInst({cl_path = [],"String"},_) -> true | _ -> false) ->
		spr ctx "strcmp(";
		generate_expr ctx e1;
		spr ctx ",";
		generate_expr ctx e2;
		print ctx ") %s 0" (if op = OpEq then "==" else "!=")
	| TBinop(op,e1,e2) ->
		generate_expr ctx e1;
		print ctx " %s " (s_binop op);
		generate_expr ctx e2;
	| TUnop(op,Prefix,e1) ->
		spr ctx (s_unop op);
		generate_expr ctx e1;
	| TUnop(op,Postfix,e1) ->
		generate_expr ctx e1;
		spr ctx (s_unop op);
	| TParenthesis e1 ->
		spr ctx "(";
		generate_expr ctx e1;
		spr ctx ")";
	| TArrayDecl _ | TTry _ | TFor _ ->
		(* handled in function context pass *)
		assert false
	| TMeta(_,e) ->
		generate_expr ctx e
	| TCast(e1,_) ->
		print ctx "((%s) " (s_type ctx e.etype);
		generate_expr ctx e1;
		spr ctx ")"
	| TEnumParameter (e1,ef,i) ->
		generate_expr ctx e1;
		begin match follow e1.etype with
			| TEnum(en,_) ->
				add_dependency ctx en.e_path;
				let s,_,_ = match ef.ef_type with TFun(args,_) -> List.nth args i | _ -> assert false in
				print ctx "->args.%s.%s" ef.ef_name s;
			| _ ->
				assert false
		end
	| TThrow e1 ->
		ctx.dependencies <- PMap.add ([],"setjmp") false ctx.dependencies;
		add_dependency ctx (["c";"hxc"],"Exception");
		spr ctx "c_hxc_Exception_thrownObject = ";
		generate_expr ctx e1;
		newline ctx;
		print ctx "(longjmp(*c_hxc_Exception_peek(),%i))" (get_type_id ctx e1.etype);
	| TPatMatch dt ->
		let fl = ctx.con.num_labels in
		ctx.con.num_labels <- ctx.con.num_labels + (Array.length dt.dt_dt_lookup) + 1;
		let mk_label i = Printf.sprintf "_hx_label%i" (i + fl) in
		let rec loop d =
			match d with
			| DTGoto i ->
				print ctx "goto %s" (mk_label i);
				newline ctx;
			| DTBind(bl,dt) ->
				List.iter (fun ((v,p),e) ->
					print ctx "%s = " v.v_name;
					generate_expr ctx e;
					newline ctx;
				) bl;
				loop dt
			| DTExpr e -> generate_expr ctx (mk_block e)
			| DTGuard(e, dt1, dt2) ->
				spr ctx "if(";
				generate_expr ctx e;
				spr ctx ")";
				loop dt1;
				(match dt2 with None -> () | Some dt ->
					spr ctx " else ";
					loop dt)
			| DTSwitch(e,cl) ->
				let def = ref None in
				let cl = List.filter (fun (e,dt) ->
					match e.eexpr with
	 				| TMeta((Meta.MatchAny,_,_),_) ->
						def := Some dt;
						false
					| _ ->
						true
				) cl in
				spr ctx "switch(";
				generate_expr ctx e;
				spr ctx ") {";
				let b = open_block ctx in
				List.iter (fun (e,dt) ->
					newline ctx;
					spr ctx "case ";
					generate_expr ctx e;
					spr ctx ":";
					loop dt;
					newline ctx;
					spr ctx "break";
				) cl;
				begin match !def with
					| None -> ()
					| Some dt ->
						newline ctx;
						spr ctx "default:";
						loop dt;
						newline ctx;
						spr ctx "break";
				end;
				b();
				newline ctx;
				spr ctx "}";
				newline ctx;
		in
		print ctx "goto %s" (mk_label dt.dt_first);
		Array.iteri (fun i d ->
			newline ctx;
			print ctx "%s: {}" (mk_label i);
			newline ctx;
			loop d;
			print ctx "goto %s" (mk_label (Array.length dt.dt_dt_lookup));
			newline ctx;
		) dt.dt_dt_lookup;
		print ctx "%s: {}" (mk_label (Array.length dt.dt_dt_lookup));
		newline ctx;
	| TFunction _ ->
		print_endline ("Not implemented yet: " ^ (Expr.debug ctx e))

let mk_array_decl ctx el t p =
	let ts, eparam = match follow t with
		| TInst(_,[t]) -> s_type ctx t, Expr.mk_type_param ctx p t
		| _ -> assert false
	in
	let name = "_hx_func_" ^ (string_of_int ctx.con.num_temp_funcs) in
	let arity = List.length el in
	print ctx "Array* %s(%s) {" name (String.concat "," (ExtList.List.mapi (fun i e -> Printf.sprintf "%s v%i" (s_type ctx e.etype) i) el));
	ctx.con.num_temp_funcs <- ctx.con.num_temp_funcs + 1;
	let bl = open_block ctx in
	newline ctx;
	print ctx "%s arr[%i]" ts arity;
	newline ctx;
	ExtList.List.iteri (fun i e ->
		print ctx "arr[%i] = v%i" i i;
		newline ctx;
	) el;
	spr ctx "return Array_ofPointerCopy(";
	generate_expr ctx eparam;
	print ctx ", %d, arr)" arity;
	bl();
	newline ctx;
	spr ctx "}";
	newline ctx;
	let v = alloc_var name t_dynamic in
	let ev = mk (TLocal v) v.v_type p in
	mk (TCall(ev,el)) t p


(* Type generation *)

(*
	This function applies some general transformations.

	- locals are collected
	- TVars are replaced by assignments
	- TArrayDecl introduces an init function which is TCalled
	- TTry is replaced with a TSwitch and uses setjmp
	- TPatMatch has some var names sanitized
	- TFor is replaced with TWhile
*)
let mk_function_context ctx cf =
	let locals = ref [] in

	(* collected parameter stack variables for type parameter passing *)
	let all_params = ref (PMap.empty) in
	let cur_params = ref (PMap.empty) in
	let add_param t =
		let path = t_path t in
		let cur_i, cur_arr = try
			PMap.find path !cur_params
		with | Not_found ->
			(0,[])
		in
		let n = path_to_name path ^ "_tmp_" ^ (string_of_int cur_i) in
		let v = try
			PMap.find n !all_params
		with | Not_found ->
			let v = alloc_var n t in
			locals := v :: !locals;
			all_params := PMap.add n v !all_params;
			v
		in
		cur_params := PMap.add path (cur_i + 1, v :: cur_arr) !cur_params;
		v
	in
	let force_add v =
		let path = t_path v.v_type in
		let n = path_to_name path ^ "_tmp_0" in
		all_params := PMap.add n v !all_params
	in

	let returns_type_param = match follow cf.cf_type with
		| TFun(_,ret) -> is_type_param ctx ret
		| _ -> false
	in
	let out_var = ref None in

	let rec loop e = match e.eexpr with
		(** 2nd pass type parameter handling **)
		(** collect the temporary stack variables that need to be created so their address can be passed around *)
		| TCall(({ eexpr = TField(ef, (FInstance(c,cf) | FStatic(c,cf) as fi)) } as e1), el)
		when function_has_type_parameter ctx cf.cf_type ->
			let old_params = !cur_params in
			let ef = loop ef in
			let args, ret = get_fun cf.cf_type in
			let args = List.rev args in
			(* if return type is a type param, add new element to call params *)
			let _, applied_ret = get_fun e1.etype in
			let args, el_last = if is_type_param ctx ret then begin
				let v = add_param applied_ret in
				let args = match args with
					| ("__out__",_,_) :: args -> args
					| _ -> args
				in (* FIXME: since this isn't entirely multi-pass, we can't guarantee that pass 1 already ran; do this right *)
				if is_type_param ctx applied_ret then
					args, [Expr.mk_local v e.epos] (* already a reference var *)
				else
					args, [Expr.mk_ref ctx e.epos (Expr.mk_local v e.epos)]
			end else
				args, []
			in

			let el = fuzzy_map2 (fun e (_,_,t) -> match e.eexpr with
				| _ when not (is_type_param ctx t) -> loop e
				| TLocal _ when is_type_param ctx e.etype -> (* type params are already encoded as pointer-to-val *)
          loop e
        | TLocal _ ->
					Expr.mk_ref ctx e.epos (loop e)
				| _ ->
					let v = add_param e.etype in
					loop (Expr.mk_assign_ref ctx e.epos (Expr.mk_local v e.epos) e)
			) (List.rev el) args []
			in
			cur_params := old_params;
			let eret = { e with eexpr = TCall({ e1 with eexpr = TField(ef, fi) }, el @ el_last) } in
			(* if type parameter is being cast into a concrete one, we need to dereference it *)
			if is_type_param ctx ret && not (is_type_param ctx applied_ret) then
				Expr.mk_deref ctx e.epos (Expr.mk_cast (ctx.con.t_pointer applied_ret) eret)
			else
				eret
		(* TODO: check also for when a TField with type parameter is read / set; its contents must be copied *)
		| TReturn (Some er) when Option.is_some !out_var ->
			let out_var = Option.get !out_var in
			(* when returning type parameter values, instead of just returning a value, we must first set the out var *)
			let ret_val = Expr.mk_comma_block ctx e.epos [
				{
					eexpr = TBinop(Ast.OpAssign, Expr.mk_local out_var e.epos, er);
					etype = er.etype;
					epos = e.epos;
				};
				Expr.mk_local out_var e.epos
			] in
			{ e with eexpr = TReturn(Some( loop ret_val )) }
		(* type parameter extra indirection handling *)
		(* we need to handle the following cases: *)
		(* - param -> param assign: always copy *)
		(* - param -> concrete cast/field use: dereference *)
		(* - concrete -> param cast/field get/set: copy *)
		| TBinop( (Ast.OpAssign | Ast.OpAssignOp _ as op), e1, e2 ) when is_field_type_param ctx e1 || is_field_type_param ctx e2 ->
			if is_field_type_param ctx e1 && is_field_type_param ctx e2 then begin
				let old_params = !cur_params in
				if op <> Ast.OpAssign then assert false; (* FIXME: AssignOp should become two operations; should be very rare though *)
				let local, wrap = match e1.eexpr with
					| TLocal _ -> e1, (fun e -> e)
					| _ ->
						let v = add_param t_dynamic in
						Expr.mk_local v e1.epos, (fun e -> { e with eexpr = TBinop(Ast.OpAssign, Expr.mk_local v e.epos, e) })
				in
				let ret = Expr.mk_comma_block ctx e.epos [
					(Expr.mk_call ctx e.epos "memcpy" [ wrap(loop e1); loop e2; Expr.mk_sizeof ctx e.epos (Expr.mk_type_param ctx e.epos e1.etype) ]);
					local
				] in
				cur_params := old_params;
				ret
			end else if is_field_type_param ctx e1 then
				{ e with eexpr = TBinop(op, Expr.mk_deref ctx e.epos (Expr.mk_cast (ctx.con.t_pointer e2.etype) (loop e1)), loop e2) }
			else
				{ e with eexpr = TBinop(op, loop e1, Expr.mk_deref ctx e.epos (Expr.mk_cast (ctx.con.t_pointer e1.etype) (loop e2))) }
		(* - pointer array access -> pointer + typeref's size * index *)
		| TArray(e1, idx) -> (match follow e1.etype with
			| TAbstract({a_path=["c"], "Pointer"},[t])
			| TInst({cl_path=["c"], "_PointerR"},[t]) when is_type_param ctx t ->
				{ e with
					eexpr = TBinop(
						Ast.OpAdd, loop e1,
						{ idx with eexpr = TBinop(Ast.OpMult, loop idx, Expr.mk_sizeof ctx e.epos (Expr.mk_type_param ctx e.epos t)) }
					);
				}
			| TInst(c,p) -> (try
				let f = PMap.find "__get" c.cl_fields in
				loop { e with eexpr = TCall({ e1 with eexpr = TField(e1,FInstance(c,f)); etype = apply_params c.cl_types p f.cf_type}, [idx]) }
			with | Not_found -> Type.map_expr loop e)
			| _ -> Type.map_expr loop e)
		| TBinop( (Ast.OpAssign | Ast.OpAssignOp _ as op), {eexpr = TArray(e1,e2)}, v) -> (try
				match follow e1.etype with
				| TInst(c,p) ->
				let f = PMap.find "__set" c.cl_fields in
				if op <> Ast.OpAssign then assert false; (* FIXME: this should be handled in an earlier stage (gencommon, anyone?) *)
				loop { e with eexpr = TCall({ e1 with eexpr = TField(e1,FInstance(c,f)); etype = apply_params c.cl_types p f.cf_type}, [e2;v]) }
				| _ -> raise Not_found
			with | Not_found ->
				Type.map_expr loop e)

		(** end of type parameter handling on 2nd pass **)
		| TVars vl ->
			let el = ExtList.List.filter_map (fun (v,eo) ->
				locals := v :: !locals;
				match eo with
				| None -> None
				| Some e -> Some (loop (mk (TBinop(OpAssign, mk (TLocal v) v.v_type e.epos,e)) e.etype e.epos))
			) vl in
			begin match el with
			| [e] -> e
			| _ -> mk (TBlock el) ctx.con.com.basic.tvoid e.epos
			end
		| TArrayDecl el ->
			mk_array_decl ctx (List.map loop el) e.etype e.epos
		| TTry (e1,cl) ->
			ctx.dependencies <- PMap.add ([],"setjmp") false ctx.dependencies;
			add_dependency ctx (["c";"hxc"],"Exception");
			let esubj = Expr.mk_ccode ctx "(setjmp(*c_hxc_Exception_push()))" in
			let epop = Expr.mk_ccode ctx "c_hxc_Exception_pop()" in
			let epopassign = Expr.mk_ccode ctx "jmp_buf* _hx_jmp_buf = c_hxc_Exception_pop()" in
			let c1 = [Expr.mk_int ctx 0 e.epos],(Codegen.concat (loop e1) epop) in
			let def = ref None in
			let cl = c1 :: (ExtList.List.filter_map (fun (v,e) ->
				let eassign = Expr.mk_ccode ctx ((s_type ctx v.v_type) ^ " " ^ v.v_name ^ " = c_hxc_Exception_thrownObject") in
				let e = Codegen.concat eassign (Codegen.concat epopassign (loop e)) in
				let e = mk (TBlock [e]) e.etype e.epos in
				if v.v_type == t_dynamic then begin
					def := Some e;
					None;
				end else
					Some ([Expr.mk_int ctx (get_type_id ctx v.v_type) e.epos],e)
			) cl) in
			mk (TSwitch(esubj,cl,!def)) e.etype e.epos
		| TPatMatch dt ->
 			let rec dtl d = match d with
				| DTGoto _ | DTExpr _ ->
					()
				| DTGuard(_,dt1,dt2) ->
					dtl dt1;
					(match dt2 with None -> () | Some dt -> dtl dt)
				| DTSwitch(_,cl) ->
					List.iter (fun (_,dt) -> dtl dt) cl
				| DTBind(bl,dt) ->
					List.iter (fun ((v,_),_) ->
						if v.v_name.[0] = '`' then v.v_name <- "_" ^ (String.sub v.v_name 1 (String.length v.v_name - 1));
						locals := v :: !locals
					) bl;
					dtl dt
			in
			Array.iter dtl dt.dt_dt_lookup;
			List.iter (fun (v,_) ->
				if v.v_name.[0] = '`' then v.v_name <- "_" ^ (String.sub v.v_name 1 (String.length v.v_name - 1));
				locals := v :: !locals
			) dt.dt_var_init;
			Type.map_expr loop e
		| TFor(v,e1,e2) ->
			let e1 = loop e1 in
			let ehasnext = mk (TField(e1,quick_field e1.etype "hasNext")) ctx.con.com.basic.tbool e1.epos in
			let enext = mk (TField(e1,quick_field e1.etype "next")) v.v_type e1.epos in
			let ebody = Codegen.concat enext e2 in
			mk (TBlock [
				mk (TVars [v,None]) ctx.con.com.basic.tvoid e1.epos;
				mk (TWhile(ehasnext,ebody,NormalWhile)) ctx.con.com.basic.tvoid e1.epos;
			]) ctx.con.com.basic.tvoid e.epos
		| _ -> Type.map_expr loop e
	in
	let e = match cf.cf_expr with
		| None -> None
		| Some ({ eexpr = TFunction(tf) } as e) when returns_type_param ->
			let var = fst (List.hd (List.rev tf.tf_args)) in
			force_add var;
			out_var := Some (var);
			Some (loop e)
		| Some e -> Some (loop e)
	in
	{
		field = cf;
		local_vars = !locals;
		expr = e;
		loop_stack = [];
	}

let generate_function_header ctx c cf =
	let args,ret,s = match follow cf.cf_type with
		| TFun(args,ret) -> args,ret,full_field_name c cf
		| TAbstract({a_path = ["c"],"Pointer"},[t]) ->
			begin match follow t with
				| TFun(args,ret) -> args,ret,"(*" ^ (full_field_name c cf) ^ ")"
				| _ -> assert false
			end
		| _ -> assert false
	in
	print ctx "%s %s(%s)" (s_type ctx ret) s (String.concat "," (List.map (fun (n,_,t) -> Printf.sprintf "%s %s" (s_type ctx t) n) args))

let get_typeref_forward ctx path =
	Printf.sprintf "extern const typeref %s__typeref" (path_to_name path)

let generate_typedef_declaration ctx t =
	let path = t_path t in
	if is_value_type ctx t then
		print ctx "const %s %s__default = { 0 }; //default" (s_type ctx t) (path_to_name path)
	else
		print ctx "const void* %s__default = NULL; //default" (path_to_name path);
	newline ctx;
	let nullval = Printf.sprintf "&%s__default" (path_to_name path) in
	Printf.sprintf "const typeref %s__typeref = { \"%s\", sizeof(%s), %s }; //typeref declaration" (path_to_name path) (s_type_path path) (s_type ctx t) nullval

let generate_method ctx c cf =
	ctx.fctx <- mk_function_context ctx cf;
	generate_function_header ctx c cf;
	match ctx.fctx.expr with
	| None -> newline ctx
	| Some {eexpr = TFunction ({tf_expr = {eexpr = TBlock el}; tf_type = t})} ->
		let el = match ctx.fctx.local_vars with
			| [] -> el
			| _ ->
				let einit = mk (TVars (List.map (fun v ->
					(* if there's a local var to a still unresolved type parameter, we must pre-allocate it *)
					match follow v.v_type with
					| TInst({ cl_kind = KTypeParameter _ },_) ->
						v, Some (Expr.mk_stack_tp_init ctx v.v_type cf.cf_pos)
					| _ ->
						v, None
				) ctx.fctx.local_vars)) ctx.con.com.basic.tvoid cf.cf_pos in
				einit :: el
		in
		let e = mk (TBlock el) t cf.cf_pos in
		generate_expr ctx e
	| _ -> assert false

(** change a function to receive its type parameters as arguments. *)
let change_parameter_function ctx cf vars =
	let tf_args, types = match vars, cf.cf_params with
		| _ :: _, _ -> (* vars *)
			List.map (fun (f,_) -> alloc_var f.cf_name f.cf_type, None) vars, List.map snd vars
		| _, _ :: _ ->
			List.map (fun (_,t) -> alloc_var (path_to_name (t_path t) ^ "_tp") (ctx.con.t_typeref t),None) cf.cf_params, List.map snd cf.cf_params
		| _ -> [],[]
	in
	match tf_args, cf.cf_type, cf.cf_expr with
	| [], TFun(_,ret), _ when not (is_type_param ctx ret) ->
		[]
	| _, TFun(args,ret), Some({ eexpr = TFunction(tf) } as e) ->
    (* if return type is a type parameter, add a reference to stack space as an argument as well *)
		let end_arg =
			if is_type_param ctx ret then [alloc_var "__out__" ret,None] else []
		in
		let mk_fun_type = List.map (fun (v,_) -> v.v_name,false,v.v_type) in
		let t = TFun(mk_fun_type tf_args @ args @ mk_fun_type end_arg,ret) in
		let e = { e with eexpr = TFunction({ tf with tf_args = tf_args @ tf.tf_args @ end_arg }); etype = t } in
		cf.cf_expr <- Some e;
		cf.cf_type <- t;
		List.map2 (fun t (v,_) -> (t_path t), v) types tf_args
	(* FIXME: handle conflicts when no cf_expr implementation is there *)
	| _ -> []

let mk_class_field name t public pos kind params =
	{
		cf_name = name;
		cf_type = t;
		cf_public = public;
		cf_pos = pos;
		cf_doc = None;
		cf_meta = [ Meta.CompilerGenerated, [], Ast.null_pos ]; (* annotate that this class field was generated by the compiler *)
		cf_kind = kind;
		cf_params = params;
		cf_expr = None;
		cf_overloads = [];
	}

let generate_class ctx c =
	let cls_parameter_vars () = match c.cl_types with
		| [] -> []
		| types ->
			let vars = List.map (fun (s,t) ->
				mk_class_field (path_to_name (t_path t) ^ "_tp") (ctx.con.t_typeref t) false c.cl_pos (Var {v_read = AccNormal; v_write = AccNormal}) []
			) types in
			c.cl_ordered_fields <- vars @ c.cl_ordered_fields;
			List.iter (fun f -> c.cl_fields <- PMap.add f.cf_name f c.cl_fields ) vars;
			List.map2 (fun v (_,t) -> v,t) vars types
	in
	let old_tparams = ctx.con.type_parameters in
	let param_vars = if c.cl_path <> ([],"typeref") then cls_parameter_vars () else [] in
	let mk_this_field f =
		{ eexpr = TField({ eexpr = TConst TThis; etype = monofy_class c; epos = f.cf_pos}, FInstance(c,f)); etype = f.cf_type; epos = f.cf_pos }
	in
	ctx.con.type_parameters <- List.map (fun (f,t) ->
		t_path t, mk_this_field f) param_vars;

	let vars = DynArray.create () in
	let svars = DynArray.create () in
	let methods = DynArray.create () in

	let add_init e = match c.cl_init with
		| None -> c.cl_init <- Some e
		| Some e2 -> c.cl_init <- Some (Codegen.concat e2 e)
	in

	let check_dynamic cf = match cf.cf_kind with
		| Method MethDynamic ->
			let cf2 = {cf with cf_name = cf.cf_name ^ "_hx_impl" } in
			DynArray.add methods cf2;
			cf.cf_expr <- None;
			cf.cf_type <- ctx.con.t_pointer cf.cf_type;
			add_init (Expr.mk_ccode ctx (Printf.sprintf "%s = %s" (full_field_name c cf) (full_field_name c cf2)));
		| _ ->
			()
	in

	(* split fields into member vars, static vars and functions *)
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ -> DynArray.add vars cf
		| Method m -> match cf.cf_type with
			| TFun(args,ret) ->
				cf.cf_type <- TFun(("this",false,monofy_class c) :: args, ret);
				check_dynamic cf;
				DynArray.add methods cf
			| _ ->
				assert false;
	) c.cl_ordered_fields;
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ -> DynArray.add svars cf
		| Method _ ->
			check_dynamic cf;
			DynArray.add methods cf
	) c.cl_ordered_statics;

	(* add constructor as function *)
	begin match c.cl_constructor with
		| None -> ()
		| Some cf ->
			let func_params = change_parameter_function ctx cf param_vars in
			match follow cf.cf_type, cf.cf_expr with
			| TFun(args,_), Some e ->
				let path = path_to_name c.cl_path in
				let einit =
					(if is_value_type ctx (TInst(c,List.map snd c.cl_types)) then
						Expr.mk_ccode ctx (Printf.sprintf "%s this" path)
					else
  					Expr.mk_ccode ctx (Printf.sprintf "%s* this = (%s*) malloc(sizeof(%s))" path path path)) ::
					List.map2 (fun (f,_) (_,v) -> {
						eexpr = TBinop(
							Ast.OpAssign,
							mk_this_field f,
							{ eexpr = TLocal v; etype = v.v_type; epos = f.cf_pos });
						etype = v.v_type;
						epos = f.cf_pos
					}) param_vars func_params
				in
				let ereturn = Expr.mk_ccode ctx "return this" in
				let e = match e.eexpr with
					| TFunction({tf_expr = ({eexpr = TBlock el } as ef) } as tf) ->
						{e with eexpr = TFunction ({tf with tf_expr = {ef with eexpr = TBlock(einit @ el @ [ereturn])}})}
					| _ -> assert false
				in
				cf.cf_expr <- Some e;
				cf.cf_type <- TFun(args, monofy_class c);
				DynArray.add methods cf
			| _ -> ()
	end;

	ctx.buf <- ctx.buf_c;

	spr ctx (generate_typedef_declaration ctx (TInst(c,List.map snd c.cl_types)));
	newline ctx;

	(* generate static vars *)
	if not (DynArray.empty svars) then begin
		spr ctx "// static vars\n";
		DynArray.iter (fun cf ->
			print ctx "%s %s" (s_type ctx cf.cf_type) (full_field_name c cf);
			newline ctx;
			match cf.cf_expr with
				| None -> ()
				| Some e ->
					let fctx = mk_function_context ctx cf in
					let e = Option.get fctx.expr in
					let locals = fctx.local_vars in
					let einit = mk (TVars (List.map (fun v -> v,None) locals)) ctx.con.com.basic.tvoid cf.cf_pos in
					let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
					let ethis = mk (TTypeExpr (TClassDecl c)) ta cf.cf_pos in
					let efield = Codegen.field ethis cf.cf_name cf.cf_type cf.cf_pos in
					let eassign = mk (TBinop(OpAssign,efield,e)) efield.etype cf.cf_pos in
					let e = Codegen.concat einit eassign in
					cf.cf_expr <- Some e;
					add_init e;
		) svars;
	end;

	(* add init field as function *)
	begin match c.cl_init with
		| None -> ()
		| Some e ->
			ctx.con.init_modules <- c.cl_path :: ctx.con.init_modules;
			let t = tfun [] ctx.con.com.basic.tvoid in
			let f = mk_field "_hx_init" t c.cl_pos in
			let tf = {
				tf_args = [];
				tf_type = ctx.con.com.basic.tvoid;
				tf_expr = mk_block e;
			} in
			f.cf_expr <- Some (mk (TFunction tf) t c.cl_pos);
			DynArray.add methods f
	end;

	(* generate function implementations *)
	if not (DynArray.empty methods) then begin
		DynArray.iter (fun cf ->
			let old_tparams = ctx.con.type_parameters in
			let params = change_parameter_function ctx cf [] in
			let params = List.map (fun (p,v) -> p,mk (TLocal v) v.v_type cf.cf_pos) params in

			ctx.con.type_parameters <- params @ old_tparams;
			generate_method ctx c cf;
			ctx.con.type_parameters <- old_tparams
		) methods;
	end;

	(* check if we have the main class *)
	(match ctx.con.com.main_class with
	| Some path when path = c.cl_path ->
		ctx.dependencies <- PMap.add ([],"setjmp") false ctx.dependencies;
		add_dependency ctx (["c";"hxc"],"Exception");
		add_dependency ctx (["hxc"],"Init");
		print ctx "int main() {\n\t_hx_init();\n\tswitch(setjmp(*c_hxc_Exception_push())) {\n\t\tcase 0: %s();break;\n\t\tdefault: printf(\"Something went wrong\");\n\t}\n}" (full_field_name c (PMap.find "main" c.cl_statics))
	| _ -> ());

	ctx.buf <- ctx.buf_h;

	(* generate member struct *)
	if not (DynArray.empty vars) then begin
		spr ctx "// member var structure\n";
		print ctx "typedef struct %s {" (path_to_name c.cl_path);
		let b = open_block ctx in
		DynArray.iter (fun cf ->
			newline ctx;
			print ctx "%s %s" (s_type ctx cf.cf_type) cf.cf_name;
		) vars;
		b();
		newline ctx;
		print ctx "} %s" (path_to_name c.cl_path);
		newline ctx;
		spr ctx "\n";
	end else begin
		print ctx "typedef struct %s { void* dummy; } %s" (path_to_name c.cl_path) (path_to_name c.cl_path);
		newline ctx;
	end;

	(* generate static vars *)
	if not (DynArray.empty svars) then begin
		spr ctx "// static vars\n";
		DynArray.iter (fun cf ->
      print ctx "extern %s %s" (s_type ctx cf.cf_type) (full_field_name c cf);
      newline ctx
    ) svars
	end;

	(* generate forward declarations of functions *)
	if not (DynArray.empty methods) then begin
		spr ctx "// forward declarations\n";
		DynArray.iter (fun cf ->
			generate_function_header ctx c cf;
			newline ctx;
		) methods;
	end;

	add_dependency ctx ([],"typeref");
	spr ctx (get_typeref_forward ctx c.cl_path);
	newline ctx;

	(* restore type parameters stack *)
	ctx.con.type_parameters <- old_tparams

let generate_enum ctx en =
	ctx.buf <- ctx.buf_h;
	add_dependency ctx ([],"typeref");
	spr ctx (get_typeref_forward ctx en.e_path);
	newline ctx;

	let ctors = List.map (fun s -> PMap.find s en.e_constrs) en.e_names in
	let path = path_to_name en.e_path in

	(* forward declare enum type *)
	print ctx "typedef struct %s %s" path path;
	newline ctx;

	(* generate constructor types *)
	spr ctx "// constructor structure";
	let ctors = List.map (fun ef ->
		newline ctx;
		match follow ef.ef_type with
		| TFun(args,_) ->
			let name = full_enum_field_name en ef in
			print ctx "typedef struct %s {" name;
			let b = open_block ctx in
			List.iter (fun (n,_,t) ->
				newline ctx;
				print ctx "%s %s" (s_type ctx t) n;
			) args;
			b();
			newline ctx;
			print ctx "} %s" name;
			ef
		| _ ->
			print ctx "typedef void* %s" (full_enum_field_name en ef);
			{ ef with ef_type = TFun([],ef.ef_type)}
	) ctors in

	(* generate enum type *)
	newline ctx;
	spr ctx "// enum structure";
	newline ctx;
	print ctx "typedef struct %s{" path;
	let b = open_block ctx in
	newline ctx;
	spr ctx "int index";
	newline ctx;
	spr ctx "union {";
	let b2 = open_block ctx in
	List.iter (fun ef ->
		newline ctx;
		print ctx "%s %s" (full_enum_field_name en ef) ef.ef_name
	) ctors;
	b2();
	newline ctx;
	spr ctx "} args";
	b();
	newline ctx;
	print ctx "} %s" (path_to_name en.e_path);
	newline ctx;

	spr ctx "// constructor forward declarations";
	List.iter (fun ef ->
		newline ctx;
		match ef.ef_type with
		| TFun(args,ret) ->
			print ctx "%s new_%s(%s)" (s_type ctx ret) (full_enum_field_name en ef) (String.concat "," (List.map (fun (n,_,t) -> Printf.sprintf "%s %s" (s_type ctx t) n) args));
		| _ ->
			assert false
	) ctors;
	newline ctx;

	ctx.buf <- ctx.buf_c;
	spr ctx (generate_typedef_declaration ctx (TEnum(en,List.map snd en.e_types)));
	newline ctx;

	(* generate constructor functions *)
	spr ctx "// constructor functions";
	List.iter (fun ef ->
		newline ctx;
		match ef.ef_type with
		| TFun(args,ret) ->
			print ctx "%s new_%s(%s) {" (s_type ctx ret) (full_enum_field_name en ef) (String.concat "," (List.map (fun (n,_,t) -> Printf.sprintf "%s %s" (s_type ctx t) n) args));
			let b = open_block ctx in
			newline ctx;
			print ctx "%s* this = (%s*) malloc(sizeof(%s))" path path path;
			newline ctx ;
			print ctx "this->index = %i" ef.ef_index;
			List.iter (fun (n,_,_) ->
				newline ctx;
				print ctx "this->args.%s.%s = %s" ef.ef_name n n;
			) args;
			newline ctx;
			spr ctx "return this";
			b();
			newline ctx;
			spr ctx "}"
		| _ ->
			assert false
	) ctors

let generate_type con mt = match mt with
	| TClassDecl c when not c.cl_extern ->
		let ctx = mk_type_context con c.cl_path in
		generate_class ctx c;
		close_type_context ctx;
	| TEnumDecl en when not en.e_extern ->
		let ctx = mk_type_context con en.e_path in
		generate_enum ctx en;
		close_type_context ctx;
	| TAbstractDecl { a_path = [],"Void" } -> ()
	| TAbstractDecl a ->
		let ctx = mk_type_context con a.a_path in
		ctx.buf <- ctx.buf_c;
		spr ctx (generate_typedef_declaration ctx (TAbstract(a,List.map snd a.a_types)));
		newline ctx;
		ctx.buf <- ctx.buf_h;
		add_dependency ctx ([],"typeref");
		spr ctx (get_typeref_forward ctx a.a_path);
		newline ctx;
		close_type_context ctx;
	| _ ->
		()

let generate_anon_file con =
	let ctx = mk_type_context con (["hxc"],"AnonTypes") in

	spr ctx "// forward declarations";
	PMap.iter (fun _ (s,_) ->
		newline ctx;
		print ctx "typedef struct %s %s" s s;
	) con.anon_types;
	newline ctx;

	spr ctx "// structures";
	PMap.iter (fun _ (s,cfl) ->
		newline ctx;
		print ctx "typedef struct %s {" s;
		let b = open_block ctx in
		List.iter (fun cf ->
			newline ctx;
			print ctx "%s %s" (s_type ctx cf.cf_type) cf.cf_name;
		) cfl;
		b();
		newline ctx;
		print ctx "} %s" s;
	) con.anon_types;
	newline ctx;
	spr ctx "// constructor forward declarations";
	PMap.iter (fun _ (s,cfl) ->
		newline ctx;
		print ctx "%s* new_%s(%s)" s s (String.concat "," (List.map (fun cf -> Printf.sprintf "%s %s" (s_type ctx cf.cf_type) cf.cf_name) cfl));
	) con.anon_types;
	newline ctx;

	ctx.buf <- ctx.buf_c;

	spr ctx "// constructor definitions";
	PMap.iter (fun _ (s,cfl) ->
		newline ctx;
		print ctx "%s* new_%s(%s) {" s s (String.concat "," (List.map (fun cf -> Printf.sprintf "%s %s" (s_type ctx cf.cf_type) cf.cf_name) cfl));
		let b = open_block ctx in
		newline ctx;
		print ctx "%s* this = (%s*) malloc(sizeof(%s))" s s s;
		List.iter (fun cf ->
			newline ctx;
			print ctx "this->%s = %s" cf.cf_name cf.cf_name;
		) cfl;
		newline ctx;
		spr ctx "return this";
		b();
		newline ctx;
		spr ctx "}"
	) con.anon_types;
	close_type_context ctx

let generate_init_file con =
	let ctx = mk_type_context con (["hxc"],"Init") in
	ctx.buf <- ctx.buf_c;
	print ctx "void _hx_init() {";
	let b = open_block ctx in
	List.iter (fun path ->
		add_dependency ctx path;
		newline ctx;
		print ctx "%s__hx_init()" (path_to_name path);
	) con.init_modules;
	b();
	newline ctx;
	spr ctx "}";
	close_type_context ctx

let generate_make_file con =
	let relpath ctx path = path_to_file_path path in
	let main_name = match con.com.main_class with Some path -> snd path | None -> "main" in
	let ch = open_out_bin (con.com.file ^ "/Makefile") in
	output_string ch ("OUT = " ^ main_name ^ "\n");
	output_string ch "ifndef MSVC\n";
	output_string ch ("\tOUTFLAG := -o \n");
	output_string ch ("else\n");
	output_string ch ("\tOUTFLAG := /Fo\n");
	output_string ch ("\tCC := cl.exe\n");
	output_string ch ("endif\n");
	output_string ch ("all: $(OUT)\n");
	List.iter (fun ctx ->
		output_string ch (Printf.sprintf "%s.o: " (relpath ctx ctx.type_path));
		PMap.iter (fun path b -> if b then output_string ch (Printf.sprintf "%s.h " (relpath ctx path)) ) ctx.dependencies;
		output_string ch (Printf.sprintf "\n\t$(CC) $(CFLAGS) $(INCLUDES) $(OUTFLAG)%s.o -c %s.c\n\n" (relpath ctx ctx.type_path) (relpath ctx ctx.type_path))
	) con.generated_types;
	output_string ch "OBJECTS = ";
	List.iter (fun ctx -> output_string ch (Printf.sprintf "%s.o " (relpath ctx ctx.type_path))) con.generated_types;
	output_string ch "\n\n$(OUT): $(OBJECTS)";
	output_string ch "\n\t$(CC) $(CFLAGS) $(INCLUDES) $(OBJECTS) -o $(OUT) $(LDFLAGS)\n";
	output_string ch "\n\nclean:\n\t$(RM) $(OUT) $(OBJECTS)";
	close_out ch

let generate_hxc_files con =
	generate_anon_file con;
	generate_init_file con;
	generate_make_file con

let get_type com path = try
	match List.find (fun md -> Type.t_path md = path) com.types with
	| TClassDecl c -> TInst(c, List.map snd c.cl_types)
	| TEnumDecl e -> TEnum(e, List.map snd e.e_types)
	| TTypeDecl t -> TType(t, List.map snd t.t_types)
	| TAbstractDecl a -> TAbstract(a, List.map snd a.a_types)
with | Not_found ->
	failwith("The type " ^ Ast.s_type_path path ^ " is required and was not found")

let generate com =
	let t_typeref = get_type com ([],"typeref") in
	let t_pointer = get_type com (["c"],"Pointer") in
	let con = {
		com = com;
		cvar = alloc_var "__c" t_dynamic;
		num_temp_funcs = 0;
		num_labels = 0;
		num_anon_types = -1;
		(* this has to start at 0 so the first type id is 1 *)
		num_identified_types = 0;
		anon_types = PMap.empty;
		type_ids = PMap.empty;
		type_parameters = [];
		init_modules = [];
		generated_types = [];
		t_typeref = (match follow t_typeref with
			| TInst(c,_) -> fun t -> TInst(c,[t])
			| _ -> assert false);
		t_pointer = (match follow t_pointer with
			| TAbstract(a,_) -> fun t -> TAbstract(a,[t])
			| _ -> assert false);
	} in
	List.iter (generate_type con) com.types;
	generate_hxc_files con
