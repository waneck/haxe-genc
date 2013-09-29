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

type dependency_type =
	| DFull
	| DForward
	| DCStd

type function_context = {
	field : tclass_field;
	mutable loop_stack : string option list;
}

type hxc = {
	t_typeref : t -> t;
	t_pointer : t -> t;
	t_const_pointer : t -> t;
	t_func_pointer : t -> t;
	t_int64 : t -> t;
	t_jmp_buf : t;

	c_lib : tclass;
	c_boot : tclass;
	c_string : tclass;
	c_array : tclass;
	c_fixed_array : tclass;
	c_exception : tclass;
	c_cstring : tclass;
	c_csetjmp : tclass;
	c_cstdlib : tclass;
	c_bool    : tabstract;

	cf_deref : tclass_field;
	cf_addressof : tclass_field;
	cf_sizeof : tclass_field;
}

type context = {
	com : Common.context;
	cvar : tvar;
	hxc : hxc;
	mutable num_temp_funcs : int;
	mutable num_labels : int;
	mutable num_anon_types : int;
	mutable num_identified_types : int;
	mutable anon_types : (string,string * tclass_field list) PMap.t;
	mutable type_ids : (string,int) PMap.t;
	mutable type_parameters : (path, texpr) PMap.t;
	mutable init_modules : path list;
	mutable generated_types : type_context list;
	mutable filters : filter list;
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
	mutable dependencies : (path,dependency_type) PMap.t;
}

and gen_context = {
	gcom : Common.context;
	gcon : context;
	mutable gfield : tclass_field;
	mutable mtype : module_type option;
	(* call this function instead of Type.map_expr () *)
	mutable map : texpr -> texpr;
	(* tvar_decl -> unit; declares a variable on the current block *)
	mutable declare_var : (tvar * texpr option) -> unit;
	mutable declare_temp : t -> texpr option -> tvar;
	(* runs a filter on the specified class field *)
	mutable run_filter : tclass_field -> unit;
	(* adds a field to the specified class *)
	mutable add_field : tclass -> tclass_field -> bool -> unit;
	(* delays to run after all filters are done *)
	mutable delays : (unit -> unit) list;
}

and filter = gen_context->(texpr->texpr)

let rec follow t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow t
		| _ -> t)
	| TLazy f ->
		follow (!f())
	| TType (t,tl) ->
		follow (apply_params t.t_types tl t.t_type)
	| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
		follow (Codegen.Abstract.get_underlying_type a pl)
	| _ -> t

module Expr = struct

	let t_path t = match follow t with
		| TInst(c,_) -> c.cl_path
		| TEnum(e,_) -> e.e_path
		| TAbstract(a,_) -> a.a_path
		| _ -> [],"Dynamic"

	let mk_static_field c cf p =
		let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
		let ethis = mk (TTypeExpr (TClassDecl c)) ta p in
		let t = monomorphs cf.cf_params cf.cf_type in
		mk (TField (ethis,(FStatic (c,cf)))) t p

	let mk_static_call c cf el p =
		let ef = mk_static_field c cf p in
		let tr = match follow ef.etype with
			| TFun(args,tr) ->
				List.iter2 (fun (_,_,t) e -> Type.unify e.etype t) args el;
				tr
			| _ -> assert false
		in
		mk (TCall(ef,el)) tr p

	let mk_static_field_2 c n p =
		mk_static_field c (PMap.find n c.cl_statics) p

	let mk_static_call_2 c n el p =
		mk_static_call c (PMap.find n c.cl_statics) el p

	let mk_local v p =
		{ eexpr = TLocal v; etype = v.v_type; epos = p }

	let mk_comma_block con p el =
		let t = match List.rev el with
			| [] -> con.com.basic.tvoid
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

	let mk_cast t e =
		{ e with eexpr = TCast(e, None); etype = t }

	let mk_ref con p e =
		mk_static_call con.hxc.c_lib con.hxc.cf_addressof [e] p

	let mk_deref con p e =
		mk_static_call con.hxc.c_lib con.hxc.cf_deref [e] p

	let mk_sizeof con p e =
		mk_static_call con.hxc.c_lib con.hxc.cf_sizeof [e] p

	let mk_assign_ref con p local value =
		mk_comma_block con p [
			{
				eexpr = TBinop(Ast.OpAssign, local, value);
				etype = value.etype;
				epos = p;
			};
			mk_ref con p local
		]

	let mk_type_param con pos t =
		let t = con.hxc.t_typeref t in
		let c,p = match follow t with
			| TInst(c,p) -> c,p
			| _ -> assert false
		in
		{ eexpr = TNew(c,p,[]); etype = t; epos = pos }

	let mk_null_type_param con pos t =
		let t = con.hxc.t_typeref t in
		let c,p = match follow t with
			| TInst(c,p) -> c,p
			| _ -> assert false
		in
		let typeref = { eexpr = TNew(c,p,[]); etype = t; epos = pos } in
		{ eexpr = TField(typeref, FInstance(c,PMap.find "nullval" c.cl_fields)); etype = t; epos = pos }


	let mk_stack_tp_init con t p =
		let e = mk_static_call_2 con.hxc.c_lib "alloca" [mk_sizeof con p (mk_type_param con p t)] p in
		{e with etype = t}

	let mk_ccode ctx s =
		mk (TCall ((mk (TLocal ctx.con.cvar) t_dynamic Ast.null_pos), [mk (TConst (TString s)) t_dynamic Ast.null_pos])) t_dynamic Ast.null_pos

	let mk_int com i p =
		mk (TConst (TInt (Int32.of_int i))) com.basic.tint p

	let debug ctx e =
		Printf.sprintf "%s: %s" ctx.fctx.field.cf_name (s_expr (s_type (print_context())) e)

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

	let mk_binop op e1 e2 et p =
		{ eexpr=TBinop(op,e1,e2); etype=et; epos=p }

	let mk_obj_decl fields p =
		let fields = List.sort compare fields in
		let t_fields = List.fold_left (fun acc (n,e) ->
			let cf = mk_class_field n e.etype true e.epos (Var {v_read = AccNormal; v_write = AccNormal}) [] in
			PMap.add n cf acc
		) PMap.empty fields in
		let t = TAnon {a_fields = t_fields; a_status = ref Closed} in
		mk (TObjectDecl fields) t p

	let insert_expr e once f =
		let el = match e.eexpr with TBlock el -> el | _ -> [e] in
		let el,found = List.fold_left (fun (acc,found) e ->
			match f e with
			| Some e1 when not once || not found -> e :: e1 :: acc,true
			| _ -> e :: acc,found
		) ([],false) el in
		mk (TBlock (List.rev el)) e.etype e.epos,found
end

let path_to_name (pack,name) = match pack with [] -> name | _ -> String.concat "_" pack ^ "_" ^ name

let get_type_id con t =
	let id = Type.s_type (print_context()) (follow t) in
	try
		PMap.find id con.type_ids
	with Not_found ->
		con.num_identified_types <- con.num_identified_types + 1;
		con.type_ids <- PMap.add id con.num_identified_types con.type_ids;
		con.num_identified_types

let wrap_function ethis efunc =
	let efunc = mk (TCast(efunc,None)) t_dynamic efunc.epos in
	let e = Expr.mk_obj_decl ["_this",ethis;"_func",efunc] efunc.epos in
	let e = mk (TCast(e,None)) efunc.etype e.epos in
	mk (TMeta((Meta.Custom ":closureWrap",[],e.epos),e)) e.etype e.epos

let wrap_static_function efunc =
	wrap_function (mk (TConst TNull) (mk_mono()) efunc.epos) efunc

module Filters = struct

	let add_filter con filter =
		con.filters <- filter :: con.filters

	let run_filters gen e =
		(* local vars / temp vars handling *)
		let declared_vars = ref [] in
		let temp_var_counter = ref (-1) in

		(* temporary var handling *)
		let old_declare = gen.declare_var in
		gen.declare_var <- (fun (tvar,eopt) ->
			declared_vars := (tvar,eopt) :: !declared_vars;
		);
		gen.declare_temp <- (fun t eopt ->
			incr temp_var_counter;
			let v = alloc_var ("_tmp" ^ (string_of_int !temp_var_counter)) t in
			gen.declare_var (v,eopt);
			v
		);

		let ret = List.fold_left (fun e f ->
			let run = f gen in
			let process_next_block = ref true in
			(* set all temps as used, as we cannot guarantee for now the availability of a var *)
			let rec map e = match e.eexpr with
				| TMeta( (Meta.Comma,_,_), { eexpr = TBlock(el) } ) ->
					process_next_block := false;
					let ret = run e in
					process_next_block := true;
					ret
				| TBlock(el) when !process_next_block ->
					let old_declared = !declared_vars in
					declared_vars := [];
					(* run loop *)
					let el = match (mk_block (run e)).eexpr with
						| TBlock el -> el
						| _ -> assert false
					in
					(* change loop with new declared vars *)
					let el = match !declared_vars with
						| [] -> el
						| vars ->
							{ eexpr = TVars(List.rev vars); etype = gen.gcom.basic.tvoid; epos = e.epos } :: el
					in
					let ret = { e with eexpr = TBlock(el) } in
					declared_vars := old_declared;
					ret
				| TBlock _ ->
					process_next_block := true;
					run e
				| _ -> run e
			in

			let last_map = gen.map in
			gen.map <- map;
			let ret = map e in
			gen.map <- last_map;
			ret
		) e gen.gcon.filters in
		gen.declare_var <- old_declare;
		ret

	let run_filters_field gen cf =
		gen.gfield <- cf;
		match cf.cf_expr with
		| None -> ()
		| Some e ->
			cf.cf_expr <- Some (run_filters gen e)

	let mk_gen_context con =
		let gen = {
			gcom = con.com;
			gcon = con;
			gfield = null_field;
			mtype = None;
			map = (function _ -> assert false);
			declare_var = (fun _ -> assert false);
			declare_temp = (fun _ _ -> assert false);
			run_filter = (fun _ -> assert false);
			add_field = (fun c cf stat ->
				if stat then
					c.cl_ordered_statics <- cf :: c.cl_ordered_statics
				else
					c.cl_ordered_fields <- cf :: c.cl_ordered_fields);
			delays = [];
		} in
		gen

	let initialize_class con c =
		let add_init e = match c.cl_init with
			| None -> c.cl_init <- Some e
			| Some e2 -> c.cl_init <- Some (Codegen.concat e2 e)
		in
		let check_dynamic cf stat = match cf.cf_kind with
			| Method MethDynamic ->
				(* create implementation field *)
				let cf2 = {cf with cf_name = cf.cf_name ^ "_hx_impl" } in
				if stat then begin
					c.cl_ordered_statics <- cf2 :: c.cl_ordered_statics;
					c.cl_statics <- PMap.add cf2.cf_name cf2 c.cl_statics;
					let ef1 = Expr.mk_static_field c cf cf.cf_pos in
					let ef2 = Expr.mk_static_field c cf2 cf2.cf_pos in
					let ef2 = wrap_static_function ef2 in
					add_init (Codegen.binop OpAssign ef1 ef2 ef1.etype ef1.epos);
				end else begin
					c.cl_ordered_fields <- cf2 :: c.cl_ordered_fields;
					c.cl_fields <- PMap.add cf2.cf_name cf2 c.cl_fields
				end;
				cf.cf_expr <- None;
				cf.cf_kind <- Var {v_read = AccNormal; v_write = AccNormal};
			| _ ->
				()
		in

		List.iter (fun cf -> match cf.cf_kind with
			| Var _ -> ()
			| Method m -> match cf.cf_type with
				| TFun(args,ret) -> check_dynamic cf false;
				| _ -> assert false;
		) c.cl_ordered_fields;

		List.iter (fun cf -> match cf.cf_kind with
			| Var _ ->
				begin match cf.cf_expr with
					| None -> ()
					| Some e ->
						(* add static var initialization to cl_init *)
						let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
						let ethis = mk (TTypeExpr (TClassDecl c)) ta cf.cf_pos in
						let efield = Codegen.field ethis cf.cf_name cf.cf_type cf.cf_pos in
						let eassign = mk (TBinop(OpAssign,efield,e)) efield.etype cf.cf_pos in
						cf.cf_expr <- Some eassign;
						add_init eassign;
				end
			| Method _ -> check_dynamic cf true;
		) c.cl_ordered_statics;

		(* check if we have the main class *)
		begin match con.com.main_class with
			| Some path when path = c.cl_path ->
				let efield = Expr.mk_static_field_2 con.hxc.c_boot "mainFunc" c.cl_pos in
				let efield2 = Expr.mk_static_field_2 c "main" c.cl_pos in
				let eassign = mk (TBinop(OpAssign,efield,efield2)) efield.etype c.cl_pos in
				add_init eassign
			| _ -> ()
		end

	let run_filters_types con =
		let gen = mk_gen_context con in
		List.iter (fun md -> match md with
			| TClassDecl c ->
				gen.mtype <- Some md;
				initialize_class con c;
				let added = ref [] in
				let old_run_filter = gen.run_filter in
				gen.run_filter <- (fun cf ->
					added := cf :: !added);

				let fields = c.cl_ordered_fields in
				let statics = c.cl_ordered_statics in
				Option.may (run_filters_field gen) c.cl_constructor;
				List.iter (run_filters_field gen) fields;
				List.iter (run_filters_field gen) statics;
				gen.gfield <- null_field;
				c.cl_init <- Option.map (run_filters gen) c.cl_init;

				(* run all added fields *)
				let rec loop () = match !added with
					| [] -> ()
					| hd :: tl ->
						added := tl;
						run_filters_field gen hd;
						loop ()
				in
				loop();
				gen.run_filter <- old_run_filter
			| _ -> ()
		) con.com.types;
		gen

end

(* filters *)
(** TypeParams **)
(**
	Handles type parameters in a way that completely avoids stack allocation on platforms that support alloca.
	The trick is to always pass a new TypeReference<T>() on each callsite, so we can always have a descriptor
	for the concrete type parameter's type. With this descriptor, we know its reference size, and thus we can handle
	array operations and operations on the type.
	We also need to update callsites so they pass the concrete variables, from type parameters by reference,
	and functions that return a type parameter reference must have an extra "out" parameter, which will take a pointer to
	an address from the caller's stack that has sufficient space to copy the needed type

	dependencies:
		This filter will perform significant changes on the callsite of each function. For this reason,
		it's best to run at min_dep, so it's the last filter running. So all filters before it will run without the
		callsite changes
    Any filter ran after it should not create temporary type parameter variables, as they will not be handled properly
**)
module TypeParams = struct

	let infer_params gen p original_t applied_t params = match params with
	| [] -> []
	| _ ->
		let monos = List.map (fun _ -> mk_mono()) params in
		let original = apply_params params monos original_t in
		(try
			unify applied_t original
		with | Unify_error _ ->
			gen.gcom.warning "This expression may be invalid" p
		);
		monos

	let is_type_param con t = match follow t with
		| TInst({ cl_kind = KTypeParameter _ },_) -> true
		| _ -> false

	let function_has_type_parameter con t = match follow t with
		| TFun(args,ret) -> is_type_param con ret || List.exists (fun (_,_,t) -> is_type_param con t) args
		| _ -> false

	let is_field_type_param con e = is_type_param con e.etype || match e.eexpr with
		| TField(_, (FInstance(_,cf) | FStatic(_,cf))) when is_type_param con cf.cf_type -> true
		| _ -> false

	let get_param_args gen e cf e1 ef = match cf.cf_params with
		| [] -> []
		| _ ->
	let cft = match follow ef.etype with
		| TInst(c,_) ->
			apply_params c.cl_types (List.map (fun _ -> mk_mono()) c.cl_types) cf.cf_type
		(* | TAbstract(a,_) -> apply_params a.a_types p e1.etype *)
		(* | TEnum(e,_) -> apply_params e.e_types p e1.etype *)
		| _ ->
			cf.cf_type
	in
			let params = infer_params gen e.epos cft e1.etype cf.cf_params in
			List.map (Expr.mk_type_param gen.gcon e.epos) params

	let get_fun t = match follow t with | TFun(r1,r2) -> (r1,r2) | _ -> assert false

	let type_parameter_expr con p path = try
		PMap.find path con.type_parameters
	with | Not_found ->
		con.com.warning ("Cannot find type parameter called " ^ s_type_path path) p;
		null t_dynamic p

	let type_param_name t =
		let path = Expr.t_path t in
		Printf.sprintf "%s_%s_tp" (String.concat "_" (fst path)) (snd path)

	(** change a class to receive its type parameter vars **)
	let cls_parameter_vars gen c = match c.cl_types with
	| [] -> ()
	| types ->
		let vars = List.map (fun (s,t) ->
			let f = Expr.mk_class_field (type_param_name t) (gen.gcon.hxc.t_typeref t) false c.cl_pos (Var {v_read = AccNormal; v_write = AccNormal}) [] in
			let key = Expr.t_path t in
			let value = {
				eexpr = TField({ eexpr = TConst TThis; etype = TInst(c, List.map snd c.cl_types); epos = c.cl_pos }, FInstance(c,f));
				etype = f.cf_type;
				epos = f.cf_pos;
			} in
			gen.gcon.type_parameters <- PMap.add key value gen.gcon.type_parameters;
			f
		) types in
		(* delay *)
		let delay () =
			c.cl_ordered_fields <- vars @ c.cl_ordered_fields;
			List.iter (fun f -> c.cl_fields <- PMap.add f.cf_name f c.cl_fields) vars
		in
		gen.delays <- delay :: gen.delays

	(** change a function to receive its type parameters as arguments. *)
	let change_parameter_function gen cf =
		let tf_args, types, is_ctor = match cf.cf_name, cf.cf_params with
			| _, _ :: _ ->
				List.map (fun (_,t) -> alloc_var (type_param_name t) (gen.gcon.hxc.t_typeref t),None) cf.cf_params, List.map snd cf.cf_params, false
			(* constructors have special treatment *)
			| "new", _ -> (match gen.mtype with
				| Some (TClassDecl c) ->
					List.map (fun (_,t) -> alloc_var (type_param_name t) (gen.gcon.hxc.t_typeref t), None) c.cl_types, List.map snd c.cl_types, true
				| _ -> [],[],false)
			| _ -> [],[],false
		in
		match tf_args, cf.cf_type with
		| [], TFun(_,ret) when not (is_type_param gen.gcon ret) ->
			None
		| _, TFun(args,ret) ->
			(* if return type is a type parameter, add a reference to stack space as an argument as well *)
			let end_arg =
				if is_type_param gen.gcon ret then [alloc_var "__out__" ret,None] else []
			in
			if not is_ctor then
				List.iter2 (fun t (v,_) ->
					gen.gcon.type_parameters <- PMap.add (Expr.t_path t) (Expr.mk_local v cf.cf_pos) gen.gcon.type_parameters
				) types tf_args;
			let delay () =
				let mk_fun_type = List.map (fun (v,_) -> v.v_name,false,v.v_type) in
				let t = TFun(mk_fun_type tf_args @ args @ mk_fun_type end_arg,ret) in
				(match cf.cf_expr with
				| Some ({ eexpr = TFunction(tf) } as e) ->
					let added_exprs = if is_ctor then
						List.map2 (fun (v,_) t -> {
							eexpr = TBinop(
								Ast.OpAssign,
								type_parameter_expr gen.gcon cf.cf_pos (Expr.t_path t),
								{ eexpr = TLocal v; etype = v.v_type; epos = cf.cf_pos });
							etype = v.v_type;
							epos = cf.cf_pos
						}) tf_args types
					else
						[]
					in

			let rec loop acc el = match el with
				| [] -> (List.rev acc) @ added_exprs
				| ({ eexpr = TVars _ } as e) :: el -> loop (e :: acc) el
				| _ -> (List.rev acc) @ added_exprs @ el
			in
			let bl = match mk_block tf.tf_expr with
				| { eexpr = TBlock(bl) } -> bl
				| _ -> assert false
			in

					cf.cf_expr <- Some { e with
						eexpr = TFunction({ tf with
							tf_args = tf_args @ tf.tf_args @ end_arg;
              tf_expr = { tf.tf_expr with eexpr = TBlock (loop [] bl) };
						});
						etype = t;
					}
				| _ -> ());
				cf.cf_type <- t
			in
			gen.delays <- delay :: gen.delays;
			(match end_arg with
			| [v,_] -> Some v
			| _ -> None)
		(* FIXME: handle conflicts when no cf_expr implementation is there *)
		| _ -> None

	let filter gen = match gen.mtype with
	| Some (TClassDecl { cl_path = [],"typeref" }) -> (fun e -> e)
	| _ ->
		let cf = gen.gfield in
		(* check current class type parameter *)
		(match gen.mtype with
		| Some (TClassDecl ({ cl_types = (_,t) :: _ } as c)) when not (PMap.mem (Expr.t_path t) gen.gcon.type_parameters) ->
			cls_parameter_vars gen c
		| _ -> ());
		let out_var = change_parameter_function gen cf in
		let temp_num = ref 0 in
		let get_temp t pos =
			let init = match follow t with
				| TInst({ cl_kind = KTypeParameter _ },_) ->
					Some(Expr.mk_stack_tp_init gen.gcon t pos)
				| _ ->
					None
			in
			let path = Expr.t_path t in
			incr temp_num;
			let v = alloc_var (Printf.sprintf "%s_%s_tmp_%d" (String.concat "_" (fst path)) (snd path) !temp_num) t in
			gen.declare_var (v,init);
			v
		in

		(* needed vars for this filter *)
		function e -> match e.eexpr with
			| TFunction _ ->
				temp_num := 0;
				Type.map_expr gen.map e
			| TVars vdecl ->
				(* ensure no uninitialized type parameter *)
				{ e with eexpr = TVars( List.map (function
					| v,None -> (match follow v.v_type with
						| TInst({ cl_kind = KTypeParameter _ }, _) ->
							v, Some(Expr.mk_stack_tp_init gen.gcon v.v_type e.epos)
						| _ -> v, None)
					| v, Some e ->
						v, Some (gen.map e)
				) vdecl ) }
			| TNew(c,tl,el) when tl <> [] && c.cl_path <> ([],"typeref") ->
				let el = List.map (Expr.mk_type_param gen.gcon e.epos) tl @ (List.map gen.map el) in
				{ e with eexpr = TNew(c,tl,el) }
			(*
				FIXME: calls with type parameters are only handled on static / member functions;
				we still need to decide what to do with function pointers
			*)
			| TCall(({ eexpr = TField(ef, (FInstance(c,cf) | FStatic(c,cf) as fi)) } as e1), el)
			when not (Meta.has Meta.Plain cf.cf_meta) && (function_has_type_parameter gen.gcon cf.cf_type || cf.cf_params <> [] && fst c.cl_path <> ["c";"_Pointer"]) ->
				let ef = gen.map ef in
				let args, ret = get_fun cf.cf_type in
				(* if return type is a type param, add new element to call params *)
				let _, applied_ret = get_fun e1.etype in
				let args, el_last = if is_type_param gen.gcon ret then begin
					(* TODO: when central temp var handling is (re)done, get_temp here *)
					let v = get_temp applied_ret e.epos in
					if is_type_param gen.gcon applied_ret then
						args, [Expr.mk_local v e.epos] (* already a reference var *)
					else
						args, [Expr.mk_ref gen.gcon e.epos (Expr.mk_local v e.epos)]
				end else
					args, []
				in

				let el = List.map2 (fun e (_,_,t) -> match e.eexpr with
					| _ when not (is_type_param gen.gcon t) -> gen.map e
					| TLocal _ when is_type_param gen.gcon e.etype -> (* type params are already encoded as pointer-to-val *)
						gen.map e
					| TLocal _ ->
						Expr.mk_ref gen.gcon e.epos (gen.map e)
					| _ ->
						let v = get_temp e.etype e.epos in
						gen.map (Expr.mk_assign_ref gen.gcon e.epos (Expr.mk_local v e.epos) e)
				) el args
				in
				let el = get_param_args gen e cf e1 ef @ el in
				let eret = { e with eexpr = TCall({ e1 with eexpr = TField(ef, fi) }, el @ el_last) } in
				(* if type parameter is being cast into a concrete one, we need to dereference it *)
				if is_type_param gen.gcon ret && not (is_type_param gen.gcon applied_ret) then
					Expr.mk_deref gen.gcon e.epos (Expr.mk_cast (gen.gcon.hxc.t_pointer applied_ret) eret)
				else
					eret
			| TReturn (Some er) when Option.is_some out_var ->
				(* when returning type parameter values, instead of just returning a value, we must first set the out var *)
				let out_var = Option.get out_var in
				let ret_val = Expr.mk_comma_block gen.gcon e.epos [
					{
						eexpr = TBinop(Ast.OpAssign, Expr.mk_local out_var e.epos, er);
						etype = er.etype;
						epos = e.epos;
					};
					Expr.mk_local out_var e.epos
				] in
				{ e with eexpr = TReturn(Some( gen.map ret_val )) }
			(* type parameter extra indirection handling *)
			(* we need to handle the following cases: *)
			(* - param -> param assign: always copy *)
			(* - param -> concrete cast/field use: dereference *)
			(* - concrete -> param cast/field get/set: copy *)
			| TBinop( (Ast.OpAssign | Ast.OpAssignOp _ as op), e1, e2 ) when is_field_type_param gen.gcon e1 || is_field_type_param gen.gcon e2 ->
				if is_field_type_param gen.gcon e1 && is_field_type_param gen.gcon e2 then begin
					if op <> Ast.OpAssign then assert false; (* FIXME: AssignOp should become two operations; should be very rare though *)
					let local, wrap = match e1.eexpr with
						| TLocal _ | TConst _ -> e1, (fun e -> e)
						| _ ->
							let t = gen.gcon.hxc.t_pointer gen.gcom.basic.tvoid in
							let v = get_temp t e.epos in
							Expr.mk_local v e1.epos, (fun e -> { e with eexpr = TBinop(Ast.OpAssign, Expr.mk_local v e.epos, e) })
					in
					let as_pointer e = { e with etype = gen.gcon.hxc.t_pointer e.etype } in
					let memcpy_args = [ as_pointer (wrap(gen.map e1)); as_pointer (gen.map e2); Expr.mk_sizeof gen.gcon e.epos (Expr.mk_type_param gen.gcon e.epos e1.etype) ] in
					let ret = Expr.mk_comma_block gen.gcon e.epos [
						Expr.mk_static_call_2 (gen.gcon.hxc.c_cstring) "memcpy" memcpy_args e.epos;
						local
					] in

					ret
				end else if is_field_type_param gen.gcon e1 then
					{ e with eexpr = TBinop(op, Expr.mk_deref gen.gcon e.epos (Expr.mk_cast (gen.gcon.hxc.t_pointer e2.etype) (gen.map e1)), gen.map e2) }
				else
					{ e with eexpr = TBinop(op, gen.map e1, Expr.mk_deref gen.gcon e.epos (Expr.mk_cast (gen.gcon.hxc.t_pointer e1.etype) (gen.map e2))) }
			(* - pointer array access -> pointer + typeref's size * index *)
			| TArray(e1, idx) -> (match follow e1.etype with
				| TAbstract({a_path=["c"], "Pointer"},[t]) when is_type_param gen.gcon t ->
					{ e with
						eexpr = TBinop(
							Ast.OpAdd, gen.map e1,
							{ idx with eexpr = TBinop(Ast.OpMult, gen.map idx, Expr.mk_sizeof gen.gcon e.epos (Expr.mk_type_param gen.gcon e.epos t)) }
						);
					}
				| TInst(c,p) -> (try
					let f = PMap.find "__get" c.cl_fields in
					gen.map { e with eexpr = TCall({ e1 with eexpr = TField(e1,FInstance(c,f)); etype = apply_params c.cl_types p f.cf_type}, [idx]) }
				with | Not_found -> Type.map_expr gen.map e)
				| _ -> Type.map_expr gen.map e)
			| TConst TNull when is_type_param gen.gcon e.etype ->
					Expr.mk_null_type_param gen.gcon e.epos e.etype
			| TBinop( (Ast.OpAssign | Ast.OpAssignOp _ as op), {eexpr = TArray(e1,e2)}, v) -> (try
					match follow e1.etype with
					| TInst(c,p) ->
					let f = PMap.find "__set" c.cl_fields in
					if op <> Ast.OpAssign then assert false; (* FIXME: this should be handled in an earlier stage (gencommon, anyone?) *)
					gen.map { e with eexpr = TCall({ e1 with eexpr = TField(e1,FInstance(c,f)); etype = apply_params c.cl_types p f.cf_type}, [e2;v]) }
					| _ -> raise Not_found
				with | Not_found ->
					Type.map_expr gen.map e)
			| _ -> Type.map_expr gen.map e

end

(** VarDeclarations **)
(**
	this filter will take all out-of-place TVars declarations and add to the beginning of each block
	TPatMatch has some var names sanitized
**)
module VarDeclarations = struct

	let filter gen = function e ->
		match e.eexpr with
		| TMeta( (Meta.Comma,a,p), ({ eexpr = TBlock(el) } as block) ) when el <> [] ->
			{ e with eexpr = TMeta( (Meta.Comma,a,p), { block with eexpr = TBlock(List.map gen.map el) }) }
		| TBlock(el) ->
			let first = ref true in
			let el = List.map (fun e -> match e.eexpr with
				| TVars(v) when !first && not (List.exists (fun (v,_) -> TypeParams.is_type_param gen.gcon v.v_type) v) ->
					e
				| _ ->
					first := false;
					gen.map e
			) el in
			{ e with eexpr = TBlock(el) }
		| TVars tvars ->
			let el = ExtList.List.filter_map (fun (v,eo) ->
				gen.declare_var (v,None);
				match eo with
				| None -> None
				| Some e -> Some { eexpr = TBinop(Ast.OpAssign, Expr.mk_local v e.epos, gen.map e); etype = e.etype; epos = e.epos }
			) tvars in
			(match el with
			| [e] -> e
			| _ -> Expr.mk_comma_block gen.gcon e.epos el)
		| TPatMatch dt ->
 			let rec dtl d = match d with
				| DTGoto _ | DTExpr _ ->
					()
				| DTGuard(_,dt1,dt2) ->
					dtl dt1;
					(match dt2 with None -> () | Some dt -> dtl dt)
				| DTSwitch(_,cl,dto) ->
					List.iter (fun (_,dt) -> dtl dt) cl;
					(match dto with None -> () | Some dt -> dtl dt)
				| DTBind(bl,dt) ->
					List.iter (fun ((v,_),_) ->
						if v.v_name.[0] = '`' then v.v_name <- "_" ^ (String.sub v.v_name 1 (String.length v.v_name - 1));
						gen.declare_var (v,None);
					) bl;
					dtl dt
			in
			Array.iter dtl dt.dt_dt_lookup;
			List.iter (fun (v,_) ->
				if v.v_name.[0] = '`' then v.v_name <- "_" ^ (String.sub v.v_name 1 (String.length v.v_name - 1));
				gen.declare_var (v,None)
			) dt.dt_var_init;
			Type.map_expr gen.map e
		| _ -> Type.map_expr gen.map e

end

(*
	Transforms (x = value) function arguments to if (x == null) x = value expressions.
	Must run before VarDeclarations or the universe implodes.
*)
module DefaultValues = struct

	let filter gen = function e ->
		match e.eexpr with
		| TFunction tf ->
			let e = List.fold_left (fun e (v,co) ->
				match co with
				| None
				| Some TNull -> e
				| Some c ->
					let eloc = Expr.mk_local v e.epos in
					let econd = Codegen.mk_parent (Codegen.binop OpEq (mk (TConst TNull) (mk_mono()) e.epos) eloc gen.gcom.basic.tbool e.epos) in
					let eassign = Codegen.binop OpAssign eloc (mk (TConst c) v.v_type e.epos) v.v_type e.epos in
					let eif = mk (TIf(econd,eassign,None)) gen.gcom.basic.tvoid e.epos in
					Codegen.concat eif e
			) tf.tf_expr tf.tf_args in
			{ e with eexpr = TFunction({tf with tf_expr = e})}
		| _ ->
			Type.map_expr gen.map e

end

let sort_anon_fields fields =
	List.sort (fun cf1 cf2 ->
		match Meta.has Meta.Optional cf1.cf_meta, Meta.has Meta.Optional cf2.cf_meta with
		| false,false | true,true -> compare cf1.cf_name cf2.cf_name
		| true, false -> 1
		| false, true -> -1
	) fields

let pmap_to_list pm = PMap.fold (fun v acc -> v :: acc) pm []

let alloc_temp_func con =
	let id = con.num_temp_funcs in
	con.num_temp_funcs <- con.num_temp_funcs + 1;
	let name = "_hx_func_" ^ (string_of_int id) in
	name, id

(*
	This filter handles unification cases where AST transformation may be required.
	These occur in the following nodes:

		- TBinop(OpAssign,_,_)
		- TVars
		- TCall and TNew
		- TArrayDecl
		- TObjectDecl
		- TReturn
		- TODO: TIf may be missing

	It may perform the following transformations:
		- pad TObjectDecl with null for optional arguments
		- use Array as argument list to "rest" argument
*)
module TypeChecker = struct

	let rec check gen e t =
		match e.eexpr,follow t with
		| TObjectDecl fl,(TAnon an as ta) ->
			let fields = sort_anon_fields (pmap_to_list an.a_fields) in
			let fl = List.map (fun cf ->
				try cf.cf_name,List.assoc cf.cf_name fl
				with Not_found -> cf.cf_name,mk (TConst TNull) (mk_mono()) e.epos
			) fields in
			{ e with eexpr = TObjectDecl fl; etype = ta}
		(* literal String assigned to const char* = pass through *)
		| TCall({eexpr = TField(_,FStatic({cl_path = [],"String"}, {cf_name = "ofPointerCopyNT"}))},[{eexpr = TConst (TString _)} as e]),(TAbstract({a_path = ["c"],"ConstPointer"},[TAbstract({a_path=[],"hx_char"},_)]) | TAbstract({a_path=["c"],"VarArg"},_)) ->
			e
		(* String assigned to const char* or VarArg = unwrap *)
		| _,(TAbstract({a_path=["c"],"VarArg"},_)) when (match follow e.etype with TInst({cl_path = [],"String"},_) -> true | _ -> false) ->
			Expr.mk_static_call_2 gen.gcon.hxc.c_string "raw" [e] e.epos
		| TMeta(m,e1),t ->
			{ e with eexpr = TMeta(m,check gen e1 t)}
		| TParenthesis(e1),t ->
			{ e with eexpr = TParenthesis(check gen e1 t)}
		| _ ->
			e

	let check_call_params gen el tl =
		let rec loop acc el tl = match el,tl with
			| e :: el, (n,_,t) :: tl ->
				(* check for rest argument *)
				begin match e.eexpr with
					| TArrayDecl el2 when n = "rest" && tl = [] && el = [] ->
						let ta = match follow e.etype with
							| TInst({cl_path=[],"Array"},[t]) -> t
							| _ -> t_dynamic
						in
						loop acc el2 (List.map (fun _ -> "rest",false,ta) el2)
					| _ ->
						loop ((check gen (gen.map e) t) :: acc) el tl
				end
			| [], [] ->
				acc
			| [],_ ->
				(* should not happen due to padded nulls *)
				assert false
			| _, [] ->
				(* not sure about this one *)
				assert false
		in
		List.rev (loop [] el tl)

	let fstack = ref []

	let filter gen = function e ->
		match e.eexpr with
		| TBinop(OpAssign,e1,e2) ->
			{e with eexpr = TBinop(OpAssign,gen.map e1,check gen (gen.map e2) e1.etype)}
		| TVars vl ->
			let vl = ExtList.List.filter_map (fun (v,eo) ->
				match eo with
				| None -> Some(v,None)
				| Some e ->
					Some (v,Some (check gen (gen.map e) v.v_type))
			) vl in
			{ e with eexpr = TVars(vl)}
		| TLocal v ->
			{ e with etype = v.v_type }
		| TCall(e1,el) ->
			begin match follow e1.etype with
				| TFun(args,_) | TAbstract({a_path = ["c"],"FunctionPointer"},[TFun(args,_)]) ->
					{e with eexpr = TCall(gen.map e1,check_call_params gen el args)}
				| _ -> Type.map_expr gen.map e
			end
		| TNew(c,tl,el) ->
			let tcf,_ = get_constructor (fun cf -> apply_params c.cl_types tl cf.cf_type) c in
			begin match follow tcf with
				| TFun(args,_) | TAbstract({a_path = ["c"],"FunctionPointer"},[TFun(args,_)]) ->
					{e with eexpr = TNew(c,tl,check_call_params gen el args)}
				| _ -> Type.map_expr gen.map e
			end
		| TArrayDecl el ->
			begin match follow e.etype with
				| TInst({cl_path=[],"Array"},[t]) -> {e with eexpr = TArrayDecl(List.map (fun e -> check gen (gen.map e) t) el)}
				| _ -> Type.map_expr gen.map e
			end
		| TObjectDecl fl ->
			begin match follow e.etype with
				| TAnon an ->
					let fl = List.map (fun (n,e) ->
						let t = (PMap.find n an.a_fields).cf_type in
						n,check gen (gen.map e) t
					) fl in
					{ e with eexpr = TObjectDecl fl }
				| _ -> Type.map_expr gen.map e
			end
		| TReturn (Some e1) ->
			begin match !fstack,follow gen.gfield.cf_type with
				| tf :: _,_ -> { e with eexpr = TReturn (Some (check gen (gen.map e1) tf.tf_type))}
				| [],TFun(_,tr) -> { e with eexpr = TReturn (Some (check gen (gen.map e1) tr))}
				| _,t -> assert false
			end
		| TCast (e1,None) ->
			let t = follow e.etype in
			if e1.etype != t then
				{e with eexpr = TCast(check gen (gen.map e1) t,None)}
			else
				{e with eexpr = TCast(gen.map e1,None)}
		| TSwitch(e1,cases,def) ->
			let cases = List.map (fun (el,e) -> List.map (fun e -> check gen (gen.map e) e1.etype) el,gen.map e) cases in
			{ e with eexpr = TSwitch(e1,cases,match def with None -> None | Some e -> Some (gen.map e))}
		| TFunction tf ->
			fstack := tf :: !fstack;
			let etf = {e with eexpr = TFunction({tf with tf_expr = gen.map tf.tf_expr})} in
			fstack := List.tl !fstack;
			etf
		| TThrow e1 ->
			{ e with eexpr = TThrow (check gen e1 e1.etype) }
		| _ ->
			Type.map_expr gen.map e

end

(*
	- wraps String literals in String
	- translates String OpAdd to String.concat
	- translates String == String to String.equals
	- translates switch(String) to if-chain
*)
module StringHandler = struct
	let is_string t = match follow t with
		| TInst({cl_path = [],"String"},_) -> true
		| _ -> false

	let filter gen e =
		match e.eexpr with
		(* always wrap String literal *)
		| (TConst (TString s) | TNew({cl_path=[],"String"},[],[{eexpr = TConst(TString s)}])) ->
			Expr.mk_static_call_2 gen.gcon.hxc.c_string "ofPointerCopyNT" [mk (TConst (TString s)) e.etype e.epos] e.epos
		| TBinop((OpEq | OpNotEq) as op,e1,e2) when is_string e1.etype ->
			Expr.mk_binop op
				(Expr.mk_static_call_2 gen.gcon.hxc.c_string "equals" [gen.map e1; gen.map e2] e1.epos)
				(Expr.mk_int gen.gcom 1 e1.epos)
				e.etype
				e.epos
		| TBinop(OpAdd,e1,e2) when is_string e1.etype ->
			Expr.mk_static_call_2 gen.gcon.hxc.c_string "concat" [gen.map e1; gen.map e2] e1.epos
		| TBinop(OpAssignOp(OpAdd),e1,e2) when is_string e1.etype ->
			(* TODO: we have to cache e1 in a temp var and handle the assignment correctly *)
			Expr.mk_binop
				OpAssign
				e1
				(Expr.mk_static_call_2 gen.gcon.hxc.c_string "concat" [gen.map e1; gen.map e2] e1.epos)
				e1.etype
				e.epos
		| TSwitch(e1,cases,def) when is_string e1.etype ->
			let mk_eq e1 e2 = mk (TBinop(OpEq,e1,e2)) gen.gcon.com.basic.tbool (punion e1.epos e2.epos) in
			let mk_or e1 e2 = mk (TBinop(OpOr,e1,e2)) gen.gcon.com.basic.tbool (punion e1.epos e2.epos) in
			let mk_if (el,e) eo =
				let eif = List.fold_left (fun eacc e -> mk_or eacc (mk_eq e1 e)) (mk_eq e1 (List.hd el)) (List.tl el) in
				mk (TIf(Codegen.mk_parent eif,e,eo)) e.etype e.epos
			in
			let ifs = match List.fold_left (fun eacc ec -> Some (mk_if ec eacc)) def cases with Some e -> e | None -> assert false in
			gen.map ifs
		| _ ->
			Type.map_expr gen.map e
end

(*
	This filter turns all non-top TFunction nodes into class fields and creates a hx_closure expression
	in their place.

	It also handles call to closures, i.e. local variables and Var class fields.
*)
module ClosureHandler = struct
	let fstack = ref []

	let ctx_name = "_ctx"

	let mk_closure_field gen tf ethis p =
		let locals = ref PMap.empty in
		let unknown = ref PMap.empty in
		let save_locals () =
			let old = !locals in
			fun () -> locals := old
		in
		let add_local v = if not (PMap.mem v.v_name !locals) then locals := PMap.add v.v_name v !locals in
		let add_unknown v = if not (PMap.mem v.v_name !unknown) then unknown := PMap.add v.v_name v !unknown in
		List.iter (fun (v,_) -> add_local v) tf.tf_args;
		let v_this = alloc_var "this" t_dynamic in
		let t_ctx = mk_mono() in
		let v_ctx = alloc_var ctx_name t_ctx in
		let e_ctx = mk (TLocal v_ctx) v_ctx.v_type p in
		let mk_ctx_field v =
			let ef = mk (TField(e_ctx,FDynamic v.v_name)) v.v_type p in
			mk (TCast(ef,None)) v.v_type p
		in
		let rec loop e = match e.eexpr with
			| TVars vl ->
				let vl = List.map (fun (v,eo) ->
					add_local v;
					v,match eo with None -> None | Some e -> Some (loop e)
				) vl in
				{ e with eexpr = TVars vl }
			| TLocal v ->
				if not (PMap.mem v.v_name !locals) then begin
					add_unknown v;
					mk_ctx_field v;
				end else
					e
			| TFunction tf ->
				let save = save_locals() in
				List.iter (fun (v,_) -> add_local v) tf.tf_args;
				let e = { e with eexpr = TFunction { tf with tf_expr = loop tf.tf_expr } } in
				save();
				e
			| TConst TThis ->
				if not (PMap.mem v_this.v_name !locals) then add_unknown v_this;
				mk_ctx_field v_this
			| _ ->
				Type.map_expr loop e
		in
		let e = loop tf.tf_expr in
		let name,id = alloc_temp_func gen.gcon in
		let vars,fields = PMap.fold (fun v (vars,fields) ->
			let e = match v.v_name,ethis with
				| "this",Some e -> e
				| _ -> mk (TLocal v) v.v_type p
			in
			(v :: vars),((v.v_name,e) :: fields)
		) !unknown ([],[]) in
		let eobj = Expr.mk_obj_decl fields p in
		Type.unify eobj.etype t_ctx;
		let t = TFun((ctx_name,false,eobj.etype) :: List.map (fun (v,_) -> v.v_name,false,v.v_type) tf.tf_args,tf.tf_type) in
		let cf = Expr.mk_class_field name t true p (Method MethNormal) [] in
		let tf = {
			tf_args = (v_ctx,None) :: List.map (fun v -> v,None) vars;
			tf_type = tf.tf_type;
			tf_expr = e;
		} in
		cf.cf_expr <- Some (mk (TFunction tf) e.etype e.epos);
		cf,eobj

	let rec is_closure_expr e =
		match e.eexpr with
			| TLocal _
			| TField(_,(FInstance(_,{cf_kind = Var _ | Method MethDynamic}) | FStatic(_,{cf_kind = Var _ | Method MethDynamic}))) ->
				true
			| TMeta(_,e1) | TParenthesis(e1) ->
				is_closure_expr e1
			| _ ->
				false

	let add_closure_field gen c tf ethis p =
		let cf,e_init = mk_closure_field gen tf ethis p in
		gen.add_field c cf true;
		gen.run_filter cf;
		let e_field = mk (TField(e_init,FStatic(c,cf))) cf.cf_type p in
		wrap_function e_init e_field

	let is_call_expr = ref false
	let is_extern = ref false

	let filter gen e =
		match e.eexpr with
		| TFunction tf ->
			fstack := tf :: !fstack;
			let e1 = match !fstack,gen.mtype with
				| _ :: [],_ ->
					{e with eexpr = TFunction({tf with tf_expr = gen.map tf.tf_expr})}
				| _,Some (TClassDecl c) ->
					add_closure_field gen c tf None e.epos
				| _ ->
					assert false
			in
			fstack := List.tl !fstack;
			e1
		| TMeta((Meta.Custom ":closureWrap",_,_),_) ->
			e
		| TCall(e1,el) ->
			let old = !is_call_expr,!is_extern in
			is_call_expr := true;
			is_extern := (match e1.eexpr with TField(_,FStatic({cl_extern = true},_)) -> true | _ -> false);
			let e1 = gen.map e1 in
			is_call_expr := fst old;
			let el = List.map gen.map el in
			let e = if not !is_extern && is_closure_expr e1 then begin
				let args,r = match follow e1.etype with TFun(args,r) -> args,r | _ -> assert false in
				let mk_cast e = mk (TCast(e,None)) (gen.gcon.hxc.t_func_pointer e.etype) e.epos in
				let efunc = mk (TField(e1,FDynamic "_func")) (TFun(args,r)) e.epos in
				let efunc2 = {efunc with etype = TFun(("_ctx",false,t_dynamic) :: args,r)} in
				let ethis = mk (TField(e1,FDynamic "_this")) t_dynamic e.epos in
				let eif = Expr.mk_binop OpNotEq ethis (mk (TConst TNull) (mk_mono()) e.epos) gen.gcom.basic.tbool e.epos in
				let ethen = mk (TCall(mk_cast efunc2,ethis :: el)) e.etype e.epos in
				let eelse = mk (TCall(mk_cast efunc,el)) e.etype e.epos in
				let e = mk (TIf(eif,ethen,Some eelse)) e.etype e.epos in
				let ternary_hack = mk (TMeta((Meta.Custom ":ternary",[],e.epos),e)) e.etype e.epos in
				mk (TCast(ternary_hack,None)) r e.epos
			end else
				{e with eexpr = TCall(e1,el)}
			in
			is_extern := snd old;
			e
		| TField(_,FStatic(c,({cf_kind = Method m} as cf))) when not !is_call_expr && not (m = MethDynamic) && not !is_extern ->
			wrap_static_function (Expr.mk_static_field c cf e.epos)
		| TField(e1,FClosure(Some c,{cf_expr = Some {eexpr = TFunction tf}})) ->
			add_closure_field gen c tf (Some e1) e.epos
		| _ ->
			Type.map_expr gen.map e
end

(*
	- TTry is replaced with a TSwitch and uses setjmp
	- TThrow is replaced with a call to longjmp
	- TFor is replaced with TWhile
*)
module ExprTransformation = struct

	let mk_array_decl gen el t p =
		let tparam = match follow t with
			| TInst(_,[t]) -> t
			| _ -> assert false
		in
		let c_fixed_array = gen.gcon.hxc.c_fixed_array in
		let v = alloc_var "arr" (TInst(c_fixed_array,[tparam])) in
		let eloc = mk (TLocal v) v.v_type p in
		let eret = mk (TReturn (Some (Expr.mk_static_call_2 gen.gcon.hxc.c_array "ofNative" [eloc] p))) t_dynamic p in
		let (vars,einit,arity) = List.fold_left (fun (vl,el,i) e ->
			let v = alloc_var ("v" ^ (string_of_int i)) tparam in
			let e = Expr.mk_binop OpAssign (mk (TArray(eloc,Expr.mk_int gen.gcom i p)) tparam p) (mk (TLocal v) v.v_type p) tparam p in
			(v :: vl,e :: el,i + 1)
		) ([],[eret],0) el in
		let vars = List.rev vars in
		let enew = mk (TNew(c_fixed_array,[tparam],[Expr.mk_int gen.gcon.com arity p;mk (TConst TNull) (mk_mono()) p])) t p in
		let evar = mk (TVars [v,Some enew]) gen.gcom.basic.tvoid p in
		let e = mk (TBlock (evar :: einit)) t p in
		let tf = {
			tf_args = List.map (fun v -> v,None) vars;
			tf_type = t;
			tf_expr = e;
		} in
		let name,id = alloc_temp_func gen.gcon in
		let tfun = TFun (List.map (fun v -> v.v_name,false,v.v_type) vars,t) in
		let cf = Expr.mk_class_field name tfun true p (Method MethNormal) [] in
		let efun = mk (TFunction tf) tfun p in
		cf.cf_expr <- Some efun;
		let c = match gen.mtype with Some (TClassDecl c) -> c | _ -> assert false in
		gen.add_field c cf true;
		gen.run_filter cf;
		Expr.mk_static_call c cf el p

	let filter gen e =
		match e.eexpr with
		| TTry (e1,cl) ->
			let p = e.epos in
			let hxc = gen.gcon.hxc in
			let epush = Expr.mk_static_call_2 hxc.c_exception "push" [] p in
			let esubj = Codegen.mk_parent (Expr.mk_static_call_2 hxc.c_csetjmp "setjmp" [Expr.mk_deref gen.gcon p epush] p) in
			let epop = Expr.mk_static_call_2 hxc.c_exception "pop" [] p in
			let loc = alloc_var "_hx_jmp_buf" (hxc.t_pointer hxc.t_jmp_buf) in
			let epopassign = mk (TVars [loc,Some epop]) gen.gcon.com.basic.tvoid p in
			let ec1,found = Expr.insert_expr (gen.map e1) true (fun e ->
				match e.eexpr with
				| TReturn _ | TBreak _ | TContinue -> Some epop
				| _ -> None
			) in
			let ec1 = if found then ec1 else Codegen.concat ec1 epop in
			let c1 = [Expr.mk_int gen.gcom 0 e.epos],ec1 in
			let def = ref None in
			let cl = c1 :: (ExtList.List.filter_map (fun (v,e) ->
				let eassign = Codegen.binop OpAssign (mk (TVars [v,None]) gen.gcon.com.basic.tvoid p) (Expr.mk_static_field_2 hxc.c_exception "thrownObject" p) v.v_type p in
				let e = Codegen.concat eassign (Codegen.concat epopassign (gen.map e)) in
				let e = mk (TBlock [e]) e.etype e.epos in
				if v.v_type == t_dynamic then begin
					def := Some e;
					None;
				end else
					Some ([Expr.mk_int gen.gcom (get_type_id gen.gcon v.v_type) e.epos],e)
			) cl) in
			mk (TSwitch(esubj,cl,!def)) e.etype e.epos
		| TThrow e1 ->
			let p = e.epos in
			let eassign = Codegen.binop OpAssign (Expr.mk_static_field_2 gen.gcon.hxc.c_exception "thrownObject" p) e1 e1.etype e1.epos in
			let epeek = Expr.mk_static_call_2 gen.gcon.hxc.c_exception "peek" [] p in
			let el = [Expr.mk_deref gen.gcon p epeek;Expr.mk_int gen.gcom (get_type_id gen.gcon e1.etype) p] in
			let ejmp = Codegen.mk_parent (Expr.mk_static_call_2 gen.gcon.hxc.c_csetjmp "longjmp" el p) in
			Expr.mk_comma_block gen.gcon p [eassign;ejmp]
		| TFor(v,e1,e2) ->
			let e1 = gen.map e1 in
			let vtemp = gen.declare_temp e1.etype None in
			gen.declare_var (v,None);
			let ev = Expr.mk_local vtemp e1.epos in
			let ehasnext = mk (TField(ev,quick_field e1.etype "hasNext")) (tfun [] gen.gcon.com.basic.tbool) e1.epos in
			let ehasnext = mk (TCast(ehasnext,None)) ehasnext.etype ehasnext.epos in
			let ehasnext = mk (TCall(ehasnext,[])) ehasnext.etype ehasnext.epos in
			let enext = mk (TField(ev,quick_field e1.etype "next")) (tfun [] v.v_type) e1.epos in
			let enext = mk (TCast(enext,None)) enext.etype enext.epos in
			let enext = mk (TCall(enext,[])) v.v_type e1.epos in
			let ebody = Codegen.concat enext e2 in
			mk (TBlock [
				mk (TVars [vtemp,Some e1]) gen.gcom.basic.tvoid e1.epos;
				mk (TWhile((mk (TParenthesis ehasnext) ehasnext.etype ehasnext.epos),ebody,NormalWhile)) gen.gcom.basic.tvoid e1.epos;
			]) gen.gcom.basic.tvoid e.epos
		| TArrayDecl [] ->
			let c,t = match follow (gen.gcon.com.basic.tarray (mk_mono())) with
				| TInst(c,[t]) -> c,t
				| _ -> assert false
			in
			mk (TNew(c,[t],[Expr.mk_type_param gen.gcon e.epos t])) gen.gcon.com.basic.tvoid e.epos
		| TArrayDecl el ->
			mk_array_decl gen el e.etype e.epos
		| _ ->
			Type.map_expr gen.map e

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
			field = null_field;
			loop_stack = [];
		};
		dependencies = PMap.empty;
	}

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
	let relpath path = path_to_file_path ((get_relative_path ctx.type_path path),snd path) in
	spr (Printf.sprintf "#ifndef %s\n" n);
	spr (Printf.sprintf "#define %s\n" n);
	spr "#include <stdio.h>\n";
	spr "#include <stdlib.h>\n";
	spr "#include <string.h>\n";
	if ctx.type_path <> ([],"hxc") then spr (Printf.sprintf "#include \"%s.h\"\n" (relpath ([],"hxc")));

	PMap.iter (fun path dept ->
		let name = path_to_name path in
		match dept with
			| DCStd -> spr (Printf.sprintf "#include <%s.h>\n" (path_to_file_path path))
			| DFull -> spr (Printf.sprintf "#include \"%s.h\"\n" (relpath path))
			| DForward -> spr (Printf.sprintf "typedef struct %s %s;\n" name name);
	) ctx.dependencies;
	Buffer.add_buffer buf ctx.buf_h;
	spr "\n#endif";

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

	write_if_changed (ctx.file_path_no_ext ^ ".h") (Buffer.contents buf);

	let sc = Buffer.contents ctx.buf_c in
	if String.length sc > 0 then begin
		let buf = Buffer.create (Buffer.length ctx.buf_c) in
		Buffer.add_string buf ("#include \"" ^ (snd ctx.type_path) ^ ".h\"\n");
		PMap.iter (fun path dept ->
			match dept with
			| DFull | DForward ->
				Buffer.add_string buf (Printf.sprintf "#include \"%s.h\"\n" (relpath path))
			| _ -> ()
		) ctx.dependencies;
		Buffer.add_string buf sc;
		write_if_changed (ctx.file_path_no_ext ^ ".c") (Buffer.contents buf);
	end

let anon_signature ctx fields =
	let fields = pmap_to_list fields in
	let fields = sort_anon_fields fields in
	let id = String.concat "," (List.map (fun cf -> cf.cf_name ^ (s_type (print_context()) (follow cf.cf_type))) fields) in
	try fst (PMap.find id ctx.con.anon_types)
	with Not_found ->
		ctx.con.num_anon_types <- ctx.con.num_anon_types + 1;
		let s = "_hx_anon_" ^ (string_of_int ctx.con.num_anon_types) in
		ctx.con.anon_types <- PMap.add id (s,fields) ctx.con.anon_types;
		s

(* Dependency handling *)

let add_dependency ctx dept path =
	if path <> ctx.type_path then ctx.dependencies <- PMap.add path dept ctx.dependencies

let parse_include com s p =
	if s.[0] = '<' then begin
		if s.[String.length s - 1] <> '>' then com.error "Invalid include directive" p;
		(* take off trailing .h because it will be added back later *)
		let i = if String.length s > 4 && s.[String.length s - 2] = 'h' && s.[String.length s - 3] = '.' then
			String.length s - 4
		else
			String.length s - 2
		in
		([],String.sub s 1 i),DCStd
	end else
		([],s),DForward

let check_include_meta ctx meta =
	try
		let _,el,p = get_meta Meta.Include meta in
		List.iter (fun e -> match fst e with
			| EConst(String s) when String.length s > 0 ->
				let path,dept = parse_include ctx.con.com s p in
				add_dependency ctx dept path
			| _ ->
				()
		) el;
		true
	with Not_found ->
		false

let add_class_dependency ctx c =
	if not (check_include_meta ctx c.cl_meta) && not c.cl_extern then
		add_dependency ctx (if Meta.has Meta.Struct c.cl_meta then DFull else DForward) c.cl_path

let add_enum_dependency ctx en =
	if not (check_include_meta ctx en.e_meta) && not en.e_extern then
		add_dependency ctx (if Meta.has Meta.Struct en.e_meta then DFull else DForward) en.e_path

let add_abstract_dependency ctx a =
	if not (check_include_meta ctx a.a_meta) then
		add_dependency ctx (if Meta.has Meta.Struct a.a_meta then DFull else DForward) a.a_path

let add_type_dependency ctx t = match follow t with
	| TInst(c,_) ->
		add_class_dependency ctx c
	| TEnum(en,_) ->
		add_enum_dependency ctx en
	| TAnon an ->
		add_dependency ctx DFull (["c"],anon_signature ctx an.a_fields);
	| TAbstract(a,_) ->
		add_abstract_dependency ctx a
	| TDynamic _ ->
		add_dependency ctx DForward ([],"Dynamic")
	| _ ->
		(* TODO: that doesn't seem quite right *)
		add_dependency ctx DForward ([],"Dynamic")

(* Helper *)

let rec is_value_type ctx t = match follow t with
	| TAbstract({ a_impl = None }, _) -> true
	| TInst(c,_) -> has_meta Meta.Struct c.cl_meta
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

let full_enum_field_name en ef = (path_to_name en.e_path) ^ "_" ^ ef.ef_name

let monofy_class c = TInst(c,List.map (fun _ -> mk_mono()) c.cl_types)

(* Type signature *)

let rec s_type ctx t =
	match follow t with
	| TAbstract({a_path = [],"Int"},[]) -> "int"
	| TAbstract({a_path = [],"Float"},[]) -> "double"
	| TAbstract({a_path = [],"Void"},[]) -> "void"
	| TAbstract({a_path = ["c"],"Pointer"},[t]) -> (match follow t with
		| TInst({cl_kind = KTypeParameter _},_) ->
			"char*" (* we will manipulate an array of type parameters like an array of bytes *)
		| _ -> s_type ctx t ^ "*")
	| TAbstract({a_path = ["c"],"ConstPointer"},[t]) -> "const " ^ (s_type ctx t) ^ "*"
	| TAbstract({a_path = ["c"],"FunctionPointer"},[TFun(args,ret)]) ->
		Printf.sprintf "%s (*)(%s)" (s_type ctx ret) (String.concat "," (List.map (fun (_,_,t) -> s_type ctx t) args))
	| TInst(({cl_path = [],"typeref"} as c),_) ->
		add_class_dependency ctx c;
		"const " ^ (path_to_name c.cl_path) ^ "*"
	| TAbstract({a_path = [],"Bool"},[]) -> "int"
	| TAbstract( a, tps ) when Meta.has (Meta.Custom ":int") a.a_meta ->
		let (meta,el,epos) = Meta.get (Meta.Custom ":int") a.a_meta in
		(match el with
			| [(EConst (String s),_)] -> ( match s with
			| "int64" -> "hx_int64"
			| "int32" -> "hx_int32"
			| "int16" -> "hx_int16"
			| "int8"  -> "hx_int8"
			| "uint64" -> "hx_uint64"
			| "uint32" -> "hx_uint32"
			| "uint16" -> "hx_uint16"
			| "uint8" -> "hx_uint8"
			| _ -> s)
			| _ -> assert false;
	)
	| TInst({cl_kind = KTypeParameter _},_) -> "void*"
	| TInst(c,_) ->
		let ptr = if is_value_type ctx t then "" else "*" in
		add_class_dependency ctx c;
		(path_to_name c.cl_path) ^ ptr
	| TEnum(en,_) ->
		let ptr = if is_value_type ctx t then "" else "*" in
		add_enum_dependency ctx en;
		(path_to_name en.e_path) ^ ptr
	| TAbstract(a,_) when Meta.has Meta.Native a.a_meta ->
		let ptr = if is_value_type ctx t then "" else "*" in
		(path_to_name a.a_path) ^ ptr
	| TAnon a ->
		begin match !(a.a_status) with
		| Statics c -> "Class_" ^ (path_to_name c.cl_path) ^ "*"
		| EnumStatics en -> "Enum_" ^ (path_to_name en.e_path) ^ "*"
		| AbstractStatics a -> "Anon_" ^ (path_to_name a.a_path) ^ "*"
		| _ ->
			let signature = anon_signature ctx a.a_fields in
			add_dependency ctx DFull (["c"],signature);
			"c_" ^ signature ^ "*"
		end
	| TFun(args,ret) ->
		(* Printf.sprintf "%s (*)(%s)" (s_type ctx ret) (String.concat "," (List.map (fun (_,_,t) -> s_type ctx t) args)) *)
		"hx_closure*";
	| _ -> "void*"

let s_type_with_name ctx t n =
	match follow t with
	| TFun(args,ret) ->
		"hx_closure* " ^ n
	| _ ->
		(s_type ctx t) ^ " " ^ n

(* Expr generation *)

let rec generate_call ctx e e1 el = match e1.eexpr,el with
	| TLocal({v_name = "__c"}),[{eexpr = TConst(TString code)}] ->
		spr ctx code;
	| TField(_,FStatic({cl_path = ["c"],"Lib"}, cf)),(e1 :: el) ->
		begin match cf.cf_name with
		| "getAddress" ->
			spr ctx "&(";
			generate_expr ctx e1;
			spr ctx ")"
		| "dereference" ->
			spr ctx "*(";
			generate_expr ctx e1;
			spr ctx ")"
		| "sizeof" ->
			(* get TypeReference's type *)
			let t = match follow e1.etype with
				| TInst({cl_path = [],"typeref"},[t]) -> follow t
				| t -> t
			in
			(match t with
			| TInst({cl_kind = KTypeParameter _},_) ->
				(* indirection *)
				spr ctx "(";
				generate_expr ctx e1;
				spr ctx ")->refSize"
			| _ ->
				print ctx "sizeof(%s)" (s_type ctx t));
		| "alloca" ->
			spr ctx "ALLOCA(";
			generate_expr ctx e1;
			spr ctx ")"
		| "cCode" ->
			let code = match e1.eexpr with
				| TConst (TString s) -> s
				| TCast ({eexpr = TConst (TString s) },None) -> s
				| _ -> assert false
			in
			spr ctx code;
		| _ ->
			assert false
		end
	| TField(_,FStatic(c,({cf_name = name} as cf))),el when Meta.has Meta.Plain cf.cf_meta ->
		ignore(check_include_meta ctx c.cl_meta);
		print ctx "%s(" name;
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")";
	| TField(_,FStatic(_,cf)),el when Meta.has Meta.Native cf.cf_meta ->
		let name = match get_native_name cf.cf_meta with
			| Some s -> s
			| None -> ctx.con.com.error "String argument expected for @:native" e.epos; "_"
		in
		print ctx "%s(" name;
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")";
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

and generate_constant ctx e = function
	| TString s ->
		print ctx "\"%s\"" s;
	| TInt i ->
		print ctx "%ld" i
	| TFloat s ->
		print ctx "%s" s
	| TNull ->
		spr ctx "NULL"
	| TSuper ->
		(* TODO: uhm... *)
		()
	| TBool true ->
		spr ctx "1"
	| TBool false ->
		spr ctx "0"
	| TThis ->
		spr ctx "this"

and generate_expr ctx e = match e.eexpr with
	| TConst c ->
		generate_constant ctx e c
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
	| TBlock [{eexpr = TBlock _} as e1] ->
		(* TODO: I don't really understand where these come from *)
		generate_expr ctx e1
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
	| TCall(e1,el) ->
		generate_call ctx e e1 el
	| TTypeExpr (TClassDecl c) ->
		spr ctx (path_to_name c.cl_path);
	| TTypeExpr (TEnumDecl e) ->
		add_enum_dependency ctx e;
		spr ctx (path_to_name e.e_path);
	| TTypeExpr (TTypeDecl _ | TAbstractDecl _) ->
		(* shouldn't happen? *)
		assert false
	| TField(_,FStatic(c,cf)) ->
		add_class_dependency ctx c;
		spr ctx (full_field_name c cf)
	| TField(_,FEnum(en,ef)) ->
		add_enum_dependency ctx en;
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
		let s = match follow e.etype with
			| TAnon an ->
				let signature = anon_signature ctx an.a_fields in
				add_dependency ctx DFull (["c"],signature);
				signature
			| _ -> assert false
		in
		print ctx "new_c_%s(" s;
		concat ctx "," (generate_expr ctx) (List.map (fun (_,e) -> add_type_dependency ctx e.etype; e) fl);
		spr ctx ")";
	| TNew({cl_path = [],"typeref"},[p],[]) -> (match follow p with
		| TInst(({cl_kind = KTypeParameter _} as c),_) ->
			generate_expr ctx (TypeParams.type_parameter_expr ctx.con e.epos c.cl_path)
		| _ ->
			let path = Expr.t_path p in
			add_type_dependency ctx p;
			spr ctx ("&" ^ (path_to_name path ) ^ "__typeref"))
	| TNew(c,tl,el) ->
		add_class_dependency ctx c;
		spr ctx (full_field_name c (match c.cl_constructor with None -> assert false | Some cf -> cf));
		spr ctx "(";
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")";
	| TReturn None ->
		spr ctx "return"
	| TReturn (Some e1) ->
		spr ctx "return ";
		generate_expr ctx e1;
	| TVars(vl) ->
		let f (v,eo) =
			spr ctx (s_type_with_name ctx v.v_type v.v_name);
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
	| TMeta((Meta.Custom ":ternary",_,_),{eexpr = TIf(e1,e2,Some e3)}) ->
		spr ctx "(";
		generate_expr ctx e1;
		spr ctx " ? ";
		generate_expr ctx e2;
		spr ctx " : ";
		generate_expr ctx e3;
		spr ctx ")"
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
			spr ctx "{";
			let b = open_block ctx in
			List.iter (fun e ->
				newline ctx;
				generate_expr ctx e;
			) (match e.eexpr with TBlock el -> el | _ -> [e]);
			newline ctx;
			spr ctx "break";
			b();
			newline ctx;
			spr ctx "}";
		in
		let b = open_block ctx in
		List.iter (fun (el,e) ->
			newline ctx;
			spr ctx "case ";
			concat ctx "," (generate_expr ctx) el;
			spr ctx ":";
			generate_case_expr e;
		) cases;
		begin match edef with
			| None -> ()
			| Some e ->
				newline ctx;
				spr ctx "default:";
				generate_case_expr e;
		end;
		b();
		newline ctx;
		spr ctx "}";
	| TBinop(op,e1,e2) ->
		generate_expr ctx e1;
		print ctx " %s " (match op with OpUShr -> ">>" | _ -> s_binop op);
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
	| TMeta(_,e) ->
		generate_expr ctx e
	| TCast(e1,_) ->
		begin match follow e1.etype with
		| TInst(c,_) when Meta.has Meta.Struct c.cl_meta -> generate_expr ctx e1;
		| TAbstract({a_path = ["c"],"Pointer"},[t]) when ((s_type ctx e.etype) = "int") -> generate_expr ctx e1;
		| TAbstract( a, tps ) when Meta.has (Meta.Custom ":int") a.a_meta -> generate_expr ctx e1;
		| _ ->
			print ctx "((%s) (" (s_type ctx e.etype);
			generate_expr ctx e1;
			spr ctx "))"
		end
	| TEnumParameter (e1,ef,i) ->
		generate_expr ctx e1;
		begin match follow e1.etype with
			| TEnum(en,_) ->
				add_enum_dependency ctx en;
				let s,_,_ = match ef.ef_type with TFun(args,_) -> List.nth args i | _ -> assert false in
				print ctx "->args.%s.%s" ef.ef_name s;
			| _ ->
				assert false
		end
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
			| DTSwitch(e,cl,dto) ->
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
				begin match dto with
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
	| TArrayDecl _ | TTry _ | TFor _ | TThrow _ | TFunction _ ->
		(* handled in field init pass *)
		assert false

(* Type generation *)

(*
	This function applies some general transformations.

	- TArrayDecl introduces an init function which is TCalled
*)
let init_field ctx cf =
	match cf.cf_expr with
		| None -> None
		| Some {eexpr = TFunction tf} -> Some tf.tf_expr
		| Some e -> Some e

let generate_function_header ctx c cf stat =
	let args,ret,s = match follow cf.cf_type with
		| TFun(args,ret) -> args,ret,full_field_name c cf
		| TAbstract({a_path = ["c"],"Pointer"},[t]) ->
			begin match follow t with
				| TFun(args,ret) -> args,ret,"(*" ^ (full_field_name c cf) ^ ")"
				| _ -> assert false
			end
		| _ -> assert false
	in
	let sargs = List.map (fun (n,_,t) -> s_type_with_name ctx t n) args in
	let sargs = if stat then sargs else (s_type_with_name ctx (monofy_class c) "this") :: sargs in
	print ctx "%s(%s)" (s_type_with_name ctx ret s) (String.concat "," sargs)

let get_typeref_forward ctx path =
	Printf.sprintf "extern const typeref %s__typeref" (path_to_name path)

let generate_typedef_declaration ctx t =
	let path = Expr.t_path t in
	if is_value_type ctx t then
		print ctx "const %s %s__default = { 0 }; //default" (s_type ctx t) (path_to_name path)
	else
		print ctx "const void* %s__default = NULL; //default" (path_to_name path);
	newline ctx;
	let nullval = Printf.sprintf "&%s__default" (path_to_name path) in
	Printf.sprintf "const typeref %s__typeref = { \"%s\", sizeof(%s), %s }; //typeref declaration" (path_to_name path) (s_type_path path) (s_type ctx t) nullval

let generate_method ctx c cf stat =
	let e = init_field ctx cf in
	ctx.fctx <- {
		field = cf;
		loop_stack = []
	};
	generate_function_header ctx c cf stat;
	begin match e with
		| None -> ()
		| Some e -> generate_expr ctx e
	end;
	newline ctx

let generate_class ctx c =
	let vars = DynArray.create () in
	let svars = DynArray.create () in
	let methods = DynArray.create () in

	(* split fields into member vars, static vars and functions *)
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ | Method MethDynamic -> DynArray.add vars cf
		| Method m ->  DynArray.add methods (cf,false)
	) c.cl_ordered_fields;
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ -> DynArray.add svars cf
		| Method _ -> DynArray.add methods (cf,true)
	) c.cl_ordered_statics;

	let path = path_to_name c.cl_path in
	let t_class = monofy_class c in

	(* add constructor as function *)
	begin match c.cl_constructor with
		| None -> ()
		| Some cf ->
			match follow cf.cf_type, cf.cf_expr with
			| TFun(args,_), Some e ->
				let einit = if is_value_type ctx t_class then
					Some (Expr.mk_ccode ctx ("{0}; //semicolon"))
				else
					let p = cf.cf_pos in
					(* TODO: get rid of this *)
					let esize = Expr.mk_ccode ctx (Printf.sprintf "sizeof(%s)" path) in
					Some (Expr.mk_static_call_2 ctx.con.hxc.c_cstdlib "calloc" [Expr.mk_int ctx.con.com 1 p;esize] p)
				in
				let loc = alloc_var "this" t_class in
				let einit = mk (TVars [loc,einit]) ctx.con.com.basic.tvoid cf.cf_pos in
				let ereturn = mk (TReturn (Some (Expr.mk_local loc cf.cf_pos))) t_dynamic cf.cf_pos in
				let e = match e.eexpr with
					| TFunction({tf_expr = ({eexpr = TBlock el } as ef) } as tf) ->
						{e with eexpr = TFunction ({tf with tf_expr = {ef with eexpr = TBlock(einit :: el @ [ereturn])}})}
					| _ -> assert false
				in
				cf.cf_expr <- Some e;
				cf.cf_type <- TFun(args, monofy_class c);
				DynArray.add methods (cf,true)
			| _ -> ()
	end;

	ctx.buf <- ctx.buf_c;

	spr ctx (generate_typedef_declaration ctx (TInst(c,List.map snd c.cl_types)));
	newline ctx;

	(* generate static vars *)
	if not (DynArray.empty svars) then begin
		spr ctx "// static vars\n";
		DynArray.iter (fun cf ->
			spr ctx (s_type_with_name ctx cf.cf_type (full_field_name c cf));
			newline ctx;
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
			DynArray.add methods (f,true)
	end;

	(* generate function implementations *)
	if not (DynArray.empty methods) then begin
		DynArray.iter (fun (cf,stat) ->
			generate_method ctx c cf stat
		) methods;
	end;

	ctx.buf <- ctx.buf_h;

	(* generate header code *)
	List.iter (function
		| Meta.HeaderCode,[(EConst(String s),_)],_ ->
			spr ctx s
		| _ -> ()
	) c.cl_meta;

	(* forward declare class type *)
	print ctx "typedef struct %s %s" path path;
	newline ctx;

	(* generate member struct *)
	if not (DynArray.empty vars) then begin
		spr ctx "// member var structure\n";
		print ctx "typedef struct %s {" path;
		let b = open_block ctx in
		DynArray.iter (fun cf ->
			newline ctx;
			spr ctx (s_type_with_name ctx cf.cf_type cf.cf_name);
		) vars;
		b();
		newline ctx;
		print ctx "} %s" path;
		newline ctx;
		spr ctx "\n";
	end else begin
		print ctx "typedef struct %s { void* dummy; } %s" path path;
		newline ctx;
	end;

	(* generate static vars *)
	if not (DynArray.empty svars) then begin
		spr ctx "// static vars\n";
		DynArray.iter (fun cf ->
		spr ctx (s_type_with_name ctx cf.cf_type (full_field_name c cf));
		newline ctx
    ) svars
	end;

	(* generate forward declarations of functions *)
	if not (DynArray.empty methods) then begin
		spr ctx "// forward declarations\n";
		DynArray.iter (fun (cf,stat) ->
			generate_function_header ctx c cf stat;
			newline ctx;
		) methods;
	end;

	add_dependency ctx DForward ([],"typeref");
	spr ctx (get_typeref_forward ctx c.cl_path);
	newline ctx

let generate_enum ctx en =
	ctx.buf <- ctx.buf_h;
	add_dependency ctx DForward ([],"typeref");
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
				spr ctx (s_type_with_name ctx t n);
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
			print ctx "%s new_%s(%s)" (s_type ctx ret) (full_enum_field_name en ef) (String.concat "," (List.map (fun (n,_,t) -> s_type_with_name ctx t n) args));
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

let generate_typeref con t =
	let path = Expr.t_path t in
	let ctx = mk_type_context con path  in
	ctx.buf <- ctx.buf_c;
	spr ctx (generate_typedef_declaration ctx t);
	newline ctx;
	ctx.buf <- ctx.buf_h;
	add_dependency ctx DForward ([],"typeref");
	spr ctx (get_typeref_forward ctx path);
	newline ctx;
	close_type_context ctx

let generate_type con mt = match mt with
	| TClassDecl c when not c.cl_extern ->
		let ctx = mk_type_context con c.cl_path  in
		generate_class ctx c;
		close_type_context ctx;
	| TEnumDecl en when not en.e_extern ->
		let ctx = mk_type_context con en.e_path  in
		generate_enum ctx en;
		close_type_context ctx;
	| TAbstractDecl { a_path = [],"Void" } -> ()
	| TAbstractDecl a when Meta.has Meta.CoreType a.a_meta ->
		generate_typeref con (TAbstract(a,List.map snd a.a_types))
	| _ ->
		()

let generate_anon con name fields =
	let ctx = mk_type_context con (["c"],name) in
	let name = "c_" ^ name in
	begin match fields with
	| [] ->
		print ctx "typedef int %s" name;
		newline ctx
	| fields ->
		spr ctx "// forward declaration";
		newline ctx;
		print ctx "typedef struct %s %s" name name;
		newline ctx;

		spr ctx "// structure";

		newline ctx;
		print ctx "typedef struct %s {" name;
		let b = open_block ctx in
		List.iter (fun cf ->
			newline ctx;
			spr ctx (s_type_with_name ctx cf.cf_type cf.cf_name);
		) fields;
		b();
		newline ctx;
		print ctx "} %s" name;
		newline ctx;
	end;

	spr ctx "// constructor forward declaration";
	newline ctx;
	print ctx "%s* new_%s(%s)" name name (String.concat "," (List.map (fun cf -> s_type_with_name ctx cf.cf_type cf.cf_name) fields));
	newline ctx;

	ctx.buf <- ctx.buf_c;

	spr ctx "// constructor definition";
	newline ctx;
	print ctx "%s* new_%s(%s) {" name name (String.concat "," (List.map (fun cf -> s_type_with_name ctx cf.cf_type cf.cf_name) fields));
	let b = open_block ctx in
	newline ctx;
	print ctx "%s* _hx_this = (%s*) malloc(sizeof(%s))" name name name;
	List.iter (fun cf ->
		newline ctx;
		print ctx "_hx_this->%s = %s" cf.cf_name cf.cf_name;
	) fields;
	newline ctx;
	spr ctx "return _hx_this";
	b();
	newline ctx;
	spr ctx "}";
	close_type_context ctx

let generate_anon_file con =
	PMap.iter (fun _ (s,cfl) -> generate_anon con s cfl) con.anon_types

let generate_init_file con =
	let ctx = mk_type_context con (["c"],"Init") in
	ctx.buf <- ctx.buf_c;
	print ctx "void _hx_init() {";
	let b = open_block ctx in
	List.iter (fun path ->
		add_dependency ctx DForward path;
		newline ctx;
		print ctx "%s__hx_init()" (path_to_name path);
	) con.init_modules;
	b();
	newline ctx;
	spr ctx "}";
	close_type_context ctx

let generate_make_file con =
	let relpath path = path_to_file_path path in
	let main_name = match con.com.main_class with Some path -> snd path | None -> "main" in
	let filepath = con.com.file ^ "/Makefile" in
	print_endline ("Writing " ^ filepath);
	let ch = open_out_bin filepath in
	output_string ch ("OUT = " ^ main_name ^ "\n");
	output_string ch "ifndef MSVC\n";
	output_string ch ("\tOUTFLAG := -o \n");
	output_string ch ("\tOBJEXT := o \n");
	output_string ch ("\tLDFLAGS += -lm \n");
	output_string ch ("else\n");
	output_string ch ("\tOUTFLAG := /Fo\n");
	output_string ch ("\tOBJEXT := obj\n");
	output_string ch ("\tCC := cl.exe\n");
	output_string ch ("endif\n");
	output_string ch ("all: $(OUT)\n");
	List.iter (fun ctx ->
		output_string ch (Printf.sprintf "%s.$(OBJEXT): %s.c " (relpath ctx.type_path) (relpath ctx.type_path));
		PMap.iter (fun path dept -> match dept with
			| DFull | DForward -> output_string ch (Printf.sprintf "%s.h " (relpath path))
			| _ -> ()
		) ctx.dependencies;
		output_string ch (Printf.sprintf "\n\t$(CC) $(CFLAGS) $(INCLUDES) $(OUTFLAG)%s.$(OBJEXT) -c %s.c\n\n" (relpath ctx.type_path) (relpath ctx.type_path))
	) con.generated_types;
	output_string ch "OBJECTS = ";
	List.iter (fun ctx -> output_string ch (Printf.sprintf "%s.$(OBJEXT) " (relpath ctx.type_path))) con.generated_types;
	output_string ch "\n\n$(OUT): $(OBJECTS)";
	output_string ch "\n\t$(CC) $(CFLAGS) $(INCLUDES) $(OBJECTS) -o $(OUT) $(LDFLAGS)\n";
	output_string ch "\n\nclean:\n\t$(RM) $(OUT) $(OBJECTS)";
	close_out ch

let generate_hxc_files con =
	generate_anon_file con;
	generate_init_file con;
	generate_make_file con

let add_filters con =
	(* ascending priority *)
	Filters.add_filter con ExprTransformation.filter;
	Filters.add_filter con TypeParams.filter;
	Filters.add_filter con VarDeclarations.filter;
	Filters.add_filter con TypeChecker.filter;
	Filters.add_filter con StringHandler.filter;
	Filters.add_filter con ClosureHandler.filter;
	Filters.add_filter con DefaultValues.filter

let generate com =
	let rec find_class path mtl = match mtl with
		| TClassDecl c :: _ when c.cl_path = path -> c
		| _ :: mtl -> find_class path mtl
		| [] -> assert false
	in
	let c_lib = find_class (["c"],"Lib") com.types in
	let null_func _ = assert false in
	let null_abstract = {
		a_path = [],"";
		a_private = false;
		a_module = null_module;
		a_pos = null_pos;
		a_doc = None;
		a_types = [];
		a_meta = [];
		a_from = [];
		a_to = [];
		a_ops = [];
		a_unops = [];
		a_impl = None;
		a_array = [];
		a_this = mk_mono();
	} in
	let hxc = List.fold_left (fun acc mt -> match mt with
		| TClassDecl c ->
			begin match c.cl_path with
				| [],"typeref" -> {acc with t_typeref = fun t -> TInst(c,[t])}
				| [],"jmp_buf" -> {acc with t_jmp_buf = TInst(c,[])}
				| [],"hxc" -> {acc with c_boot = c}
				| [],"String" -> {acc with c_string = c}
				| [],"Array" -> {acc with c_array = c}
				| ["c"],"FixedArray" -> {acc with c_fixed_array = c}
				| ["c"],"Exception" -> {acc with c_exception = c}
				| ["c"],"CString" -> {acc with c_cstring = c}
				| ["c"],"CStdlib" -> {acc with c_cstdlib = c}
				| ["c"],"CSetjmp" -> {acc with c_csetjmp = c}
				| _ -> acc
			end
		| TAbstractDecl a ->
			begin match a.a_path with
			| ["c"],"Pointer" ->
				a.a_meta <- (Meta.CoreType,[],Ast.null_pos) :: a.a_meta;
				{acc with t_pointer = fun t -> TAbstract(a,[t])}
			| ["c"],"ConstPointer" ->
				a.a_meta <- (Meta.CoreType,[],Ast.null_pos) :: a.a_meta;
				{acc with t_const_pointer = fun t -> TAbstract(a,[t])}
			| ["c"],"FunctionPointer" ->
				{acc with t_func_pointer = fun t -> TAbstract(a,[t])}
			| ["c"],"Int64" ->
				a.a_meta <- (Meta.CoreType,[],Ast.null_pos) :: a.a_meta;
				{acc with t_int64 = fun t -> TAbstract(a,[t])}
			| [],"Bool" ->
				{acc with c_bool = a}
			| _ ->
				acc
			end
		| _ ->
			acc
	) {
		c_lib = c_lib;
		cf_deref = PMap.find "dereference" c_lib.cl_statics;
		cf_addressof = PMap.find "getAddress" c_lib.cl_statics;
		cf_sizeof = PMap.find "sizeof" c_lib.cl_statics;
		t_typeref = null_func;
		t_pointer = null_func;
		t_const_pointer = null_func;
		t_func_pointer = null_func;
		t_int64 = null_func;
		t_jmp_buf = t_dynamic;
		c_boot = null_class;
		c_exception = null_class;
		c_string = null_class;
		c_array = null_class;
		c_fixed_array = null_class;
		c_cstring = null_class;
		c_csetjmp = null_class;
		c_cstdlib = null_class;
		c_bool = null_abstract;
	} com.types in
	let con = {
		com = com;
		hxc = hxc;
		cvar = alloc_var "__c" t_dynamic;
		num_temp_funcs = 0;
		num_labels = 0;
		num_anon_types = -1;
		(* this has to start at 0 so the first type id is 1 *)
		num_identified_types = 0;
		anon_types = PMap.empty;
		type_ids = PMap.empty;
		type_parameters = PMap.empty;
		init_modules = [];
		generated_types = [];
		filters = [];
	} in
	add_filters con;
	let gen = Filters.run_filters_types con in
	List.iter (fun f -> f()) gen.delays; (* we can choose another time to run this if needed *)
	List.iter (generate_type con) com.types;
	generate_hxc_files con
