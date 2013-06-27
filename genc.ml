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
	expr : texpr option;
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
	mutable type_parameters : (path, texpr) PMap.t;
	mutable init_modules : path list;
	mutable generated_types : type_context list;
	mutable t_typeref : t -> t;
	mutable t_pointer : t -> t;
	mutable filters : (string * float * filter) list;
	mutable filters_dirty : bool;
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
	(* tvar_decl -> is_temp -> unit; declares a variable on the current block *)
	mutable declare_var : (tvar * texpr option) -> bool -> unit;
	(* requests a new temporary variable *)
	mutable get_temp : Type.t -> tvar;
	(* frees the temporary variable, making it available for further use *)
	mutable free_temp : tvar -> unit;
	(* forces a var to be available as a temporary *)
	mutable force_temp : tvar -> unit;
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

	let mk_local v p =
		{ eexpr = TLocal v; etype = v.v_type; epos = p }

	let mk_ref con p e =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__adressof" t_dynamic); etype = t_dynamic; epos = p },
				[e]
			);
			etype = t_dynamic;
			epos = p;
		}

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

	let mk_assign_ref con p local value =
		mk_comma_block con p [
			{
				eexpr = TBinop(Ast.OpAssign, local, value);
				etype = value.etype;
				epos = p;
			};
			mk_ref con p local
		]

	let mk_cast t e =
		{ e with eexpr = TCast(e, None); etype = t }

	let mk_deref con p e =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__deref" t_dynamic); etype = t_dynamic; epos = p },
				[e]
			);
			etype = t_dynamic;
			epos = p;
		}

	let mk_call con p name args =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__call" t_dynamic); etype = t_dynamic; epos = p },
				{ eexpr = TConst(TString name); etype = t_dynamic; epos = p } :: args
			);
			etype = t_dynamic;
			epos = p;
		}

	let mk_sizeof con p e =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__sizeof__" t_dynamic); etype = t_dynamic; epos = p },
				[e]
			);
			etype = con.com.basic.tint;
			epos = p;
		}

	let mk_type_param con pos t =
		let t = con.t_typeref t in
		let c,p = match follow t with
			| TInst(c,p) -> c,p
			| _ -> assert false
		in
		{ eexpr = TNew(c,p,[]); etype = t; epos = pos }

	let mk_stack_tp_init con t p =
		{
			eexpr = TCall(
				{ eexpr = TLocal(alloc_var "__call" t_dynamic); etype = t_dynamic; epos = p },
				[
					{ eexpr = TConst (TString "ALLOCA"); etype = t_dynamic; epos = p };
					{
						eexpr = TCall(
							{ eexpr = TLocal(alloc_var "__sizeof__" t_dynamic); etype = t_dynamic; epos = p },
							[mk_type_param con p t]
						);
						etype = con.com.basic.tint;
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
end

type t_dependency =
	| DAfter of float
	| DBefore of float

module Filters = struct
	exception ImpossibleDependency of string

	let max_dep = 10000.0
	let min_dep = - (10000.0)

	let solve_deps name (deps:t_dependency list) =
		let vmin = min_dep -. 1.0 in
		let vmax = max_dep +. 1.0 in
		let rec loop dep vmin vmax =
			match dep with
			| [] ->
				if vmin >= vmax then raise (ImpossibleDependency name);
				(vmin +. vmax) /. 2.0
			| head :: tail ->
				match head with
				| DBefore f ->
					loop tail (max vmin f) vmax
				| DAfter f ->
					loop tail vmin (min vmax f)
		in
		loop deps vmin vmax

	let add_filter con name priority filter =
		con.filters <- (name, priority, filter) :: con.filters;
		con.filters_dirty <- true

	let run_filters gen e =
		(* sort by priority *)
		if gen.gcon.filters_dirty then begin
			gen.gcon.filters <- List.sort (fun (_,f1,_) (_,f2,_) -> - (compare f1 f2)) gen.gcon.filters;
			gen.gcon.filters_dirty <- false
		end;
		(* local vars / temp vars handling *)
		let declared_vars = ref [] in
		let temps = ref (PMap.empty) in
		let temps_in_scope = ref (PMap.empty) in
		let used_temps = ref (PMap.empty) in

		(* temporary var handling *)
		let old_declare = gen.declare_var in
		let old_get = gen.get_temp in
		let old_free = gen.free_temp in
		let old_force = gen.force_temp in
		gen.declare_var <- (fun (tvar,eopt) is_temp ->
			declared_vars := (tvar,eopt) :: !declared_vars;
			if is_temp then gen.force_temp tvar
		);
		gen.get_temp <- (fun t ->
			let path = Expr.t_path t in
			let full_temps = ref [] in
			let temp_num = ref 0 in
			let var = try
				let tn = PMap.find path !temps in
				temp_num := tn;
				let temp = PMap.find path !temps_in_scope in
				full_temps := temp;
				let rec loop = function
					| [] -> raise Not_found
					| tvar :: acc when not (PMap.find tvar.v_id !used_temps) -> (* won't throw as all temporaries are added to used_temps *)
						tvar
					| _ :: acc -> loop acc
				in
				loop temp
			with | Not_found ->
				let ret = alloc_var (Printf.sprintf "%s_%s_tmp_%d" (String.concat "_" (fst path)) (snd path) !temp_num) t in
				declared_vars := (ret,None) :: !declared_vars;
				ret
			in
			temps_in_scope := PMap.add path (var :: !full_temps) !temps_in_scope;
			used_temps := PMap.add var.v_id true !used_temps;
			temps := PMap.add path (!temp_num + 1) !temps;
			var
		);
		gen.free_temp <- (fun v ->
			used_temps := PMap.add v.v_id false !used_temps
		);
		gen.force_temp <- (fun v ->
			used_temps := PMap.add v.v_id false !used_temps;
			let path = Expr.t_path v.v_type in
			try
				temps_in_scope := PMap.add path (v :: PMap.find path !temps_in_scope) !temps_in_scope;
			with | Not_found ->
				temps_in_scope := PMap.add path [v] !temps_in_scope
		);


		let ret = List.fold_left (fun e (_,_,f) ->
			let run = f gen in
			let process_next_block = ref true in
			(* set all temps as used, as we cannot guarantee for now the availability of a var *)
			used_temps := PMap.map (fun _ -> true) !used_temps;
			let rec map e = match e.eexpr with
				| TMeta( (Meta.Comma,_,_), { eexpr = TBlock(el) } ) ->
					process_next_block := false;
					let ret = run e in
					process_next_block := true;
					ret
				| TBlock(el) when !process_next_block ->
					let old_scope = !temps_in_scope in
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
							let vars = List.map (function
								| (v,None) -> (match follow v.v_type with
									| TInst({ cl_kind = KTypeParameter _ },_) ->
										v, Some(Expr.mk_stack_tp_init gen.gcon v.v_type e.epos)
									| _ -> v,None)
								| var -> var
							) vars in
							{ eexpr = TVars(List.rev vars); etype = gen.gcom.basic.tvoid; epos = e.epos } :: el
					in
					(* ensure no uninitialized type parameter *)
					let rec loop el acc = match el with
						| { eexpr = TVars [] } :: el ->
							loop el acc
						| { eexpr = TVars vdecl } :: el ->
							loop el ({ e with eexpr = TVars(List.map (function
								| (v,None) -> (match follow v.v_type with
									| TInst({ cl_kind = KTypeParameter _ },_) ->
										v, Some(Expr.mk_stack_tp_init gen.gcon v.v_type e.epos)
									| _ -> v,None)
								| var -> var
							) (vdecl)) } :: acc)
						| _ -> (List.rev acc) @ el
					in
					let el = loop el [] in
					let ret = { e with eexpr = TBlock(el) } in
					temps_in_scope := old_scope;
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
		gen.get_temp <- old_get;
		gen.free_temp <- old_free;
		gen.force_temp <- old_force;
		ret

	let mk_gen_context con =
		{
			gcom = con.com;
			gcon = con;
			gfield = null_field;
			mtype = None;
			map = (function _ -> assert false);
			declare_var = (fun _ _ -> assert false);
			get_temp = (fun _ -> assert false);
			free_temp = (fun _ -> assert false);
			force_temp = (fun _ -> assert false);
			delays = [];
		}

	let run_filters_field gen cf =
		gen.gfield <- cf;
		match cf.cf_expr with
		| None -> ()
		| Some e ->
			cf.cf_expr <- Some (run_filters gen e)

	let run_filters_types con =
		let gen = mk_gen_context con in
		List.iter (fun md -> match md with
			| TClassDecl c ->
				gen.mtype <- Some md;
				Option.may (run_filters_field gen) c.cl_constructor;
				List.iter (run_filters_field gen) c.cl_ordered_fields;
				List.iter (run_filters_field gen) c.cl_ordered_statics;
				gen.gfield <- null_field;
				c.cl_init <- Option.map (run_filters gen) c.cl_init
			| _ -> () (* TODO? *)
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
**)
module TypeParams = struct

	let name = "type_params_filter"

	let priority = Filters.min_dep

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
			let f = Expr.mk_class_field (type_param_name t) (gen.gcon.t_typeref t) false c.cl_pos (Var {v_read = AccNormal; v_write = AccNormal}) [] in
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
				List.map (fun (_,t) -> alloc_var (type_param_name t) (gen.gcon.t_typeref t),None) cf.cf_params, List.map snd cf.cf_params, false
			(* constructors have special treatment *)
			| "new", _ -> (match gen.mtype with
				| Some (TClassDecl c) ->
					List.map (fun (_,t) -> alloc_var (type_param_name t) (gen.gcon.t_typeref t), None) c.cl_types, List.map snd c.cl_types, true
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

					let bl = { eexpr = TBlock added_exprs; etype = gen.gcom.basic.tvoid; epos = cf.cf_pos } in
					cf.cf_expr <- Some { e with
						eexpr = TFunction({ tf with
							tf_args = tf_args @ tf.tf_args @ end_arg;
							tf_expr = Codegen.concat bl tf.tf_expr;
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

		(* needed vars for this filter *)
		function e -> match e.eexpr with
			| TNew(c,tl,el) when tl <> [] && c.cl_path <> ([],"typeref") ->
				let el = List.map (Expr.mk_type_param gen.gcon e.epos) tl @ (List.map gen.map el) in
				{ e with eexpr = TNew(c,tl,el) }
			| TCall(({ eexpr = TField(ef, (FInstance(c,cf) | FStatic(c,cf) as fi)) } as e1), el)
			when function_has_type_parameter gen.gcon cf.cf_type || cf.cf_params <> [] && fst c.cl_path <> ["c";"_Pointer"] ->
				let temps = ref [] in
				let ef = gen.map ef in
				let args, ret = get_fun cf.cf_type in
				(* if return type is a type param, add new element to call params *)
				let _, applied_ret = get_fun e1.etype in
				let args, el_last = if is_type_param gen.gcon ret then begin
					let v = gen.get_temp applied_ret in
					temps := v :: !temps;
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
						let v = gen.get_temp e.etype in
						temps := v :: !temps;
						gen.map (Expr.mk_assign_ref gen.gcon e.epos (Expr.mk_local v e.epos) e)
				) el args
				in
				List.iter (gen.free_temp) !temps;
				let el = get_param_args gen e cf e1 ef @ el in
				let eret = { e with eexpr = TCall({ e1 with eexpr = TField(ef, fi) }, el @ el_last) } in
				(* if type parameter is being cast into a concrete one, we need to dereference it *)
				if is_type_param gen.gcon ret && not (is_type_param gen.gcon applied_ret) then
					Expr.mk_deref gen.gcon e.epos (Expr.mk_cast (gen.gcon.t_pointer applied_ret) eret)
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
					let temps = ref [] in
					let local, wrap = match e1.eexpr with
						| TLocal _ -> e1, (fun e -> e)
						| _ ->
							let v = gen.get_temp t_dynamic in
							temps := v :: !temps;
							Expr.mk_local v e1.epos, (fun e -> { e with eexpr = TBinop(Ast.OpAssign, Expr.mk_local v e.epos, e) })
					in
					let ret = Expr.mk_comma_block gen.gcon e.epos [
						(Expr.mk_call gen.gcon e.epos "memcpy" [ wrap(gen.map e1); gen.map e2; Expr.mk_sizeof gen.gcon e.epos (Expr.mk_type_param gen.gcon e.epos e1.etype) ]);
						local
					] in

					List.iter (gen.free_temp) !temps;
					ret
				end else if is_field_type_param gen.gcon e1 then
					{ e with eexpr = TBinop(op, Expr.mk_deref gen.gcon e.epos (Expr.mk_cast (gen.gcon.t_pointer e2.etype) (gen.map e1)), gen.map e2) }
				else
					{ e with eexpr = TBinop(op, gen.map e1, Expr.mk_deref gen.gcon e.epos (Expr.mk_cast (gen.gcon.t_pointer e1.etype) (gen.map e2))) }
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

	let configure con =
		Filters.add_filter con name priority filter

end

(** VarDeclarations **)
(**
	this filter will take all out-of-place TVars declarations and add to the beginning of each block
	TPatMatch has some var names sanitized
**)
module VarDeclarations = struct

	let name = "var_declarations"

	let priority = Filters.solve_deps name [DBefore TypeParams.priority]

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
				gen.declare_var (v,None) false;
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
						gen.declare_var (v,None) false;
					) bl;
					dtl dt
			in
			Array.iter dtl dt.dt_dt_lookup;
			List.iter (fun (v,_) ->
				if v.v_name.[0] = '`' then v.v_name <- "_" ^ (String.sub v.v_name 1 (String.length v.v_name - 1));
				gen.declare_var (v,None) false
			) dt.dt_var_init;
			Type.map_expr gen.map e
		| _ -> Type.map_expr gen.map e

	let configure con =
		Filters.add_filter con name priority filter
end

let sort_anon_fields fields =
	List.sort (fun cf1 cf2 ->
		match Meta.has Meta.Optional cf1.cf_meta, Meta.has Meta.Optional cf2.cf_meta with
		| false,false | true,true -> compare cf1.cf_name cf2.cf_name
		| true, false -> 1
		| false, true -> -1
	) fields

let pmap_to_list pm = PMap.fold (fun v acc -> v :: acc) pm []

(*
	This filter handles unification cases where AST transformation may be required.
	These occur in the following nodes:

		- TBinop(OpAssign,_,_)
		- TVars
		- TCall
		- TArrayDecl
		- TObjectDecl
		- TReturn
		- TODO: TIf and TSwitch may be missing

	It may perform the following transformations:
		- pad TObjectDecl with null for optional arguments
*)
module TypeChecker = struct

	let name = "type_checker"

	let priority = Filters.solve_deps name [DBefore VarDeclarations.priority]

	let rec check gen e t =
		match e.eexpr,follow t with
		| TObjectDecl fl,(TAnon an as ta) ->
			let fields = sort_anon_fields (pmap_to_list an.a_fields) in
			let fl = List.map (fun cf ->
				try cf.cf_name,List.assoc cf.cf_name fl
				with Not_found -> cf.cf_name,mk (TConst TNull) (mk_mono()) e.epos
			) fields in
			{ e with eexpr = TObjectDecl fl; etype = ta}
		| TMeta(m,e1),t ->
			{ e with eexpr = TMeta(m,check gen e1 t)}
		| TParenthesis(e1),t ->
			{ e with eexpr = TParenthesis(check gen e1 t)}
		| _ ->
			e

	let filter gen = function e ->
		match e.eexpr with
		| TBinop(OpAssign,e1,e2) ->
			{e with eexpr = TBinop(OpAssign,gen.map e1,check gen (gen.map e2) e1.etype)}
		| TVars vl ->
			{e with eexpr = TVars(List.map (fun (v,eo) -> v,match eo with None -> None | Some e -> Some (check gen (gen.map e) v.v_type)) vl)}
		| TCall(e1,el) ->
			begin match follow e1.etype with
				| TFun(args,ret) ->
					let rec loop acc el tl = match el,tl with
						| e :: el, (_,_,t) :: tl ->
							loop ((check gen (gen.map e) t) :: acc) el tl
						| [], [] ->
							acc
						| [],_ ->
							(* should not happen due to padded nulls *)
							assert false
						| _, [] ->
							(* not sure about this one *)
							assert false
					in
					{e with eexpr = TCall(gen.map e1,(List.rev (loop [] el args)))}
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
			begin match follow gen.gfield.cf_type with
				| TFun(_,tr) -> { e with eexpr = TReturn (Some (check gen (gen.map e1) tr))}
				| _ -> assert false
			end
		| TCast (e1,None) ->
			let t = follow e.etype in
			if e1.etype != t then
				{e with eexpr = TCast(check gen (gen.map e1) t,None)}
			else
				{e with eexpr = TCast(gen.map e1,None)}
		| _ ->
			Type.map_expr gen.map e

	let configure con =
		Filters.add_filter con name priority filter

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
	| TAnon _ ->
		add_dependency ctx DFull (["c"],"AnonTypes");
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

let full_field_name c cf = (path_to_name c.cl_path) ^ "_" ^ cf.cf_name
let full_enum_field_name en ef = (path_to_name en.e_path) ^ "_" ^ ef.ef_name

let monofy_class c = TInst(c,List.map (fun _ -> mk_mono()) c.cl_types)

(* Type signature *)

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
	| TInst({cl_path = [],"String"},[]) ->
		"const char*"
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
			add_dependency ctx DFull (["c"],"AnonTypes");
			(anon_signature ctx a.a_fields) ^ "*"
		end
	| _ -> "void*"

let s_type_with_name ctx t n =
	match follow t with
	| TFun(args,ret) ->
		Printf.sprintf "%s (*%s)(%s)" (s_type ctx ret) n (String.concat "," (List.map (fun (_,_,t) -> s_type ctx t) args))
	| _ ->
		(s_type ctx t) ^ " " ^ n

let get_type_id ctx t =
	let id = Type.s_type (print_context()) (follow t) in
	try
		PMap.find id ctx.con.type_ids
	with Not_found ->
		ctx.con.num_identified_types <- ctx.con.num_identified_types + 1;
		ctx.con.type_ids <- PMap.add id ctx.con.num_identified_types ctx.con.type_ids;
		ctx.con.num_identified_types

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
		| _ ->
			ctx.con.com.error "This expression is invalid" e.epos);
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
	| TConst TNull when TypeParams.is_type_param ctx.con e.etype ->
		generate_expr ctx (Expr.mk_type_param ctx.con e.epos e.etype);
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
		let s = match follow e.etype with TAnon an -> anon_signature ctx an.a_fields | _ -> assert false in
		print ctx "new_%s(" s;
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
		spr ctx "return (";
		generate_expr ctx e1;
		spr ctx ")"
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
			newline ctx;
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
		begin match follow e1.etype with
		| TInst(c,_) when Meta.has Meta.Struct c.cl_meta -> generate_expr ctx e1;
		| _ ->
			print ctx "((%s) " (s_type ctx e.etype);
			generate_expr ctx e1;
			spr ctx ")"
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
	| TThrow e1 ->
		add_dependency ctx DCStd ([],"setjmp");
		add_dependency ctx DFull (["c"],"Exception");
		spr ctx "c_Exception_thrownObject = ";
		generate_expr ctx e1;
		newline ctx;
		print ctx "(longjmp(*c_Exception_peek(),%i))" (get_type_id ctx e1.etype);
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
	| TFunction _ ->
		print_endline ("Not implemented yet: " ^ (Expr.debug ctx e))

let mk_array_decl ctx el t p =
	let ts, eparam = match follow t with
		| TInst(_,[t]) -> s_type ctx t, Expr.mk_type_param ctx.con p t
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

	- TArrayDecl introduces an init function which is TCalled
	- TTry is replaced with a TSwitch and uses setjmp
	- TFor is replaced with TWhile
*)
let mk_function_context ctx cf =

	let rec loop e = match e.eexpr with
		| TArrayDecl el ->
			mk_array_decl ctx (List.map loop el) e.etype e.epos
		| TTry (e1,cl) ->
			add_dependency ctx DCStd ([],"setjmp");
			add_dependency ctx DFull (["c"],"Exception");
			let esubj = Expr.mk_ccode ctx "(setjmp(*c_Exception_push()))" in
			let epop = Expr.mk_ccode ctx "c_Exception_pop()" in
			let epopassign = Expr.mk_ccode ctx "jmp_buf* _hx_jmp_buf = c_Exception_pop()" in
			let c1 = [Expr.mk_int ctx 0 e.epos],(Codegen.concat (loop e1) epop) in
			let def = ref None in
			let cl = c1 :: (ExtList.List.filter_map (fun (v,e) ->
				let eassign = Expr.mk_ccode ctx ((s_type_with_name ctx v.v_type v.v_name) ^ " = c_Exception_thrownObject") in
				let e = Codegen.concat eassign (Codegen.concat epopassign (loop e)) in
				let e = mk (TBlock [e]) e.etype e.epos in
				if v.v_type == t_dynamic then begin
					def := Some e;
					None;
				end else
					Some ([Expr.mk_int ctx (get_type_id ctx v.v_type) e.epos],e)
			) cl) in
			mk (TSwitch(esubj,cl,!def)) e.etype e.epos
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
		| Some e -> Some (loop e)
	in
	{
		field = cf;
		expr = e;
		loop_stack = [];
	}

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
	ctx.fctx <- mk_function_context ctx cf;
	generate_function_header ctx c cf stat;
	match ctx.fctx.expr with
	| None -> newline ctx
	| Some {eexpr = TFunction ({tf_expr = {eexpr = TBlock el}; tf_type = t})} ->
		let e = mk (TBlock el) t cf.cf_pos in
		generate_expr ctx e
	| _ -> assert false

let generate_class ctx c =
	let vars = DynArray.create () in
	let svars = DynArray.create () in
	let methods = DynArray.create () in

	let add_init e = match c.cl_init with
		| None -> c.cl_init <- Some e
		| Some e2 -> c.cl_init <- Some (Codegen.concat e2 e)
	in

	let check_dynamic cf stat = match cf.cf_kind with
		| Method MethDynamic ->
			let cf2 = {cf with cf_name = cf.cf_name ^ "_hx_impl" } in
			DynArray.add methods (cf2,stat);
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
				check_dynamic cf false;
				DynArray.add methods (cf,false)
			| _ ->
				assert false;
	) c.cl_ordered_fields;
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ -> DynArray.add svars cf
		| Method _ ->
			check_dynamic cf true;
			DynArray.add methods (cf,true)
	) c.cl_ordered_statics;

	let path = path_to_name c.cl_path in

	(* add constructor as function *)
	begin match c.cl_constructor with
		| None -> ()
		| Some cf ->
			match follow cf.cf_type, cf.cf_expr with
			| TFun(args,_), Some e ->
				let einit =
					(if is_value_type ctx (TInst(c,List.map snd c.cl_types)) then
						Expr.mk_ccode ctx (Printf.sprintf "%s this" path)
					else
  					Expr.mk_ccode ctx (Printf.sprintf "%s* this = (%s*) calloc(1, sizeof(%s))" path path path))
				in
				let ereturn = Expr.mk_ccode ctx "return this" in
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
			match cf.cf_expr with
				| None -> ()
				| Some e ->
					let fctx = mk_function_context ctx cf in
					let e = Option.get fctx.expr in
					let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
					let ethis = mk (TTypeExpr (TClassDecl c)) ta cf.cf_pos in
					let efield = Codegen.field ethis cf.cf_name cf.cf_type cf.cf_pos in
					let eassign = mk (TBinop(OpAssign,efield,e)) efield.etype cf.cf_pos in
					cf.cf_expr <- Some eassign;
					add_init eassign;
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

	(* check if we have the main class *)
	(match ctx.con.com.main_class with
	| Some path when path = c.cl_path ->
		add_dependency ctx DCStd ([],"setjmp");
		add_dependency ctx DFull (["c"],"Exception");
		add_dependency ctx DForward (["c"],"Init");
		print ctx "int main() {\n\t_hx_init();\n\tswitch(setjmp(*c_Exception_push())) {\n\t\tcase 0: %s();break;\n\t\tdefault: printf(\"Something went wrong\");\n\t}\n}" (full_field_name c (PMap.find "main" c.cl_statics))
	| _ -> ());

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
      print ctx "extern %s %s" (s_type ctx cf.cf_type) (full_field_name c cf);
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

let generate_anon_file con =
	let ctx = mk_type_context con (["c"],"AnonTypes") in

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
			spr ctx (s_type_with_name ctx cf.cf_type cf.cf_name);
		) cfl;
		b();
		newline ctx;
		print ctx "} %s" s;
	) con.anon_types;
	newline ctx;

	spr ctx "// constructor forward declarations";
	PMap.iter (fun _ (s,cfl) ->
		newline ctx;
		print ctx "%s* new_%s(%s)" s s (String.concat "," (List.map (fun cf -> s_type_with_name ctx cf.cf_type cf.cf_name) cfl));
	) con.anon_types;
	newline ctx;

	ctx.buf <- ctx.buf_c;

	spr ctx "// constructor definitions";
	PMap.iter (fun _ (s,cfl) ->
		newline ctx;
		print ctx "%s* new_%s(%s) {" s s (String.concat "," (List.map (fun cf -> s_type_with_name ctx cf.cf_type cf.cf_name) cfl));
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

let get_type com path = try
	match List.find (fun md -> Type.t_path md = path) com.types with
	| TClassDecl c -> TInst(c, List.map snd c.cl_types)
	| TEnumDecl e -> TEnum(e, List.map snd e.e_types)
	| TTypeDecl t -> TType(t, List.map snd t.t_types)
	| TAbstractDecl a -> TAbstract(a, List.map snd a.a_types)
with | Not_found ->
	failwith("The type " ^ Ast.s_type_path path ^ " is required and was not found")

let add_filters con =
	TypeParams.configure con;
	VarDeclarations.configure con;
	TypeChecker.configure con

let generate com =
	let t_typeref = get_type com ([],"typeref") in
	let t_pointer = get_type com (["c"],"Pointer") in
	(* HACK: Pointer is actually a @:coreType *)
	(match t_pointer with TAbstract(a,_) -> a.a_meta <- (Meta.CoreType,[],Ast.null_pos) :: a.a_meta | _ -> assert false);
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
		type_parameters = PMap.empty;
		init_modules = [];
		generated_types = [];
		t_typeref = (match follow t_typeref with
			| TInst(c,_) -> fun t -> TInst(c,[t])
			| _ -> assert false);
		t_pointer = (match follow t_pointer with
			| TAbstract(a,_) -> fun t -> TAbstract(a,[t])
			| _ -> assert false);
		filters = [];
		filters_dirty = false;
	} in
	add_filters con;
	let gen = Filters.run_filters_types con in
	List.iter (fun f -> f()) gen.delays; (* we can choose another time to run this if needed *)
	List.iter (generate_type con) com.types;
	generate_hxc_files con
