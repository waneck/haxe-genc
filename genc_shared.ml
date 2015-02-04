open Ast
open Type
open Common

let is_main p = p.pfile = "src/Main.hx"

let null_wrap_const = ref Const

let mk_runtime_prefix n = "_hx_" ^ n
let cf_name_vtable = mk_runtime_prefix "v_table"
let cf_name_init = mk_runtime_prefix "init"

(* Static extensions for Common.context *)
module ExtCommon = struct
	let resolve_class com path =
		let rec loop types = match types with
			| [] -> raise Not_found
			| TClassDecl c :: _ when c.cl_path = path -> c
			| _ :: types -> loop types
		in
		loop com.types

	let resolve_static_field com path name p =
		let c  = resolve_class com path in
		try
			c,PMap.find name c.cl_statics
		with Not_found ->
			error (Printf.sprintf "Class %s has no field %s" (s_type_path c.cl_path) name) p
end

(* Collection of functions that return expressions *)
module ExprBuilder = struct
	let make_static_this c p =
		let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
		mk (TTypeExpr (TClassDecl c)) ta p

	let make_library_call com path name el p =
		let c,cf = ExtCommon.resolve_static_field com path name p in
		let ethis = make_static_this c p in
		let r = match follow cf.cf_type with
			| TFun(_,tr) -> tr
			| _ -> error (Printf.sprintf "Field is not a function") p
		in
		let e_field = mk (TField (ethis,(FStatic (c,cf)))) cf.cf_type p in
		mk (TCall(e_field,el)) r p

	let make_int com i p =
		mk (TConst (TInt (Int32.of_int i))) com.basic.tint p

	let make_float com f p =
		mk (TConst (TFloat f)) com.basic.tfloat p

	let make_null t p =
		mk (TConst TNull) t p

	let make_local v p =
		mk (TLocal v) v.v_type p

	let make_const_texpr com ct p = match ct with
		| TString s -> mk (TConst (TString s)) com.basic.tstring p
		| TInt i -> mk (TConst (TInt i)) com.basic.tint p
		| TFloat f -> mk (TConst (TFloat f)) com.basic.tfloat p
		| TBool b -> mk (TConst (TBool b)) com.basic.tbool p
		| TNull -> mk (TConst TNull) (com.basic.tnull (mk_mono())) p
		| _ -> error "Unsupported constant" p

	let make_default com t p = match follow t with
		| TAbstract({a_path=[],"Int"},_) -> make_int com 0 p
		| _ -> make_null t p

end

(* Static extensions for classes *)
module ExtClass = struct
	let add_static_init c e = match c.cl_init with
		| None -> c.cl_init <- Some e
		| Some e' -> c.cl_init <- Some (concat e' e)
end

(* Static extensions for types *)
module ExtType = struct

	type type_kind =
		| KBool
		| KInteger of int
		| KFloat
		| KNullBool
		| KNullInteger of int
		| KNullFloat
		| KString
		| KPointer of type_kind
		| KStructure
		| KFunction
		| KVoid
		| KNamed of string
		| KDynamic
		| KTypeParameter
		| KVarArgs

	type cast_kind =
		| CastNone
		| CastError
		| CastNext of cast_kind * cast_kind
		| CastInteger
		| CastIntegerToFloat
		| CastFloatToInteger
		| CastIntegerToPointer
		| CastPointerToInteger
		| CastPointerToPointer of cast_kind
		| CastBox
		| CastUnbox
		| CastToDynamic
		| CastFromDynamic

	let rec string_of_type_kind = function
		| KBool -> "KBool"
		| KInteger i -> "KInteger " ^ (string_of_int i)
		| KFloat -> "KFloat"
		| KNullBool -> "KNullBool"
		| KNullInteger i -> "KNullInteger " ^ (string_of_int i)
		| KNullFloat -> "KNullFloat"
		| KString -> "KString"
		| KPointer k -> "KPointer " ^ (string_of_type_kind k)
		| KStructure -> "KStructure"
		| KFunction -> "KFunction"
		| KVoid -> "KVoid"
		| KNamed s -> "KNamed " ^ s
		| KDynamic -> "KDynamic"
		| KTypeParameter -> "KTypeParameter"
		| KVarArgs -> "KVarArgs"

	let rec classify_type t = match follow t with
		| TInst({cl_kind = Type.KTypeParameter _},_) ->
			KPointer KTypeParameter
		| TInst(c,tl) ->
			KPointer (KNamed (s_type_path c.cl_path))
		| TEnum(en,tl) ->
			KPointer (KNamed (s_type_path en.e_path))
		| TAbstract(a,tl) ->
			begin match a.a_path with
			| [],"Int" ->
				if Type.is_null t then
					KNullInteger 32
				else
					KInteger 32
			| [],"Float" ->
				if Type.is_null t then
					KNullFloat
				else
					KFloat
			| [],"Bool" ->
				if Type.is_null t then
					KNullBool
				else
					KBool
			| [],"Void" ->
				KVoid
			| ["c"],("ConstPointer" | "Pointer") ->
				KPointer (classify_type (List.hd tl))
			| ["c"],"VarArg" ->
				KVarArgs
			| _ ->
				classify_type (Abstract.get_underlying_type a tl)
			end
		| TFun _ ->
			KFunction
		| TAnon _ ->
			KStructure
		| TDynamic _ | TMono _ ->
			KDynamic
		| TLazy _ | TType _ ->
			assert false

	let rec cast_type k1 k2 = match k1,k2 with
		| KInteger i1,KInteger i2 ->
			if i1 = i2 then CastNone else CastInteger
		| KInteger _,KFloat ->
			CastIntegerToFloat
		| KFloat,KInteger _ ->
			CastFloatToInteger
		| (KInteger _ | KFloat | KBool),KPointer KTypeParameter ->
			CastBox
		| KPointer KTypeParameter,(KInteger _ | KFloat | KBool) ->
			CastUnbox
		| KInteger i1,KNullInteger i2 ->
			if i1 = i2 then
				CastBox
			else
				CastNext(CastInteger,CastBox)
		| KNullInteger i1,KInteger i2 ->
			if i1 = i2 then
				CastUnbox
			else
				CastNext(CastInteger,CastUnbox)
		| KFloat,KNullFloat ->
			CastBox
		| KNullFloat,KFloat ->
			CastUnbox
		| KInteger _,KNullFloat ->
			CastNext(CastIntegerToFloat,CastBox)
		| KPointer _,KInteger _ ->
			CastIntegerToPointer
		| KInteger _,KPointer _ ->
			CastPointerToInteger
		| KPointer k1,KPointer k2 ->
			CastPointerToPointer(cast_type k1 k2)
		| (KNullFloat | KNullInteger _ | KNullBool),KVarArgs ->
			CastUnbox
		| (KInteger _ | KFloat | KBool),KVarArgs ->
			CastNone
		| KNamed _,KNamed _
		| KDynamic,KDynamic
		| KVoid,KVoid
		| KFloat,KFloat
		| KFunction,KFunction ->
			CastNone
		| KDynamic,_ ->
			CastFromDynamic
		| _,KDynamic ->
			CastToDynamic
		| _ ->
			CastError

	let is_runtime_string_type t = match follow t with
		| TInst({cl_path=[],"String"},_) -> true
		| _ -> false

	let is_native_string_type t = match follow t with
		| TAbstract({a_path = ["c"],"ConstPointer"},[TAbstract({a_path=[],"hx_char"},_)])
		| TAbstract({a_path=["c"],"VarArg"},_) ->
			true
		| _ ->
			false

	let is_int_type t = match follow t with
		| TAbstract({a_path=[],"Int"},_) -> true
		| _ -> false

	let is_type_parameter t = match follow t with
		| TInst({cl_kind = Type.KTypeParameter _},_) -> true
		| _ -> false

	let is_var_args_type t = match follow t with
		| TInst({cl_path = [],"Array"},[t]) ->
			begin match follow t with
				| TAbstract({a_path=["c"],"VarArg"},_) -> true
				| _ -> false
			end
		| _ ->
			false

	let is_void_type t = match follow t with
		| TAbstract({a_path=[],"Void"},_) -> true
		| _ -> false

	let iseq_strict a b =
		try
			type_eq EqDoNotFollowNull a b;
			true
		with Unify_error _ ->
			false
end

(* Static extensions for functions *)
module ExtFunction = struct
	let get_closure_environment tf =
		let v_env = alloc_var "env" (mk_mono()) in
		let e_env p = mk (TLocal v_env) v_env.v_type p in
		let mk_acc cf p = mk (TField(e_env p,FAnon cf)) cf.cf_type p in
		let known = ref PMap.empty in
		let env = ref PMap.empty in
		let fl = ref [] in
		let use e v =
			if PMap.mem v.v_id !known then
				e
			else try
				let cf = PMap.find v.v_name !env in
				mk_acc cf e.epos
			with Not_found ->
				let cf = mk_field v.v_name v.v_type e.epos in
				env := PMap.add v.v_name cf !env;
				fl := (v.v_name,e) :: !fl;
				mk_acc cf e.epos
		in
		let declare v = known := PMap.add v.v_id true !known in
		let rec loop e = match e.eexpr with
			| TVar(v,eo) ->
				let eo = match eo with
					| None -> None
					| Some e -> Some (loop e)
				in
				declare v;
				{e with eexpr = TVar(v,eo)}
			| TLocal v ->
				use e v;
			| TFunction tf ->
				List.iter (fun (v,_) -> declare v) tf.tf_args;
				{e with eexpr = TFunction {tf with tf_expr = loop tf.tf_expr}}
			| _ ->
				Type.map_expr loop e
		in
		List.iter (fun (v,_) -> declare v) tf.tf_args;
		let e = loop tf.tf_expr in
		let an = {
			a_fields = !env;
			a_status = ref Closed
		} in
		unify v_env.v_type (TAnon an);
		let e_obj = mk (TObjectDecl !fl) (TAnon an) e.epos in
		e,v_env,e_obj

	let apply_default_values com tf p =
		let el = match tf.tf_expr with
			| {eexpr = TBlock el} -> el
			| e -> [e]
		in
		let args,el = List.fold_left (fun (args,el) (v,cto) -> match cto with
			| Some ct when ct <> TNull ->
				let v' = if not (is_nullable v.v_type) then
					alloc_var v.v_name (com.basic.tnull v.v_type)
				else
					v
				in
				let e_null = ExprBuilder.make_null v'.v_type p in
				let e_v = ExprBuilder.make_local v p in
				let e_v' = ExprBuilder.make_local v' p in
				let e_cond = mk (TBinop(OpEq,e_v',e_null)) com.basic.tbool p in
				let e_ct = ExprBuilder.make_const_texpr com ct p in
				let e_then = mk (TBinop(OpAssign,e_v,e_ct)) e_ct.etype p in
				let el = if v == v' then begin
					let e_if = mk (TIf(e_cond,e_then,None)) com.basic.tvoid p in
					e_if :: el
				end else begin
					let e_else = mk (TBinop(OpAssign,e_v,e_v')) e_v'.etype p in
					let e_if = mk (TIf(e_cond,e_then,Some e_else)) com.basic.tvoid p in
					let e_v = mk (TVar(v,None)) com.basic.tvoid p in
					e_v :: e_if :: el
				end in

				((v',cto) :: args),el
			| _ ->
				((v,cto) :: args),el
		) ([],el) (List.rev tf.tf_args) in
		{tf with tf_args = args; tf_expr = {tf.tf_expr with eexpr = TBlock el}}
end

module TypeChecker = struct
	let make_adapter_function e tl2 tr2 =
		let tl1,tr1 = match follow e.etype with
			| TFun(tl,tr) -> tl,tr
			| t -> error ("Unsupported assignment of a function to " ^ (s_type (print_context()) t)) e.epos
		in
		let vl = List.map (fun (n,o,t) ->
			alloc_var n t
		) tl2 in
		let evl = List.map2 (fun v (_,_,t) ->
			let e = mk (TLocal v) v.v_type e.epos in
			let e = mk (TCast(e,None)) t e.epos in
			e
		) vl tl1 in
		let e = mk (TCall(e,evl)) tr2 e.epos in
		let e = if ExtType.is_void_type tr2 then e else mk (TReturn (Some e)) t_dynamic e.epos in
		let tf = {
			tf_args = List.map (fun v -> v,None) vl;
			tf_type = tr2;
			tf_expr = e;
		} in
		mk (TFunction tf) (TFun(tl2,tr2)) e.epos

	let rec run com e t =
		(* if is_main e.epos then Printf.printf "RUN %s\nAND %s\n" (s_expr_ast true "" (s_type (print_context())) e) (s_type (print_context()) t); *)
		let e = match e.eexpr,follow t with
		| TCall({eexpr = TField(_,FStatic({cl_path = [],"String"}, {cf_name = "HX_STR"}))},[{eexpr = TConst (TString _)} as e]),_ when ExtType.is_native_string_type t ->
			e
		| _,(TAbstract({a_path=["c"],"VarArg"},_)) when ExtType.is_runtime_string_type e.etype ->
			ExprBuilder.make_library_call com ([],"String") "raw" [e] e.epos
		| _,(TFun(tl,tr) as t2) when not (ExtType.iseq_strict e.etype t2) ->
			Codegen.UnificationCallback.run (run com) (make_adapter_function e tl tr)
		| TConst TNull,_ when not (is_null t) ->
			e
		| _,(TAbstract({a_path=[],"Float"},_)) when ExtType.is_int_type e.etype ->
			let e = mk (TCast(e,None)) t e.epos in
			e
		| _ ->
			if not (ExtType.iseq_strict e.etype t) then
				mk (TCast(e,None)) t e.epos
			else
				e
		in
		(* if is_main e.epos then Printf.printf "YIELD %s\n" (s_expr_ast true "" (s_type (print_context())) e); *)
		e
end

module StringHandler = struct
	let rec run com e = match e.eexpr with
		| TConst (TString s) ->
			ExprBuilder.make_library_call com ([],"String") "HX_STR" [mk (TCast(e,None)) e.etype e.epos] e.epos
		| _ ->
			Type.map_expr (run com) e
end

module Specializer = struct

	let has_spec_meta m = has_meta (Meta.Custom ":specialize") m

	let find_specialized_field c cf stat postfix =
		let cf_name = cf.cf_name ^ postfix in
		let cf = try PMap.find cf_name (if stat then c.cl_statics else c.cl_fields) with Not_found ->
			print_endline ("didn't find " ^ cf_name ^ " in " ^ (snd c.cl_path));
			assert false
		in
		cf

	let get_cl_path t = (match t with | TInst(c,_) -> c.cl_path | _ -> ([],"") )

	let specialize_function c cf cf_expr cf_name_postfix tps =
		let m = PMap.empty in
		let m = List.fold_left (fun m (t_src,t_dest) -> PMap.add (get_cl_path t_src) t_dest m) m tps in
		let ft t = let rec loop t = (match t with
				| TInst({cl_path=path;cl_kind=KTypeParameter _},_) ->
					let t_dest = ( try PMap.find path m with Not_found ->
					t ) in t_dest
				| _ -> Type.map loop t)
				in loop t
		in
		let vars = Hashtbl.create 0 in
		let fv v =
			try Hashtbl.find vars v.v_id with Not_found ->
			let v2 = Type.alloc_var v.v_name (ft v.v_type) in
			v2.v_meta <- v.v_meta;
			Hashtbl.add vars v.v_id v2;
			v2
		in
		let rec map_expr te = match te with | _ -> Type.map_expr_type map_expr ft fv te in
		let cf_expr = map_expr cf_expr in
		let cf_type = ft cf.cf_type in
		{ cf with
			cf_expr = Some(cf_expr);
			cf_type = cf_type;
			cf_name = cf.cf_name ^ cf_name_postfix;
			cf_meta = List.filter (fun (m,_,_) -> match m with Meta.Custom ":specialize" -> false | _ -> true) cf.cf_meta;
		}

	let _specialize_class c t_dest_l =
		let spec_cf_l = List.fold_left (fun cfl cf ->
			(match c.cl_params,cf.cf_kind,cf.cf_expr with
				| [_,t_src],(Method _),Some(cf_expr) when has_spec_meta cf.cf_meta ->
					(List.map ( fun (t_dest,postfix) ->
						let cf = specialize_function c cf cf_expr postfix [(t_src,t_dest)] in cf
					) t_dest_l ) @ cfl
				| _ -> cfl )) [] c.cl_ordered_fields in
		c.cl_ordered_fields <- spec_cf_l @ c.cl_ordered_fields;
		c.cl_fields <- List.fold_left ( fun cfm cf -> PMap.add cf.cf_name cf cfm ) c.cl_fields spec_cf_l;
		let spec_cf_l = List.fold_left (fun cfl cf ->
			(match cf.cf_params,cf.cf_kind,cf.cf_expr with
				| [_,t_src],(Method _),Some(cf_expr) when has_spec_meta cf.cf_meta ->
					(List.map ( fun (t_dest,postfix) ->
						let cf = specialize_function c cf cf_expr postfix [(t_src,t_dest)] in cf
					) t_dest_l ) @ cfl
				| _ -> cfl )) [] c.cl_ordered_statics in
		c.cl_ordered_statics <- spec_cf_l @ c.cl_ordered_statics;
		c.cl_statics <- List.fold_left ( fun cfm cf -> PMap.add cf.cf_name cf cfm ) c.cl_statics spec_cf_l

	let specialize_class c l =
		let _ = _specialize_class c l in
		()

end

module ClassPreprocessor = struct
	let create_vtable c =
		let overridden = ref PMap.empty in
		let rec loop c =
			begin match c.cl_super with
			| None -> ()
			| Some(c,_) -> loop c
			end;
			List.iter (fun cf ->
				if Meta.has (Meta.Custom ":overridden") cf.cf_meta then
					overridden := PMap.add cf.cf_name (c,cf) !overridden
			) c.cl_ordered_fields
		in
		loop c;
		if PMap.is_empty !overridden then
			None
		else begin
			let ta = {
				a_fields = PMap.foldi (fun n (_,cf) acc -> PMap.add n cf acc) !overridden PMap.empty;
				a_status = ref Const;
			} in
			let cf = mk_field cf_name_vtable (TAnon ta) c.cl_pos in
			c.cl_ordered_fields <- cf :: c.cl_ordered_fields;
			c.cl_fields <- PMap.add cf.cf_name cf c.cl_fields;
			Some (cf,!overridden)
		end

	let handle_constructor com c cf =
		let p = c.cl_pos in
		let tl = List.map snd c.cl_params in
		let e_ctor = cf.cf_expr in
		let tf = match e_ctor with
			| Some {eexpr = TFunction tf} -> tf
			| _ -> error "Invalid constructor, expected function" cf.cf_pos
		in
		let t = match follow cf.cf_type with
			(* we have to do this by hand here *)
			| TFun(args,tr) ->
				cf.cf_type <- TFun(args,TInst(c,tl));
				TFun(args @ ["this",false,TInst(c,tl)],com.basic.tvoid)
			| _ -> assert false
		in
		let cf_init = mk_field cf_name_init t cf.cf_pos in
		cf_init.cf_kind <- Method MethNormal;
		cf_init.cf_expr <- e_ctor;
		c.cl_ordered_fields <- cf_init :: c.cl_ordered_fields;
		c.cl_fields <- PMap.add cf_init.cf_name cf_init c.cl_fields;

		let e_this = mk (TConst TThis) (TInst(c,tl)) p in
		let e_init = mk (TField(e_this,FInstance(c,tl,cf_init))) cf_init.cf_type p in
		let el = List.map (fun (v,_) -> mk (TLocal v) v.v_type cf.cf_pos) tf.tf_args in
		let e_init_call = mk (TCall(e_init,el)) com.basic.tvoid p in
		let e_return = mk (TReturn (Some e_this)) e_this.etype p in
		let el = e_init_call :: e_return :: [] in
		let el = match create_vtable c with
			| None ->
				el
			| Some (cf_vTable,overridden) ->
				let fl = PMap.fold (fun (c,cf) acc ->
					let e_field = mk (TField(e_this,FClosure((Some(c,List.map snd c.cl_params),cf)))) cf.cf_type p in
					(cf.cf_name,e_field) :: acc
				) overridden [] in
				let e_obj = mk (TObjectDecl fl) cf_vTable.cf_type p in
				let e_vTable = mk (TField(e_this,FInstance(c,tl,cf_vTable))) cf_vTable.cf_type p in
				let e_assign = mk (TBinop(OpAssign,e_vTable,e_obj)) e_obj.etype p in
				e_assign :: el
		in
		let e = mk (TFunction {tf with tf_type = TInst(c,tl); tf_expr = mk (TBlock el) com.basic.tvoid p}) cf.cf_type cf.cf_pos in
		cf.cf_expr <- Some e

	(* Transforms dynamic methods into var fields and assigns their default
	   implementation as static init. *)
	let transform_dynamic_method com stat c cf =
		let p = cf.cf_pos in
		let cf_default = {cf with cf_kind = Method MethNormal; cf_name = cf.cf_name ^ "_default"} in
		let add_member_init e = match c.cl_constructor with
			| Some ({cf_expr = Some ({eexpr = TFunction tf} as ef)} as cf) ->
				cf.cf_expr <- Some ({ef with eexpr = TFunction {tf with tf_expr = concat e tf.tf_expr}})
			| _ ->
				failwith "Constructor is not a function (wtf?)"
		in
		match cf.cf_kind with
		| Method MethDynamic ->
			if stat then begin
				let ethis = ExprBuilder.make_static_this c p in
				let ef1 = mk (TField(ethis,FStatic(c,cf))) cf.cf_type p in
				let ef2 = mk (TField(ethis,FStatic(c,cf_default))) cf_default.cf_type p in
				let e_assign = mk (TBinop(OpAssign,ef1,ef2)) ef2.etype p in
				ExtClass.add_static_init c e_assign;
				c.cl_ordered_statics <- cf_default :: c.cl_ordered_statics;
				c.cl_statics <- PMap.add cf_default.cf_name cf_default c.cl_statics
			end else begin
				let tl = List.map snd c.cl_params in
				let ethis = mk (TConst TThis) (TInst(c,tl)) p in
				let ef1 = mk (TField(ethis,FInstance(c,tl,cf))) cf.cf_type p in
				let ef2 = mk (TField(ethis,FClosure(Some(c,tl),cf_default))) cf_default.cf_type p in
				let e_assign = mk (TBinop(OpAssign,ef1,ef2)) ef2.etype p in
				add_member_init e_assign;
				c.cl_ordered_fields <- cf_default :: c.cl_ordered_fields;
				c.cl_fields <- PMap.add cf_default.cf_name cf_default c.cl_fields
			end;
			cf.cf_expr <- None;
			cf.cf_kind <- Var {v_read = AccNormal; v_write = AccNormal};
		| _ ->
			()

	(* Appends the `this` type to all member fields. *)
	let lift_member_field c cf  = match cf.cf_kind,follow cf.cf_type with
		| Method _,TFun(tl,tr) ->
			let t = TInst(c,List.map snd c.cl_params) in
			let tl = tl @ ["this",false,t] in
			cf.cf_type <- TFun(tl,tr);
		| _ ->
			()

	let infer_null_argument com cf =
		match cf.cf_expr,follow cf.cf_type with
		| Some ({eexpr = TFunction tf} as e),TFun(tl,tr) ->
			let tf = ExtFunction.apply_default_values com tf cf.cf_pos in
			let tl = List.map2 (fun (v,_) (n,o,_) -> n,o,v.v_type) tf.tf_args tl in
			cf.cf_type <- TFun(tl,tr);
			cf.cf_expr <- Some ({e with eexpr = TFunction tf; etype = cf.cf_type});
		| _ ->
			()

	let prepare_class com c =
		List.iter (infer_null_argument com) c.cl_ordered_statics;
		List.iter (infer_null_argument com) c.cl_ordered_fields;
		begin match c.cl_constructor with
			| None -> ()
			| Some cf -> infer_null_argument com cf
		end;
		List.iter (transform_dynamic_method com true c) c.cl_ordered_statics;
		List.iter (transform_dynamic_method com false c) c.cl_ordered_fields;
		List.iter (lift_member_field c) c.cl_ordered_fields;
		begin match c.cl_constructor with
			| None -> ()
			| Some cf -> handle_constructor com c cf
		end
end