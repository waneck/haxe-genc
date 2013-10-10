open Ast
open Type

	let assigns_to_trace = ref false

	let rec run e =
		match e.eexpr with
		| TBinop(OpAssign, {eexpr = TField(_,FStatic({cl_path=["haxe"],"Log"}, {cf_name = "trace"}))}, _) ->
			assigns_to_trace := true
		| _ ->
			Type.iter run e


type gconstant_t = int
and gtype_t = int
and gvar_t  = Type.tvar
and ganon_t = int
and gclass_t = Type.tclass
and gfunc_t = (Type.tfunc * gexpr_t)
and gfield_access_t = tfield_access
and gmodule_type_t = int
and gdecision_tree_t = int
and genum_field_t = int
and gnode_t =
	| GNone
	| GWhatever of (int * gexpr_t)

and gdata_value =
	| GDInst of int * tclass
	| GDEnum of int * tenum
	| GDAnon of int * tanon

and gdata_t =
	| GDNone
	| GDBlockInfo of ( int * gdata_t list )

and gexpr_t = {
	g_te  : Type.texpr;
	gtype : Type.t;
	gexpr : gexpr_expr_t;
	gdata : gdata_t;
}
	(*| GE of (texpr * gexpr_expr_t)
	| GMergeBlock of (texpr * gexpr_expr_t) list*)


and gexpr_expr_t    =
	| GConst of gconstant_t
	| GLocal of gvar_t
	| GArray of gexpr_t * gexpr_t
	| GBinop of Ast.binop * gexpr_t * gexpr_t
	| GField of gexpr_t * gfield_access_t
	| GTypeExpr of gmodule_type_t
	| GParenthesis of gexpr_t
	| GObjectDecl of (string * gexpr_t) list
	| GArrayDecl of gexpr_t list
	| GCall of gexpr_t * gexpr_t list
	| GNew of gclass_t * tparams * gexpr_t list
	| GUnop of Ast.unop * Ast.unop_flag * gexpr_t
	| GFunction of gfunc_t
	| GVars of (gvar_t * gexpr_t option) list
	| GSVar of (gvar_t * gexpr_t)
	| GNVar of  gvar_t
	| GBlock of gexpr_t list
	| GFor of gvar_t * gexpr_t * gexpr_t
	| GIf of gexpr_t * gexpr_t * gexpr_t option
	| GWhile of gexpr_t * gexpr_t * Ast.while_flag
	| GSwitch of gexpr_t * (gexpr_t list * gexpr_t) list * gexpr_t option
	| GPatMatch of gdecision_tree_t
	| GTry of gexpr_t * (gvar_t * gexpr_t) list
	| GReturn of gexpr_t option
	| GBreak
	| GContinue
	| GThrow of gexpr_t
	| GCast of gexpr_t * gmodule_type_t option
	| GMeta of metadata_entry * gexpr_t
	| GEnumParameter of gexpr_t * genum_field_t * int
	| GNode of gnode_t



let fdefault v:'a = 0
let fid      v:'a = v
let ftype          = fdefault
let fconstant      = fdefault
let fvar           = fid
let fexpr          = fdefault
let fanon          = fdefault
let fclass         = fid
let ffield_access  = fid
let fmodule_type   = fdefault
let fdecision_tree = fdefault
let fenum_field    = fdefault

let gdata_default () = GDNone

let map_expr f  ( e : Type.texpr ) =
	let te = e in
	match e.eexpr with
	| TConst v ->  { g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =  GConst (fconstant v) }
	| TLocal v ->  { g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =  GLocal (fvar v) }
	| TBreak   ->  { g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =  GBreak }
	| TContinue -> { g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =  GContinue }
	| TTypeExpr mt -> { g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =  GTypeExpr (fmodule_type mt) }
	| TArray (e1,e2) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GArray (f e1,f e2) }
	| TBinop (op,e1,e2) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GBinop (op,f e1,f e2) }
	| TFor (v,e1,e2) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GFor (fvar v,f e1,f e2) }
	| TWhile (e1,e2,flag) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GWhile (f e1,f e2,flag) }
	| TThrow e1 ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GThrow (f e1) }
	| TEnumParameter (e1,ef,i) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GEnumParameter(f e1,fenum_field ef,i) }
	| TField (e1,v) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GField (f e1, ffield_access v) }
	| TParenthesis e1 ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GParenthesis (f e1) }
	| TUnop (op,pre,e1) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GUnop (op,pre,f e1) }
	| TArrayDecl el ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GArrayDecl (List.map f el) }
	| TNew (t,pl,el) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GNew (fclass t,pl,List.map f el) }
	| TBlock el ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GBlock (List.map f el) }
	| TObjectDecl el ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GObjectDecl (List.map (fun (v,e) -> v, f e) el) }
	| TCall (e1,el) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GCall (f e1, List.map f el) }
	| TVars vl ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GVars (List.map (fun (v,e) -> fvar v , match e with None -> None | Some e -> Some (f e)) vl) }
	| TFunction tf ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GFunction (tf, f tf.tf_expr) }
	| TIf (ec,e1,e2) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GIf (f ec,f e1,match e2 with None -> None | Some e -> Some (f e)) }
	| TSwitch (e1,cases,def) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GSwitch (f e1, List.map (fun (el,e2) -> List.map f el, f e2) cases, match def with None -> None | Some e -> Some (f e)) }
	| TPatMatch dt ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GPatMatch( fdecision_tree dt ) }
	| TTry (e1,catches) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GTry (f e1, List.map (fun (v,e) -> fvar v, f e) catches) }
	| TReturn eo ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GReturn (match eo with None -> None | Some e -> Some (f e)) }
	| TCast (e1,t) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GCast (f e1, match t with None -> None | Some mt -> Some (fmodule_type mt)) }
	| TMeta (m,e1) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GMeta(m,f e1) }

let map_gexpr f e : gexpr_t = match e.gexpr with
		| GConst _
		| GLocal _
		| GBreak
		| GContinue
		| GTypeExpr _ ->
			e
		| GArray (e1,e2) ->
			{ e with gexpr =   GArray (f e1,f e2) }
		| GBinop (op,e1,e2) ->
			{ e with gexpr =   GBinop (op,f e1,f e2) }
		| GFor (v,e1,e2) ->
			{ e with gexpr =   GFor ( v,f e1,f e2) }
		| GWhile (e1,e2,flag) ->
			{ e with gexpr =   GWhile (f e1,f e2,flag) }
		| GThrow e1 ->
			{ e with gexpr =   GThrow (f e1) }
		| GEnumParameter (e1,ef,i) ->
			{ e with gexpr =   GEnumParameter(f e1, ef,i) }
		| GField (e1,v) ->
			{ e with gexpr =   GField (f e1, v) }
		| GParenthesis e1 ->
			{ e with gexpr =   GParenthesis (f e1) }
		| GUnop (op,pre,e1) ->
			{ e with gexpr =   GUnop (op,pre,f e1) }
		| GArrayDecl el ->
			{ e with gexpr =   GArrayDecl (List.map f el) }
		| GNew (t,pl,el) ->
			{ e with gexpr =   GNew (fclass t,pl,List.map f el) }
		| GBlock el ->
			{ e with gexpr =   GBlock (List.map f el) }
		| GObjectDecl el ->
			{ e with gexpr =   GObjectDecl (List.map (fun (v,e) -> v, f e) el) }
		| GCall (e1,el) ->
			{ e with gexpr =   GCall (f e1, List.map f el) }
		| GNVar _ ->
			e
		| GSVar(v, e) ->
			{ e with gexpr =   GSVar (v, f e) }
		| GVars vl ->
			{ e with gexpr =   GVars (List.map (fun (v,e) -> fvar v , match e with None -> None | Some e -> Some (f e)) vl) }
		| GFunction (tf, e) ->
			{ e with gexpr =   GFunction (tf, f e) }
		| GIf (ec,e1,e2) ->
			{ e with gexpr =   GIf (f ec,f e1,match e2 with None -> None | Some e -> Some (f e)) }
		| GSwitch (e1,cases,def) ->
			{ e with gexpr =   GSwitch (f e1, List.map (fun (el,e2) -> List.map f el, f e2) cases, match def with None -> None | Some e -> Some (f e)) }
		| GPatMatch dt ->
			{ e with gexpr =   GPatMatch( dt ) }
		| GTry (e1,catches) ->
			{ e with gexpr =   GTry (f e1, List.map (fun (v,e) -> fvar v, f e) catches) }
		| GReturn eo ->
			{ e with gexpr =   GReturn (match eo with None -> None | Some e -> Some (f e)) }
		| GCast (e1,t) ->
			{ e with gexpr =   GCast (f e1, match t with None -> None | Some mt -> Some (mt)) }
		| GMeta (m,e1) ->
			{ e with gexpr =   GMeta(m,f e1) }


let fold_gexpr (f : 'a -> gexpr_t -> 'a) (acc : 'a) ( e : gexpr_t)  : 'a = match e.gexpr with
		| GConst _
		| GLocal _
		| GBreak
		| GContinue
		| GTypeExpr _ ->
			acc
		| GArray (e1,e2)
		| GBinop (_,e1,e2)
		| GFor (_,e1,e2)
		| GWhile (e1,e2,_) ->
			let acc = f acc e1 in
			f acc e2
		| GThrow e
		| GField (e,_)
		| GEnumParameter (e,_,_)
		| GParenthesis e
		| GCast (e,_)
		| GUnop (_,_,e)
		| GMeta(_,e) ->
			f acc e
		| GArrayDecl el
		| GNew (_,_,el)
		| GBlock el ->
			List.fold_left (fun acc e -> f acc e) acc el
		| GObjectDecl fl ->
			List.fold_left (fun acc (_,e) -> f acc e) acc fl
		| GCall (e,el) ->
			let acc = f acc e in
			List.fold_left (fun acc e -> f acc e) acc el
		| GVars vl ->
			List.fold_left (fun acc (_,e) -> match e with None -> acc | Some e -> f acc e) acc vl
		| GNVar v -> acc
		| GSVar (v, e) -> f acc e
		| GFunction (tf, e) ->
			f acc e
		| GIf (e,e1,e2) ->
			let acc = f acc e in
			let acc = f acc e1 in
			(match e2 with None -> acc | Some e -> f acc e)
		| GSwitch (e,cases,def) ->
			let acc = f acc e in
			let acc = List.fold_left (fun acc (el,e2) ->
				let acc = List.fold_left (fun acc e-> f acc e) acc el in
				f acc e2 ) acc cases in
			(match def with None -> acc | Some e -> f acc e)
		| GPatMatch dt -> acc
		| GTry (e,catches) ->
			let acc = f acc e in
			List.fold_left (fun acc (_,e) -> f acc e) acc catches
		| GReturn eo ->
			(match eo with None -> acc | Some e -> f acc e)


let iter_gexpr f e : unit = match e.gexpr with
		| GConst _
		| GLocal _
		| GBreak
		| GContinue
		| GTypeExpr _ ->
			()
		| GArray (e1,e2)
		| GBinop (_,e1,e2)
		| GFor (_,e1,e2)
		| GWhile (e1,e2,_) ->
			f e1;
			f e2;
		| GThrow e
		| GField (e,_)
		| GEnumParameter (e,_,_)
		| GParenthesis e
		| GCast (e,_)
		| GUnop (_,_,e)
		| GMeta(_,e) ->
			f e
		| GArrayDecl el
		| GNew (_,_,el)
		| GBlock el ->
			List.iter f el
		| GObjectDecl fl ->
			List.iter (fun (_,e) -> f e) fl
		| GCall (e,el) ->
			f e;
			List.iter f el
		| GVars vl ->
			List.iter (fun (_,e) -> match e with None -> () | Some e -> f e) vl
		| GNVar v -> ()
		| GSVar (v, e) -> f e
		| GFunction (tf, e) ->
			f e
		| GIf (e,e1,e2) ->
			f e;
			f e1;
			(match e2 with None -> () | Some e -> f e)
		| GSwitch (e,cases,def) ->
			f e;
			List.iter (fun (el,e2) -> List.iter f el; f e2) cases;
			(match def with None -> () | Some e -> f e)
		| GPatMatch dt -> ()
		| GTry (e,catches) ->
			f e;
			List.iter (fun (_,e) -> f e) catches
		| GReturn eo ->
			(match eo with None -> () | Some e -> f e)


(* ---------------------------------------------------------------------- *)


(*

	how do values get into scope?

	1. they are constructed in scope.
	2. they are passed via function arguments
	3. they are fields of the class instance
	3. they are static fields



	how are values accessed?
(* 	when accessing a static fieldzdfzd *)

	accessed value id via path from scope id


	access this.field.field
	access typeexpr.field.field.


*)


(* ----------------------------  Interpreter  --------------------------- *)

type gr_id  = int
type gr_tid = int
type gr_iid = int

type gr_func = gexpr_t

and gr_ctx = {
	gr_classes : gr_sclass  DynArray.t;
	gr_enums   : gr_senum   DynArray.t;
	gr_anons   : gr_sanon   DynArray.t;

	gr_iclasses: gr_class   DynArray.t;
	gr_ienums  : gr_enum    DynArray.t;
	gr_ianon   : gr_anon    DynArray.t;
	gr_iclosure: gr_closure DynArray.t;

}

and gr_scope = {
	gsc_id    : gr_id;
	gsc_vars  : gr_value DynArray.t;
}

and gr_function_ctx = {
	mutable gr_  : int;

}

and gr_fields = {
	gf_size   : int;
	gf_values : (int,int) Hashtbl.t;
}

and gr_class = {
	gcl_id     : gr_tid;
	gcl_iid    : gr_iid;
	gcl_vars   : gr_value DynArray.t;
}
and gr_sclass = {
	gcl_tid       : gr_tid;
	gcl_sfields   : gr_value DynArray.t;

	gcl_var_map   : gr_fields;
	gcl_svar_map  : gr_fields;

	gcl_methods   : gr_func DynArray.t;
	gcl_smethods  : gr_func DynArray.t;
}

and gr_closure = {
	gclr_id   : gr_tid;
	gclr_iid  : gr_iid;
	gclr_ctx  : gr_value DynArray.t;
	gclr_func : gr_func;
}

and gr_enum = {
	gen_id       : gr_tid;
	gen_iid      : gr_iid;
	gen_idx      : int;
	gen_fields   : gr_value DynArray.t;
}

and gr_senum = {
	gen_tid     : gr_tid;
	gen_con_map : (int,gr_fields) Hashtbl.t;
}

and gr_anon = {
	ga_id        : gr_tid;
	ga_iid       : gr_iid;
	ga_fields    : gr_value DynArray.t;
}

and gr_sanon = {
	ga_tid      : gr_tid;
	ga_var_map  : gr_fields;
}

and gr_value_t =
	| GRClass   of gr_class
	| GRSClass  of gr_sclass
	| GRAnon    of gr_anon
	| GREnum    of gr_enum
	| GRArray   of int DynArray.t
	| GRClosure of gr_closure
	| GRString  of string
	| GRInt     of Int32.t
	| GRInt64   of Int64.t
	| GRFloat   of string
	| GRBool    of bool
	| GRNull


and gr_value = {
	grv_val   : gr_value_t;
	grv_refs  : gr_value_t list;

}

let gr_null_val = { grv_val = GRNull; grv_refs = []}

let gf_set_field fields idx v =
	DynArray.unsafe_set fields idx v

let gf_get_field fields idx =
	DynArray.unsafe_get fields idx

let gr_set_field ctx self n v = try ( match self with
	| GRClass c ->
		let scl  = DynArray.unsafe_get ctx.gr_classes c.gcl_id in
		let fidx = Hashtbl.find scl.gcl_var_map.gf_values n in
		gf_set_field c.gcl_vars fidx v
	| GRAnon a ->
		let s  = DynArray.unsafe_get ctx.gr_anons a.ga_id in
		let fidx = Hashtbl.find s.ga_var_map.gf_values n in
		gf_set_field a.ga_fields fidx v
	| _ -> assert false
	) with Not_found -> assert false

let gr_get_field ctx self n = try ( match self with
	| GRClass c ->
		let scl  = DynArray.unsafe_get ctx.gr_classes c.gcl_id in
		let fidx = Hashtbl.find scl.gcl_var_map.gf_values n in
		gf_get_field c.gcl_vars fidx
	| GRAnon a ->
		let s  = DynArray.unsafe_get ctx.gr_anons a.ga_id in
		let fidx = Hashtbl.find s.ga_var_map.gf_values n in
		gf_get_field a.ga_fields fidx
	| _ -> assert false
	) with Not_found -> assert false

let gcl_init_class ctx tid =
	let scl  = DynArray.unsafe_get ctx.gr_classes tid in
	let v = {
		gcl_id     = scl.gcl_tid;
		gcl_iid    = DynArray.length ctx.gr_iclasses;
		gcl_vars   = DynArray.init scl.gcl_var_map.gf_size (fun _-> gr_null_val)
	} in
	DynArray.add ctx.gr_iclasses v;
	v

let gen_init_enum ctx tid eidx =
	let s  = DynArray.unsafe_get ctx.gr_enums tid in
	let gfields = Hashtbl.find s.gen_con_map eidx in
	let v = {
		gen_id     = tid;
		gen_iid    = DynArray.length ctx.gr_ienums;
		gen_idx    = eidx;
		gen_fields = DynArray.init gfields.gf_size (fun _-> gr_null_val)
	} in
	DynArray.add ctx.gr_ienums v;
	v


(*
ctx requires:
- this
- super
- current scope
- state
*)

let gr_new_var ctx = ()
	(*ctx.scope*)

let gr_open_scope ctx = ()
	(*let ctx = { ctx with scope = }*)

let gr_close_scope ctx = ()



let gr_getval_var v : gr_value = gr_null_val

let gr_getval_typeexpr : gr_value = gr_null_val

let ctx_open_branch ctx = ()

let ctx_close_branch ctx = ()

(*let eval_gexpr f ctx e : gr_value = match e.gexpr with
	| GConst c -> (*(match c with
		| TInt v   -> GRInt v
		| TFloat v -> GRFloat v
		| TBool v  -> GRBool v
		| TNull    -> GRNull
		| TThis    -> ctx.gr_this
		| TSuper   -> ctx.gr_super
	)*) gr_null_val
	| GLocal v -> gr_null_val
	| GBreak
	| GContinue
	| GTypeExpr _ ->
		GRNull
	| GArray (e1,e2)
	| GBinop (_,e1,e2)
	| GFor (_,e1,e2)
	| GWhile (e1,e2,_) ->
		f e1;
		f e2;
	| GThrow e
	| GField (e,_)
	| GEnumParameter (e,_,_)
	| GParenthesis e
	| GCast (e,_)
	| GUnop (_,_,e)
	| GMeta(_,e) ->
		f e
	| GArrayDecl el
	| GNew (_,_,el)
	| GBlock el ->
		List.iter f el
	| GObjectDecl fl ->
		List.iter (fun (_,e) -> f e) fl
	| GCall (e,el) ->
		f e;
		List.iter f el
	| GVars vl ->
		List.iter (fun (_,e) -> match e with None -> gr_null_val | Some e -> f e) vl
	| GNVar v -> gr_null_val
	| GSVar (v, e) -> f e
	| GFunction (tf, e) ->
		f e

	| GIf (e,e1,e2) ->
		f e;
		f e1;
		(match e2 with None -> gr_null_val | Some e -> f e)

	| GSwitch (e,cases,def) ->
		f e;
		List.iter (fun (el,e2) -> List.iter f el; f e2) cases;
		(match def with None -> gr_null_val | Some e -> f e)
	| GPatMatch dt -> GRNull
	| GTry (e,catches) ->
		f e;
		List.iter (fun (_,e) -> f e) catches
	| GReturn eo ->
		(match eo with None -> GRNull | Some e -> f e)*)


(* ---------------------------------------------------------------------- *)






type blockinfogctx = {
	mutable blockid : int;
}
type blockinfoctx = {
	mutable blocks : gdata_t list;
}

let p_blockinfo xs =
	let rec loop depth x =
		match x with
		| GDBlockInfo (id,xs) ->
			print_endline ("block " ^ (string_of_int id) ^ " cs:" ^
				(String.concat "," (List.map ( fun d -> match d with
					| GDBlockInfo(id,_) -> (string_of_int id)
					| _ -> ""
				) xs )))
		| _ -> ()
	in
	List.iter (loop 0) xs

let s_type t = Type.s_type (print_context()) t
let s_types tl = String.concat ", " (List.map s_type tl)
let s_path (p,n) = (String.concat "." p) ^ "."  ^ n
let s_tparms xs = String.concat ", " (List.map (fun (s,t) -> (s ^ ":" ^(s_type t))) xs)

let s_class c =
    (s_path c.cl_path) ^ "<" ^ (s_tparms c.cl_types) ^">"
let p_call c cf el =
	let _ = print_endline ( "call " ^ (s_path c.cl_path) ^ "." ^ cf.cf_name ^"<"^ (s_tparms c.cl_types) ^">"^ (s_tparms cf.cf_params) ^ "" ) in
	let _ = print_endline (s_types (List.map (fun e -> e.gtype) el)) in
	let _ = (match cf.cf_type with
		| TFun (args,ret) ->
			let _ = print_endline (s_types (List.map (fun (_,_,t) -> t) args)) in ()
		| _ -> ()
		) in
	let _ = print_endline " --- " in
	()

let s_var v =
	(String.concat " " ["var";string_of_int v.v_id;v.v_name;s_type v.v_type])


let p_assign lhs rhs =
	let lhs = match lhs.gexpr with
	| GField (e1,(FInstance(c,cf)|FStatic(c,cf))) ->
		print_endline (String.concat " " ["assign to field:";s_class c;".";cf.cf_name;s_expr_kind e1.g_te;s_type e1.gtype])
	| GLocal v ->
		print_endline (String.concat " " ["assign to local:";string_of_int v.v_id;v.v_name;s_type v.v_type])
	|_ ->
		print_endline (String.concat " " [s_expr s_type lhs.g_te;"=";s_expr s_type rhs.g_te])
	in ()

let collect_block_info e =
	let rec f (gctx,ctx) e : gexpr_t = match e.gexpr with
	| GNew (c, [], el)   -> e
	| GNew (c, tl, el) ->
		let _ = print_endline ( "class " ^ (s_path c.cl_path) ^ "<" ^ (s_types tl) ^ ">" ) in
		let _ = Option.map (fun cf -> p_call c cf el) c.cl_constructor in
		e
	| GCall (e1, el) -> ( match e1.gexpr with
		| GField (_,(FInstance(c,cf)(*|FStatic(c,cf)*))) ->
			let _ = p_call c cf el in
			e
		| _ -> e
		)
	| GBlock el ->
		let _,nctx = (gctx,{ blocks = [] }) in
		let id = gctx.blockid in
		let _  = gctx.blockid <- id + 1 in
		let _ = List.map (f (gctx,nctx)) el in
		let data = GDBlockInfo (id, nctx.blocks ) in
		let _ = ctx.blocks <- data :: ctx.blocks in
		{ e with gdata = data }
	| GBinop (OpAssign,lhs,rhs) ->
			p_assign lhs rhs;
			e
	|_ -> map_gexpr (f (gctx,ctx)) e in
	let gctx,ctx = {blockid = 0},{blocks= []} in
	let _ = f (gctx,ctx) e in
	ctx.blocks

let gexpr_of_texpr e =
	let rec f e = match e.eexpr with
	_ -> map_expr f e
	in f e

let get_field_expressions xs = List.fold_left (fun acc cf ->
		match cf.cf_expr with
		| None -> acc
		| Some {eexpr = TFunction tf} -> tf.tf_expr :: acc
		| Some e -> e :: acc

	) [] xs


let run_analyzer ( mt : Type.module_type list ) : unit =
	print_endline "start";
	List.iter ( fun mt -> match mt with
	| TClassDecl v ->
		let fields  = List.map  gexpr_of_texpr (get_field_expressions v.cl_ordered_statics) in
		let statics = List.map  gexpr_of_texpr (get_field_expressions v.cl_ordered_fields) in
		let _ = List.iter (fun e -> p_blockinfo (collect_block_info e)) fields in
		()
	| TEnumDecl  v -> ()
	| TTypeDecl  v -> ()
	| TAbstractDecl v -> ()
    ) mt;
	print_endline "done."
