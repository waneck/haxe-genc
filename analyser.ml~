open Ast
open Type


type gconstant_t = int
and gtype_t = int
and gvar_t  = Type.tvar
and ganon_t = int
and gclass_t = int
and gfunc_t = (Type.tfunc * gexpr_t)
and gfield_access_t = int
and gmodule_type_t = int
and gdecision_tree_t = int
and genum_field_t = int

and gexpr_t =
	| GE of (texpr * gexpr_expr_t)
	| GMergeBlock of (texpr * gexpr_expr_t) list


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

let fdefault v:'a = 0
let id       v:'a = v
let ftype          = fdefault
let fconstant      = fdefault
let fvar           = id
let fexpr          = fdefault
let fanon          = fdefault
let fclass         = fdefault
let ffield_access  = fdefault
let fmodule_type   = fdefault
let fdecision_tree = fdefault
let fenum_field    = fdefault

let map_expr f  ( e : Type.texpr ) =
	match e.eexpr with
	| TConst v ->  GE (e, GConst (fconstant v) )
	| TLocal v ->  GE (e, GLocal (fvar v) )
	| TBreak   ->  GE (e, GBreak )
	| TContinue -> GE (e, GContinue )
	| TTypeExpr mt -> GE (e, GTypeExpr (fmodule_type mt))
	| TArray (e1,e2) ->
		GE (e,  GArray (f e1,f e2) )
	| TBinop (op,e1,e2) ->
		GE (e,  GBinop (op,f e1,f e2) )
	| TFor (v,e1,e2) ->
		GE (e,  GFor (fvar v,f e1,f e2) )
	| TWhile (e1,e2,flag) ->
		GE (e,  GWhile (f e1,f e2,flag) )
	| TThrow e1 ->
		GE (e,  GThrow (f e1) )
	| TEnumParameter (e1,ef,i) ->
		GE (e,  GEnumParameter(f e1,fenum_field ef,i) )
	| TField (e1,v) ->
		GE (e,  GField (f e1, ffield_access v) )
	| TParenthesis e1 ->
		GE (e,  GParenthesis (f e1) )
	| TUnop (op,pre,e1) ->
		GE (e,  GUnop (op,pre,f e1) )
	| TArrayDecl el ->
		GE (e,  GArrayDecl (List.map f el) )
	| TNew (t,pl,el) ->
		GE (e,  GNew (fclass t,pl,List.map f el) )
	| TBlock el ->
		GE (e,  GBlock (List.map f el) )
	| TObjectDecl el ->
		GE (e,  GObjectDecl (List.map (fun (v,e) -> v, f e) el) )
	| TCall (e1,el) ->
		GE (e,  GCall (f e1, List.map f el) )
	| TVars vl ->
		GE (e,  GVars (List.map (fun (v,e) -> fvar v , match e with None -> None | Some e -> Some (f e)) vl) )
	| TFunction tf ->
		GE (e,  GFunction (tf, f tf.tf_expr))
	| TIf (ec,e1,e2) ->
		GE (e,  GIf (f ec,f e1,match e2 with None -> None | Some e -> Some (f e)) )
	| TSwitch (e1,cases,def) ->
		GE (e,  GSwitch (f e1, List.map (fun (el,e2) -> List.map f el, f e2) cases, match def with None -> None | Some e -> Some (f e)) )
	| TPatMatch dt ->
		GE (e,  GPatMatch( fdecision_tree dt ))
	| TTry (e1,catches) ->
		GE (e,  GTry (f e1, List.map (fun (v,e) -> fvar v, f e) catches) )
	| TReturn eo ->
		GE (e,  GReturn (match eo with None -> None | Some e -> Some (f e)) )
	| TCast (e1,t) ->
		GE (e,  GCast (f e1, match t with None -> None | Some mt -> Some (fmodule_type mt)) )
	| TMeta (m,e1) ->
		GE (e,  GMeta(m,f e1) )

let map_gexpr f ge : gexpr_t = match ge with
	| GE(te, e)-> (match e with
		| GConst _
		| GLocal _
		| GBreak
		| GContinue
		| GTypeExpr _ ->
			ge
		| GArray (e1,e2) ->
			GE (te,  GArray (f e1,f e2) )
		| GBinop (op,e1,e2) ->
			GE (te,  GBinop (op,f e1,f e2) )
		| GFor (v,e1,e2) ->
			GE (te,  GFor ( v,f e1,f e2) )
		| GWhile (e1,e2,flag) ->
			GE (te,  GWhile (f e1,f e2,flag) )
		| GThrow e1 ->
			GE (te,  GThrow (f e1) )
		| GEnumParameter (e1,ef,i) ->
			GE (te,  GEnumParameter(f e1, ef,i) )
		| GField (e1,v) ->
			GE (te,  GField (f e1, v) )
		| GParenthesis e1 ->
			GE (te,  GParenthesis (f e1) )
		| GUnop (op,pre,e1) ->
			GE (te,  GUnop (op,pre,f e1) )
		| GArrayDecl el ->
			GE (te,  GArrayDecl (List.map f el) )
		| GNew (t,pl,el) ->
			GE (te,  GNew (fclass t,pl,List.map f el) )
		| GBlock el ->
			GE (te,  GBlock (List.map f el) )
		| GObjectDecl el ->
			GE (te,  GObjectDecl (List.map (fun (v,e) -> v, f e) el) )
		| GCall (e1,el) ->
			GE (te,  GCall (f e1, List.map f el) )
		| GNVar _ ->
			ge
		| GSVar(v, e) ->
			GE(te, GSVar (v, f e))
		| GVars vl ->
			GE (te,  GVars (List.map (fun (v,e) -> fvar v , match e with None -> None | Some e -> Some (f e)) vl) )
		| GFunction (tf, e) ->
			GE (te,  GFunction (tf, f e))
		| GIf (ec,e1,e2) ->
			GE (te,  GIf (f ec,f e1,match e2 with None -> None | Some e -> Some (f e)) )
		| GSwitch (e1,cases,def) ->
			GE (te,  GSwitch (f e1, List.map (fun (el,e2) -> List.map f el, f e2) cases, match def with None -> None | Some e -> Some (f e)) )
		| GPatMatch dt ->
			GE (te,  GPatMatch( dt ))
		| GTry (e1,catches) ->
			GE (te,  GTry (f e1, List.map (fun (v,e) -> fvar v, f e) catches) )
		| GReturn eo ->
			GE (te,  GReturn (match eo with None -> None | Some e -> Some (f e)) )
		| GCast (e1,t) ->
			GE (te,  GCast (f e1, match t with None -> None | Some mt -> Some (mt)) )
		| GMeta (m,e1) ->
			GE (te,  GMeta(m,f e1) )
		)
	| _ -> ge

let fold_gexpr (f : 'a -> gexpr_t -> 'a) (acc : 'a) ( ge : gexpr_t)  : 'a = match ge with
	| GE(te, e) -> (match e with
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
		)
	| _ -> acc

let iter_gexpr f ge : unit = match ge with
	| GE(te, e) -> (match e with
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
		)
	| _ -> ()

let gexpr_of_texpr e =
	let rec f e = match e.eexpr with
	_ -> map_expr f e
	in f e

let fold_tree e =
	let rec f acc ge : int =
		let acc = acc+1 in
		match ge with
		| GE(te, e) -> (match e with
			| _ -> fold_gexpr f acc ge
			)
		| _ -> acc
	in f 0 e
