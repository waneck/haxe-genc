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
and gclass_t = int
and gfunc_t = (Type.tfunc * gexpr_t)
and gfield_access_t = int
and gmodule_type_t = int
and gdecision_tree_t = int
and genum_field_t = int
and gnode_t =
	| GNone
	| GWhatever of (int * gexpr_t)

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


let collect_block_info e =
	let rec f (gctx,ctx) e : gexpr_t = match e.gexpr with
	| GBlock el ->
		let _,nctx = (gctx,{ blocks = [] }) in
		let _ = List.map (f (gctx,nctx)) el in
		let id = gctx.blockid in
		let _  = gctx.blockid <- id + 1 in
		let data = GDBlockInfo (id, nctx.blocks ) in
		let _ = ctx.blocks <- data :: ctx.blocks in
		{ e with gdata = data }

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
