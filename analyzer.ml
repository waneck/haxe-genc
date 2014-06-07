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
	| TVar(v,eo) ->
		{ g_te = te; gtype = te.etype; gdata = gdata_default(); gexpr =   GVars ([fvar v , match eo with None -> None | Some e -> Some (f e)]) }
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



(* Simn part starts here *)

open Common
open Type

let s_expr = s_expr (s_type (print_context()))
let debug e = if e.epos.pfile = "src/Main.hx" then print_endline (s_expr e)

module Simplifier = struct
	let mk_block_context com gen_temp =
		let block_el = ref [] in
		let push e = block_el := e :: !block_el in
		let assign ev e =
			(* debug e; *)
			let mk_assign e2 = mk (TBinop(OpAssign,ev,e2)) e2.etype e2.epos in
			let rec loop e = match e.eexpr with
	 			| TBlock el ->
					begin match List.rev el with
						| e1 :: el ->
							let el = List.rev ((loop e1) :: el) in
							{e with eexpr = TBlock el}
						| _ ->
							mk_assign e
					end
				| TIf(e1,e2,eo) ->
					let e2 = loop e2 in
					let eo = match eo with None -> None | Some e3 -> Some (loop e3) in
					{e with eexpr = TIf(e1,e2,eo)}
				| TSwitch(e1,cases,edef) ->
					let cases = List.map (fun (el,e) ->
						let e = loop e in
						el,e
					) cases in
					let edef = match edef with None -> None | Some edef -> Some (loop edef) in
					{e with eexpr = TSwitch(e1,cases,edef)}
				| TBinop(OpAssign,({eexpr = TLocal _} as e1),e2) ->
					push e;
					mk_assign e1
				| TBinop(OpAssignOp op,({eexpr = TLocal _} as e1),e2) ->
					push e;
					mk_assign e1
				| _ ->
					mk_assign e
			in
			loop e
		in
		let declare_temp t eo p =
			let v = gen_temp t in
			let e_v = mk (TLocal v) t p in
			let declare e_init =
				let e = mk (TVar (v,e_init)) com.basic.tvoid p in
				push e;
			in
			begin match eo with
				| None ->
					declare None
				| Some e1 ->
					let e1 = assign e_v e1 in
					begin match e1.eexpr with
						| TBinop(OpAssign,{eexpr = TLocal v1},e2) when v == v1 ->
							declare (Some e2)
						| _ ->
							declare None;
							push e1
					end
			end;
			e_v
		in
		let rec push_block () =
			let cur = !block_el in
			block_el := [];
			fun () ->
				let added = !block_el in
				block_el := cur;
				List.rev added
		and block f el =
			let close = push_block() in
			List.iter (fun e ->
				push (f e)
			) el;
			close()
		in
		block,declare_temp,fun () -> !block_el

	let run com gen_temp e =
		let block,declare_temp,close_block = mk_block_context com gen_temp in
		let has_untyped e =
			let rec loop e =
				match e.eexpr with
				| TLocal v when Meta.has Meta.Unbound v.v_meta -> raise Exit
				| _ -> Type.iter loop e
			in
			try
				loop e; false
			with Exit ->
				true
		in
		let skip_binding e =
			let rec loop e =
				match e.eexpr with
				| TLocal _ | TConst _ | TTypeExpr _ -> ()
				| TParenthesis e1 | TCast(e1,None) | TEnumParameter(e1,_,_) -> Type.iter loop e
				| _ -> raise Exit
			in
			try
				loop e;
				true
			with Exit ->
				false
		in
		let rec loop e =
			match e.eexpr with
			| TBlock el ->
				{e with eexpr = TBlock (block loop el)}
			| TCall({eexpr = TLocal v},_) when Meta.has Meta.Unbound v.v_meta ->
				e
			| TCall({eexpr = TField(_,FStatic(c,_))},_) when c.cl_extern ->
				e
			| TCall(e1,el) ->
				let e1 = loop e1 in
				{e with eexpr = TCall(e1,ordered_list el)}
			| TNew(c,tl,el) ->
				{e with eexpr = TNew(c,tl,ordered_list el)}
			| TArrayDecl el ->
				{e with eexpr = TArrayDecl (ordered_list el)}
(* 			| TObjectDecl _ ->
				e *)
			| TObjectDecl fl ->
				let el = ordered_list (List.map snd fl) in
				{e with eexpr = TObjectDecl (List.map2 (fun (n,_) e -> n,e) fl el)}
			| TBinop(OpBoolAnd | OpBoolOr as op,e1,e2) ->
				let e1 = loop e1 in
				let e_then = mk (TBlock (block loop [e2])) e2.etype e2.epos in
				let e_if,e_else = if op = OpBoolOr then
					mk (TUnop(Not,Prefix,e1)) com.basic.tbool e.epos,mk (TConst (TBool(true))) com.basic.tbool e.epos
				else
					e1,mk (TConst (TBool(false))) com.basic.tbool e.epos
				in
				loop (mk (TIf(e_if,e_then,Some e_else)) com.basic.tbool e.epos)
			| TBinop((OpAssign | OpAssignOp _) as op,{eexpr = TArray(e11,e12)},e2) ->
				let e1 = match ordered_list [e11;e12] with
					| [e1;e2] ->
						{e with eexpr = TArray(e1,e2)}
					| _ ->
						assert false
				in
				let e2 = loop e2 in
				{e with eexpr = TBinop(op,e1,e2)}
			| TBinop((OpAssign | OpAssignOp _) as op,e1,e2) ->
				let e2 = bind e2 in
				let e1 = loop e1 in
				{e with eexpr = TBinop(op,e1,e2)}
	 		| TBinop(op,e1,e2) ->
				begin match ordered_list [e1;e2] with
					| [e1;e2] ->
						{e with eexpr = TBinop(op,e1,e2)}
					| _ ->
						assert false
				end
			| TArray(e1,e2) ->
				begin match ordered_list [e1;e2] with
					| [e1;e2] ->
						{e with eexpr = TArray(e1,e2)}
					| _ ->
						assert false
				end
			| TWhile(e1,e2,flag) when (match e1.eexpr with TParenthesis {eexpr = TConst(TBool true)} -> false | _ -> true) ->
				let p = e.epos in
				let e_break = mk TBreak t_dynamic p in
				let e_not = mk (TUnop(Not,Prefix,Codegen.mk_parent e1)) e1.etype e1.epos in
				let e_if = mk (TIf(e_not,e_break,None)) com.basic.tvoid p in
				let e_block = if flag = NormalWhile then Type.concat e_if e2 else Type.concat e2 e_if in
				let e_true = mk (TConst (TBool true)) com.basic.tbool p in
				let e = mk (TWhile(Codegen.mk_parent e_true,e_block,NormalWhile)) e.etype p in
				loop e
			| TIf(e1,e2,eo) ->
				let e1 = bind e1 in
				let e2 = loop e2 in
				let eo = match eo with None -> None | Some e -> Some (loop e) in
				{e with eexpr = TIf(e1,e2,eo)}
			| TVar(v,Some e1) ->
				let e1 = bind e1 in
				{e with eexpr = TVar(v,Some e1)}
			| TUnop(op,flag,e1) ->
				let e1 = bind e1 in
				{e with eexpr = TUnop(op,flag,e1)}
			| _ ->
				Type.map_expr loop e
		and bind e =
			let e = loop e in
			if skip_binding e then
				e
			else
				declare_temp e.etype (Some e) e.epos
		and ordered_list el =
			List.map bind el
		in
		if has_untyped e then
			e
		else begin
			let e = loop e in
			match close_block() with
				| [] ->
					e
				| el ->
					mk (TBlock (List.rev (e :: el))) e.etype e.epos
		end
end

module Ssa = struct
	type ssa_context = {
		mutable ssa_var_map : (int,tvar) PMap.t;
		mutable ssa_var_values : (int,texpr) PMap.t;
	}

	let apply com e =
		let var_map = ref PMap.empty in
		let var_count = ref PMap.empty in
		let ssa = {
			ssa_var_map = PMap.empty;
			ssa_var_values = PMap.empty;
		} in
		let set_value v e =
			(* if e.epos.pfile = "src/Main.hx" then Printf.printf "set value %s = %s\n" v.v_name (s_expr e); *)
			ssa.ssa_var_values <- PMap.add v.v_id e ssa.ssa_var_values
		in
		let save () =
			let old = ssa.ssa_var_values in
			(fun () ->
				ssa.ssa_var_values <- old
			)
		in
		let assign v e =
			let v2 = try
				let count = (PMap.find v.v_id !var_count) + 1 in
				var_count := PMap.add v.v_id count !var_count;
				let name = Printf.sprintf "%s<%i>" v.v_name count in
				let v2 = alloc_var name v.v_type in
				var_map := PMap.add v.v_id v2 !var_map;
				ssa.ssa_var_map <- PMap.add v2.v_id v ssa.ssa_var_map;
				v2
			with Not_found ->
				v
			in
			set_value v2 e;
			mk (TLocal v2) v2.v_type e.epos
		in
		let declare v =
			var_count := PMap.add v.v_id (-1) !var_count;
		in
		let cur_el = ref [] in
		let post_el = ref [] in
		let first_function = ref true in
		let rec loop e = match e.eexpr with
			| TBlock el ->
				let old = !cur_el in
				cur_el := [];
				List.iter (fun e ->
					let e = loop e in
					cur_el := e :: !cur_el;
					match !post_el with
						| [] ->
							()
						| el ->
							List.iter (fun e ->
								cur_el := e :: !cur_el
							) el;
							post_el := [];
				) el;
				let new_el = !cur_el in
				cur_el := old;
				{e with eexpr = TBlock (List.rev new_el)}
			| TVar(v,eo) ->
				declare v;
				let eo = match eo with
					| None ->
						None
					| Some e ->
						let e = loop e in
						set_value v e;
						Some e
				in
				{e with eexpr = TVar(v,eo)}
			| TFunction tf ->
				if not !first_function then raise Exit;
				first_function := false;
				List.iter (fun (v,_) -> declare v) tf.tf_args;
				{e with eexpr = TFunction {tf with tf_expr = loop tf.tf_expr}}
			| TBinop(OpAssign,{eexpr = TLocal v},e2) ->
				let e2 = loop e2 in
				{e with eexpr = TBinop(OpAssign,assign v e2,e2)}
			| TBinop(OpAssignOp op,({eexpr = TLocal v} as e1),e2) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				let e_op = mk (TBinop(op,e1,e2)) e.etype e.epos in
				let ev = assign v e_op in
				let e_assign = {e with eexpr = TBinop(OpAssign,ev,e_op) } in
				e_assign
			| TUnop((Increment | Decrement) as op,flag,({eexpr = TLocal v} as e1)) ->
				let e_one = mk (TConst (TInt (Int32.of_int 1))) com.basic.tint e.epos in
				let binop = if op = Increment then OpAdd else OpSub in
				let e1 = loop e1 in
				let e_op = mk (TBinop(binop,e1,e_one)) e1.etype e1.epos in
				let ev = assign v e_op in
				let e_assign = {e with eexpr = TBinop(OpAssign,ev,e_op) } in
				if flag = Prefix then begin
					cur_el := e_assign :: !cur_el;
					ev
				end else begin
					post_el := e_assign :: !post_el;
					e1
				end
			| TIf(e1,e2,eo) ->
				let e1 = loop e1 in
				let restore1 = save() in
				let e2 = loop e2 in
				restore1();
				let eo = match eo with
					| None ->
						None
					| Some e ->
						let restore2 = save() in
						let e = loop e in
						restore2();
						Some e
				in
				{e with eexpr = TIf(e1,e2,eo)}
			| TSwitch(e1,cases,eo) ->
				let e1 = loop e1 in
				let cases = List.map (fun (el,e) ->
					let restore = save() in
					let el = List.map loop el in
					let e = loop e in
					restore();
					el,e
				) cases in
				let eo = match eo with
					| None ->
						None
					| Some e ->
						let restore = save() in
						let e = loop e in
						restore();
						Some e
				in
				{e with eexpr = TSwitch(e1,cases,eo)}
			| TWhile _ | TFor _ ->
				raise Exit
			| TLocal v ->
				begin try
					let v2 = PMap.find v.v_id !var_map in
					mk (TLocal v2) v2.v_type e.epos
				with Not_found ->
					e
				end
			| _ ->
				Type.map_expr loop e
		in
		let e = loop e in
		e,ssa

	let unapply ssa e =
		let rec loop e = match e.eexpr with
			| TLocal v ->
				begin try
					let v2 = PMap.find v.v_id ssa.ssa_var_map in
					{e with eexpr = TLocal v2}
				with Not_found ->
					e
				end
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

module ConstPropagation = struct
	open Ssa
	let run ssa e =
		let rec loop e = match e.eexpr with
			| TLocal v when follow v.v_type != t_dynamic ->
				begin try
					let e' = loop (PMap.find v.v_id ssa.ssa_var_values) in
					begin match e'.eexpr with
						| TConst (TThis | TSuper) ->
							e
						| TConst _ ->
							e'
						| _ ->
							e
					end
				with Not_found ->
					e
				end
			| TBinop((OpAssign | OpAssignOp _) as op,({eexpr = TLocal v} as e1),e2) ->
				let e2 = loop e2 in
				{e with eexpr = TBinop(op,e1,e2)}
			| TBinop(op,e1,e2) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				let e = {e with eexpr = TBinop(op,e1,e2)} in
				let e' = Optimizer.optimize_binop e op e1 e2 in
				e'
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

module LocalDce = struct
	let run e =
		let is_used v = Meta.has Meta.Used v.v_meta in
		let use v = v.v_meta <- (Meta.Used,[],Ast.null_pos) :: v.v_meta in
		let rec filter e = match e.eexpr with
			| TVar(v,eo) when not (is_used v) ->
				begin match eo with
					| None ->
						None
					| Some e when not (Optimizer.has_side_effect e) ->
						None
					| Some e ->
						Some e
				end
			| TBinop(OpAssign,{eexpr = TLocal v},e2) when not (is_used v) ->
				filter e2
			| TLocal v when not (is_used v) ->
				None
			| _ ->
				Some e
		in
		let rec loop e = match e.eexpr with
			| TBlock el ->
				let rec block el = match el with
					| e :: el ->
						let el = block el in
						let e = loop e in
						begin match filter e with
							| None ->
								el
							| Some e ->
								e :: el
						end
					| [] ->
						[]
				in
				{e with eexpr = TBlock (block el)}
			| TLocal v ->
				use v;
				e
			| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
				let e2 = loop e2 in
				{e with eexpr = TBinop(OpAssign,e1,e2)}
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

let run_ssa com e =
	let n = ref (-1) in
	let gen_local t =
		incr n;
		alloc_var ("_" ^ (string_of_int !n)) t
	in
	let e = Simplifier.run com gen_local e in
	(* let e = try *)
		(* let e,ssa = Ssa.apply com e in *)
		(* let e = ConstPropagation.run ssa e in *)
		(* let e = Ssa.unapply ssa e in *)
		(* let e = LocalDce.run e in *)
		(* e *)
	(* with Exit -> *)
		(* e *)
	(* in *)
	e