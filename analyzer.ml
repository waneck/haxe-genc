open Ast
open Type
open Common

let assigns_to_trace = ref false

let rec run e =
	match e.eexpr with
	| TBinop(OpAssign, {eexpr = TField(_,FStatic({cl_path=["haxe"],"Log"}, {cf_name = "trace"}))}, _) ->
		assigns_to_trace := true
	| _ ->
		Type.iter run e

let s_expr = s_expr (s_type (print_context()))
let debug e = if e.epos.pfile = "src/Main.hx" then print_endline (s_expr e)

let has_analyzer_option meta s =
	try
		let _,el,_ = Meta.get Meta.Analyzer meta in
		List.exists (fun (e,p) ->
			match e with
				| EConst(Ident s2) when s = s2 -> true
				| _ -> false
		) el
	with Not_found ->
		false

let rec get_type_meta t = match t with
	| TMono r ->
		begin match !r with
			| None -> raise Not_found
			| Some t -> get_type_meta t
		end
	| TLazy f ->
		get_type_meta (!f())
	| TInst(c,_) ->
		c.cl_meta
	| TEnum(en,_) ->
		en.e_meta
	| TAbstract(a,_) ->
		a.a_meta
	| TType(t,_) ->
		t.t_meta
	| TAnon _ | TFun _ | TDynamic _ ->
		raise Not_found

let type_has_analyzer_option t s =
	try
		has_analyzer_option (get_type_meta t) s
	with Not_found ->
		false

(*
	This module simplifies the AST by introducing temporary variables for complex expressions in many places.
	In particular, it ensures that no branching can occur in value-places so that we can later insert SSA PHI
	nodes without worrying about their placement.
*)
module Simplifier = struct
	let mk_block_context com gen_temp =
		let block_el = ref [] in
		let push e = block_el := e :: !block_el in
		let assign ev e =
			(* debug e; *)
			let mk_assign e2 = match e2.eexpr with
				| TBreak | TContinue | TThrow _ -> e2
				| _ -> mk (TBinop(OpAssign,ev,e2)) e2.etype e2.epos
			in
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
				| TParenthesis e1 ->
					loop e1 (* this is weird *)
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
					begin match e1.eexpr with
						| TThrow _ | TReturn _ | TBreak | TContinue ->
							()
						| _ ->
							let e1 = assign e_v e1 in
							begin match e1.eexpr with
								| TBinop(OpAssign,{eexpr = TLocal v1},e2) when v == v1 ->
									declare (Some e2)
								| _ ->
									declare None;
									push e1
							end
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
		let skip_binding ?(allow_tlocal=false) e =
			let rec loop e =
				match e.eexpr with
				| TConst _ | TTypeExpr _ | TFunction _ -> ()
				| TLocal _ when allow_tlocal -> ()
				| TParenthesis e1 | TCast(e1,None) | TEnumParameter(e1,_,_) -> Type.iter loop e
				| _ -> raise Exit
			in
			try
				loop e;
				true
			with Exit ->
				begin match follow e.etype with
					| TAbstract({a_path = [],"Void"},_) -> true
					| _ -> false
				end
		in
		let rec loop e =
			match e.eexpr with
			| TLocal v when Meta.has Meta.Unbound v.v_meta ->
				raise Exit (* nope *)
			| TBlock el ->
				{e with eexpr = TBlock (block loop el)}
			| TCall({eexpr = TField(_,(FStatic(c,cf) | FInstance(c,_,cf)))},el) when has_analyzer_option cf.cf_meta "no_simplification" || has_analyzer_option c.cl_meta "no_simplification" ->
				e
			| TCall(e1,el) ->
				let e1 = loop e1 in
				let check e t =
					try
						let meta = get_type_meta t in
						if has_analyzer_option meta "no_simplification" then
							e
						else
							bind e
					with Not_found ->
						bind e
				in
				let el = Codegen.UnificationCallback.check_call check el e1.etype in
				{e with eexpr = TCall(e1,el)}
			| TNew(c,tl,el) ->
				{e with eexpr = TNew(c,tl,ordered_list el)}
			| TArrayDecl el ->
				{e with eexpr = TArrayDecl (ordered_list el)}
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
				let e_if eo = mk (TIf(e_not,e_break,eo)) com.basic.tvoid p in
				let rec map_continue e = match e.eexpr with
					| TContinue ->
						(e_if (Some e))
					| TWhile _ | TFor _ ->
						e
					| _ ->
						Type.map_expr map_continue e
				in
				let e2 = if flag = NormalWhile then e2 else map_continue e2 in
				let e_if = e_if None in
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
				let e1 = match e1.eexpr with
					| TFunction _ -> loop e1
					| TArrayDecl [{eexpr = TFunction _}] -> loop e1
					| _ -> bind e1
				in
				{e with eexpr = TVar(v,Some e1)}
			| TUnop((Neg | NegBits | Not) as op,flag,e1) ->
				let e1 = bind e1 in
				{e with eexpr = TUnop(op,flag,e1)}
			| TField(e1,fa) ->
				let e1 = bind e1 in
				{e with eexpr = TField(e1,fa)}
			| TReturn (Some ({eexpr = TThrow _} as e1)) ->
				e1 (* this is a bit hackish *)
			| TReturn (Some e1) ->
				let e1 = bind e1 in
				{e with eexpr = TReturn (Some e1)}
			| _ ->
				Type.map_expr loop e
		and bind e =
			let e = loop e in
			if skip_binding e then
				e
			else
				declare_temp e.etype (Some e) e.epos
		and ordered_list el =
			if List.for_all (skip_binding ~allow_tlocal:true) el then
				List.map loop el
			else
				List.map bind el
		in
		let e = loop e in
		match close_block() with
			| [] ->
				e
			| el ->
				mk (TBlock (List.rev (e :: el))) e.etype e.epos
end

module Ssa = struct

	type var_map = ((int,tvar) PMap.t)

	type join_node = {
		mutable branches : var_map list;
	}

	let mk_phi =
		let v_phi = alloc_var "__ssa_phi__" t_dynamic in
		(fun vl p ->
			let mk_loc v = mk (TLocal v) v.v_type p in
			let e = mk (TCall(mk_loc v_phi,(List.map mk_loc vl))) t_dynamic p in
			e
		)

	let invert_phi phi =
		List.fold_left (fun acc (v,vl) -> match List.rev vl with
			| [] -> acc
			| v2 :: _ -> PMap.add v2.v_id v acc
		) PMap.empty phi

	let replace_locals phi_inv e =
		let rec loop e = match e.eexpr with
			| TLocal v ->
				begin try
					{e with eexpr = TLocal (PMap.find v.v_id phi_inv)}
				with Not_found ->
					e
				end
			| _ ->
				Type.map_expr loop e
		in
		loop e

	let replace_vars phi_inv vars =
		PMap.mapi (fun i v ->
			try
				let v' = PMap.find v.v_id phi_inv in
				v'
			with Not_found -> v
		) vars

	let build_phi vl p =
		List.map (fun (v,vl) ->
			let phi = mk_phi vl p in
			let e = Codegen.binop OpAssign (mk (TLocal v) v.v_type p) phi phi.etype p in
			mk (TMeta((Meta.Custom ":ssa",[],e.epos),e)) e.etype e.epos
		) vl

	let to_block el t p =
		mk (TBlock el) t p

	(* TODO: make sure this is conservative *)
	let can_throw e =
		let rec loop e = match e.eexpr with
			| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ -> ()
			| TCall _ | TNew _ | TThrow _ | TCast(_,Some _) -> raise Exit
			| _ -> Type.iter loop e
		in
		try
			loop e; false
		with Exit ->
			true

	let apply com e =
		let mk_join_node() = {
			branches = []
		} in
		let add_vars join vars =
			join.branches <- vars :: join.branches;
		in
		let var_map = ref PMap.empty in
		let cur_vars = ref PMap.empty in
		let cur_loop_top = ref None in
		let cur_loop_bottom = ref None in
		let cur_exception_join = ref None in
		let branch () =
			let old = !cur_vars in
			(fun join ->
				add_vars join !cur_vars;
				cur_vars := old
			)
		in
		let set_loop_join join_top join_bottom =
			let old = !cur_loop_top,!cur_loop_bottom in
			cur_loop_top := Some join_top;
			cur_loop_bottom := Some join_bottom;
			(fun () ->
				cur_loop_top := fst old;
				cur_loop_bottom := snd old;
			)
		in
		let set_exception_join join =
			let old = !cur_exception_join in
			cur_exception_join := Some join;
			(fun () ->
				cur_exception_join := old;
			)
		in
		let declare_var v =
			var_map := PMap.add v.v_id (0,v) !var_map;
			cur_vars := PMap.add v.v_id v !cur_vars
		in
		let assign_var v p =
			if v.v_capture then v else
			try
				let i,_ = PMap.find v.v_id !var_map in
				let i = i + 1 in
				var_map := PMap.add v.v_id (i,v) !var_map;
				let v' = alloc_var (Printf.sprintf "%s<%i>" v.v_name i) v.v_type in
				v'.v_meta <- [(Meta.Custom ":ssa"),[EConst(Int (string_of_int v.v_id)),e.epos],e.epos];
				cur_vars := PMap.add v.v_id v' !cur_vars;
				v'
			with Not_found ->
				com.warning ("Unbound variable " ^ v.v_name) p;
				v
		in
		let get_var v p =
			try
				PMap.find v.v_id !cur_vars
			with Not_found ->
				com.warning ("Unbound variable " ^ v.v_name) p;
				v
		in
(* 		let s_phi phi =
			let sl = List.fold_left (fun acc (v,vl) ->
				(Printf.sprintf "%s = PHI(%s)" v.v_name (String.concat ", " (List.map (fun v -> v.v_name) vl))) :: acc
			) [] phi in
			String.concat "\n" sl
		in
		let s_vars vars =
			let sl = PMap.foldi (fun i v acc ->
				let _,k = PMap.find i !var_map in
				Printf.sprintf "%s = %s" (k.v_name) v.v_name :: acc
			) vars [] in
			String.concat ", " sl
		in *)
		let close_join_node node =
			let vars = ref PMap.empty in
			let rec handle_branch branch =
				PMap.iter (fun i v ->
					try
						let vl = PMap.find i !vars in
						if not (List.memq v vl) then
							vars := PMap.add i (v :: vl) !vars
					with Not_found ->
						vars := PMap.add i [v] !vars
				) branch;
			in
			List.iter handle_branch node.branches;
			PMap.foldi (fun i vl acc -> match vl with
				| [v] ->
					cur_vars := PMap.add i v !cur_vars;
					acc
				| _ ->
					let _,v = PMap.find i !var_map in
					let v' = assign_var v null_pos in
					((v',vl) :: acc)
			) !vars []
		in
		let append_phi e phi = match phi with
			| [] ->
				e
			| _ ->
				let e_phi = to_block (build_phi phi e.epos) e.etype e.epos in
				concat e e_phi
		in
		let rec handle_if e econd eif eelse =
			let econd = loop econd in
			let join = mk_join_node() in
			let close = branch() in
			let eif = loop eif in
			close join;
			let eelse = match eelse with
				| None ->
					add_vars join !cur_vars;
					None
				| Some e ->
					let close = branch() in
					let eelse = loop e in
					close join;
					Some eelse
			in
			let phi = close_join_node join in
			let e = {e with eexpr = TIf(econd,eif,eelse)} in
			e,(build_phi phi e.epos)
		and handle_unop op flag e1 =
			let v = match e1.eexpr with
				| TLocal v -> v
				| _ -> error "Unop on non-local" e1.epos
			in
			let e_one = mk (TConst (TInt (Int32.of_int 1))) com.basic.tint e.epos in
			let binop = if op = Increment then OpAdd else OpSub in
			let e1 = loop e1 in
			let e_op = mk (TBinop(binop,e1,e_one)) e1.etype e1.epos in
			let v = assign_var v e1.epos in
			let ev = {e1 with eexpr = TLocal v} in
			let e_assign = {e with eexpr = TBinop(OpAssign,ev,e_op) } in
			e_assign,if flag = Prefix then
				ev
			else
				e1
		and handle_loop_body e =
			let join_top = mk_join_node() in
			let join_bottom = mk_join_node() in
			let unset = set_loop_join join_top join_bottom in
			let close = branch() in
			let ebody = loop e in
			close join_top;
			unset();
			add_vars join_top !cur_vars;
			let phi_top = close_join_node join_top in
			let phi_top_inv = invert_phi phi_top in
			let ebody = replace_locals phi_top_inv ebody in
			join_bottom.branches <- List.map (replace_vars phi_top_inv) join_bottom.branches;
			let phi_bottom = close_join_node join_bottom in
			ebody,phi_bottom,phi_top
		and loop e = match e.eexpr with
			(* var declarations *)
			| TVar(v,eo) ->
				declare_var v;
				let eo = match eo with
					| None -> None
					| Some e -> Some (loop e)
				in
				{e with eexpr = TVar(v,eo)}
			| TFunction tf ->
				List.iter (fun (v,_) -> declare_var v) tf.tf_args;
				let e' = loop tf.tf_expr in
				{e with eexpr = TFunction {tf with tf_expr = e'}}
			(* var modifications *)
			| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) when not (Meta.has Meta.Unbound v.v_meta) && v.v_name <> "this" ->
				let e2 = loop e2 in
				let v = assign_var v e1.epos in
				{e with eexpr = TBinop(OpAssign,{e1 with eexpr = TLocal v},e2)}
			| TBinop(OpAssignOp op,({eexpr = TLocal v} as e1),e2) ->
				let e2 = loop e2 in
				let e1 = loop e1 in
				let e_op = mk (TBinop(op,e1,e2)) e.etype e.epos in
				let v = assign_var v e.epos in
				let ev = {e1 with eexpr = TLocal v} in
				let e_assign = {e with eexpr = TBinop(OpAssign,ev,e_op) } in
				e_assign
			| TUnop((Increment | Decrement as op),Prefix,({eexpr = TLocal _} as e1)) ->
				let e_one = mk (TConst (TInt (Int32.of_int 1))) com.basic.tint e.epos in
				let binop = if op = Increment then OpAdd else OpSub in
				let e = {e with eexpr = TBinop(OpAssignOp binop,e1,e_one)} in
				loop e
			| TUnop((Increment | Decrement),Postfix,{eexpr = TLocal _}) ->
				com.warning ("Postfix unop outside block: " ^ (s_expr e)) e.epos;
				e
			(* var user *)
			| TLocal v ->
				let v = get_var v e.epos in
				{e with eexpr = TLocal v}
			(* control flow *)
			| TIf(econd,eif,eelse) ->
				(* com.warning "If outside block" e.epos; *)
				let e,phi = handle_if e econd eif eelse in
				mk (TBlock (e :: phi)) e.etype e.epos
			| TSwitch(e1,cases,edef) ->
				let e1 = loop e1 in
				let join = mk_join_node() in
				let cases = List.map (fun (el,e) ->
					let close = branch() in
					let el = List.map loop el in
					let e = loop e in
					close join;
					el,e
				) cases in
				let edef = match edef with
					| Some e ->
						let close = branch() in
						let e = loop e in
						close join;
						Some e
					| None ->
						begin match e1.eexpr with
							| TMeta((Meta.Exhaustive,_,_),_)
							| TParenthesis({eexpr = TMeta((Meta.Exhaustive,_,_),_)}) ->
								()
							| _ ->
								add_vars join !cur_vars;
						end;
						None
				in
				let phi = close_join_node join in
				let e = {e with eexpr = TSwitch(e1,cases,edef)} in
				append_phi e phi
			| TWhile(econd,ebody,mode) ->
				let econd = loop econd in
				let ebody,phi_bottom,phi_top = handle_loop_body ebody in
				let e_phi_top = to_block (build_phi phi_top e.epos) e.etype e.epos in
				let e = {e with eexpr = TWhile(econd,concat e_phi_top ebody,mode)} in
				append_phi e phi_bottom
			| TFor(v,e1,ebody) ->
				declare_var v;
				let e1 = loop e1 in
				let ebody,phi_bottom,phi_top = handle_loop_body ebody in
				let e_phi_top = to_block (build_phi phi_top e.epos) e.etype e.epos in
				let e = {e with eexpr = TFor(v,e1,concat e_phi_top ebody)} in
				append_phi e phi_bottom
			| TTry(e1,catches) ->
				let join_ex = mk_join_node() in
				let join_bottom = mk_join_node() in
				let unset = set_exception_join join_ex in
				let e1 = loop e1 in
				unset();
				add_vars join_bottom !cur_vars;
				let phi = close_join_node join_ex in
				let e_phi = to_block (build_phi phi e.epos) e.etype e.epos in
				let catches = List.map (fun (v,e) ->
					declare_var v;
					let close = branch() in
					let e = loop e in
					close join_bottom;
					let e = concat e_phi e in
					v,e
				) catches in
				let phi = close_join_node join_bottom in
				let e = {e with eexpr = TTry(e1,catches)} in
				append_phi e phi
			| TBreak ->
				begin match !cur_loop_bottom with
					| None -> error "Break outside loop" e.epos
					| Some join -> add_vars join !cur_vars
				end;
				e
			| TContinue ->
				begin match !cur_loop_top with
					| None -> error "Continue outside loop" e.epos
					| Some join -> add_vars join !cur_vars
				end;
				e
			(* misc *)
			| TBlock el ->
				let rec loop2 el = match el with
					| e :: el ->
						begin match e.eexpr with
							| TIf(econd,eif,eelse) | TParenthesis({eexpr = TIf(econd,eif,eelse)}) ->
								let e,phi = handle_if e econd eif eelse in
								(e :: phi) @ loop2 el
							| TUnop((Increment | Decrement as op),flag,({eexpr = TLocal _} as e1)) ->
								let e_assign,e' = handle_unop op flag e1 in
								e_assign :: (loop2 el)
							| TVar (v,Some {eexpr = TUnop((Increment | Decrement as op),Postfix,({eexpr = TLocal v2} as e1))}) ->
								declare_var v;
								let e_one = mk (TConst (TInt (Int32.of_int 1))) com.basic.tint e.epos in
								let binop = if op = Increment then OpAdd else OpSub in
								let e1 = loop e1 in
								let e_op = mk (TBinop(binop,e1,e_one)) e1.etype e1.epos in
								let e_v = {e with eexpr = TVar(v, Some e1)} in
								let v2 = assign_var v2 e1.epos in
								let e_assign = {e with eexpr = TBinop(OpAssign,{e1 with eexpr = TLocal v2},e_op)} in
								e_v :: e_assign :: (loop2 el)
							| TBinop (OpAssign,e1,{eexpr = TUnop((Increment | Decrement as op),Postfix,({eexpr = TLocal v2} as e2))}) ->
								let e_one = mk (TConst (TInt (Int32.of_int 1))) com.basic.tint e.epos in
								let binop = if op = Increment then OpAdd else OpSub in
								let e2 = loop e2 in
								let e1 = loop e1 in
								let e_op = mk (TBinop(binop,e2,e_one)) e1.etype e1.epos in
								let e_v = {e with eexpr = TBinop(OpAssign,e1,e2)} in
								let v2 = assign_var v2 e1.epos in
								let e_assign = {e with eexpr = TBinop(OpAssign,{e1 with eexpr = TLocal v2},e_op)} in
								e_v :: e_assign :: (loop2 el)
							| TVar (v,Some {eexpr = TUnop((Increment | Decrement as op),Prefix,({eexpr = TLocal v2} as e1))}) ->
								declare_var v;
								let e_one = mk (TConst (TInt (Int32.of_int 1))) com.basic.tint e.epos in
								let binop = if op = Increment then OpAdd else OpSub in
								let e1 = loop e1 in
								let e_op = mk (TBinop(binop,e1,e_one)) e1.etype e1.epos in
								let v2 = assign_var v2 e1.epos in
								let e_loc = {e1 with eexpr = TLocal v2} in
								let e_v = {e with eexpr = TVar(v, Some e_loc)} in
								let e_assign = {e with eexpr = TBinop(OpAssign,e_loc,e_op)} in
								e_assign :: e_v :: (loop2 el)
							| TParenthesis (e1) ->
								loop2 (e1 :: el)
							| _ ->
								let e = loop e in
								e :: (loop2 el)
						end
					| [] ->
						[]
				in
				let el = loop2 el in
				{e with eexpr = TBlock el}
			| _ ->
				begin match !cur_exception_join with
					| Some join when can_throw e -> add_vars join !cur_vars
					| Some join -> ()
					| _ -> ()
				end;
				Type.map_expr loop e
		in
		loop e

	let unapply com e =
		let vars = ref PMap.empty in
		let declare v =
			vars := PMap.add (string_of_int v.v_id) v !vars;
		in
		let rec loop e = match e.eexpr with
			| TVar(v,eo) ->
				let e1 = match eo with
					| None -> None
					| Some e -> Some (loop e)
				in
				declare v;
				{e with eexpr = TVar(v,e1)}
			| TFunction tf ->
				List.iter (fun (v,_) -> declare v) tf.tf_args;
				{e with eexpr = TFunction {tf with tf_expr = loop tf.tf_expr}}
			| TTry (e1,catches) ->
				let e1 = loop e1 in
				let catches = List.map (fun (v,e) ->
					declare v;
					v,loop e
				) catches in
				{e with eexpr = TTry(e1,catches)}
			| TFor(v,e1,e2) ->
				declare v;
				let e1 = loop e1 in
				let e2 = loop e2 in
				{e with eexpr = TFor(v,e1,e2)}
			| TLocal v when Meta.has (Meta.Custom ":ssa") v.v_meta ->
				begin match Meta.get (Meta.Custom ":ssa") v.v_meta with
					| _,[(EConst(Int i)),_],_ ->
						begin try
							let v = PMap.find i !vars in
							{e with eexpr = TLocal v}
						with Not_found ->
							com.warning ("Not_found: " ^ (v.v_name)) e.epos;
							e
						end
					| _ ->
						assert false
				end
			| TBlock el ->
				let rec filter e = match e.eexpr with
					| TMeta((Meta.Custom ":ssa",_,_),_) ->
						false
					| _ ->
						true
				in
				let el = List.filter filter el in
				let el = List.map loop el in
				{e with eexpr = TBlock el}
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

module ConstPropagation = struct
	let expr_eq e1 e2 = match e1.eexpr,e2.eexpr with
		| TConst ct1, TConst ct2 ->
			ct1 = ct2
		| _ ->
			false

	let apply com e =
		let var_to_expr_map = ref PMap.empty in
		let assign v e =
			var_to_expr_map := PMap.add v.v_id e !var_to_expr_map
		in
		let is_first_loop_iteration = ref false in
		let had_function = ref false in
		let rec loop e = match e.eexpr with
			| TFunction _ when !had_function ->
				e
			| TFunction tf ->
				had_function := true;
				{e with eexpr = TFunction {tf with tf_expr = loop tf.tf_expr}}
			| TVar(v,Some e1) ->
				let e1 = loop e1 in
				assign v e1;
				{e with eexpr = TVar(v,Some e1)}
			| TLocal v when (match follow v.v_type with TDynamic _ -> false | _ -> not v.v_capture && not (type_has_analyzer_option v.v_type "no_const_propagation")) ->
				begin try
					let e' = (PMap.find v.v_id !var_to_expr_map) in
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
			| TCall ({eexpr = TLocal {v_name = "__ssa_phi__"}},el) ->
				let el = List.map loop el in
				begin match List.rev el with
					| [] -> assert false
					| e1 :: el ->
						let e1 = loop e1 in
						if List.for_all (fun e2 -> match e2.eexpr with
 							| TLocal v when not !is_first_loop_iteration && Meta.has (Meta.Custom ":ssa") v.v_meta && not (PMap.mem v.v_id !var_to_expr_map) ->
 								(* This means that the SSA variable was not part of the AST, which can happen if parts of the
								   AST are eliminated (e.g. `if (false)`). In this case we can ignore the value. *)
								true
							| _ ->
								let b = expr_eq e1 (loop e2) in
								(* if e1.epos.pfile = "src/Main.hx" then Printf.printf "%s %s %b\n" (s_expr e1) (s_expr e2) b; *)
								b
						) el then
							e1
						else
							e
				end
			| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
				let e2 = loop e2 in
				assign v e2;
				{e with eexpr = TBinop(OpAssign,e1,e2)}
			| TBinop(op,e1,e2) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				let e = {e with eexpr = TBinop(op,e1,e2)} in
				let e' = Optimizer.optimize_binop e op e1 e2 in
				e'
			| TUnop(op,flag,e1) ->
				let e1 = loop e1 in
				Optimizer.optimize_unop {e with eexpr = TUnop(op,flag,e1)} op flag e1
			| TIf(e1,e2,eo) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				begin match e1.eexpr with
					| TConst (TBool true) ->
						e2
					| TConst (TBool false) ->
						begin match eo with
							| None ->
								mk (TConst TNull) t_dynamic e.epos
							| Some e ->
								loop e
						end
					| _ ->
						let eo = match eo with None -> None | Some e -> Some (loop e) in
						{e with eexpr = TIf(e1,e2,eo)}
				end;
			| TCall({eexpr = TField(_,(FStatic(_,cf) | FInstance(_,_,cf) | FAnon cf))},el) when has_analyzer_option cf.cf_meta "no_const_propagation" ->
				e
			| TCall(e1,el) ->
				let e1 = loop e1 in
				let check e t =
					try
						let meta = get_type_meta t in
						if has_analyzer_option meta "no_const_propagation" then
							e
						else
							loop e
					with Not_found ->
						loop e
				in
				let el = Codegen.UnificationCallback.check_call check el e1.etype in
				{e with eexpr = TCall(e1,el)}
			| TWhile(econd,ebody,flag) ->
				let econd = loop econd in
				let old = !is_first_loop_iteration in
				is_first_loop_iteration := true;
				let ebody = loop ebody in
				is_first_loop_iteration := old;
				let ebody = loop ebody in
				{e with eexpr = TWhile(econd,ebody,flag)}
			| TFor(v,e1,ebody) ->
				let e1 = loop e1 in
				let old = !is_first_loop_iteration in
				is_first_loop_iteration := true;
				let ebody = loop ebody in
				is_first_loop_iteration := old;
				let ebody = loop ebody in
				{e with eexpr = TFor(v,e1,ebody)}
			| TBlock el ->
				let rec loop2 el = match el with
					| e :: el ->
						let e = loop e in
						begin match e.eexpr with
							| TBreak | TContinue | TReturn _ | TThrow _ ->
								begin match el with
									| e :: _ -> com.warning "Unreachable code" e.epos
									| [] -> ()
								end;
								[e]
							| _ ->
								e :: loop2 el
						end
					| [] ->
						[]
				in
				let el = loop2 el in
				{e with eexpr = TBlock el}
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

module LocalDce = struct
	let apply com e =
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
						filter e
				end
			| TVar _ ->
				Some e
			| TBinop(OpAssign,{eexpr = TLocal v},e2) when not (is_used v) ->
				filter e2
			| TLocal v when not (is_used v) ->
				None
			| _ ->
				if Optimizer.has_side_effect e then
					Some e
				else
					None
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
			| TIf ({ eexpr = TConst (TBool t) },e1,e2) ->
				(if t then loop e1 else match e2 with None -> { e with eexpr = TBlock [] } | Some e -> loop e)
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

let run_ssa com e =
	let all_locals = ref PMap.empty in
	let rec loop e = match e.eexpr with
		| TVar(v,eo) ->
			all_locals := PMap.add v.v_name true !all_locals;
			begin match eo with None -> () | Some e -> loop e end
		| _ ->
			Type.iter loop e
	in
	loop e;
	let n = ref (-1) in
	let rec gen_local t =
		incr n;
		let name = "_" ^ (string_of_int !n) in
		if PMap.mem name !all_locals then
			gen_local t
		else
			alloc_var name t
	in
	let has_analyzer_define = Common.defined com Define.StaticAnalyzer in
	let do_simplify,do_optimize = match com.platform with
		| Cpp | Flash8 -> true,has_analyzer_define
		| _ -> has_analyzer_define,has_analyzer_define
	in
	try
		let e = if do_simplify then
			 Simplifier.run com gen_local e
		else
			e
		in
		let e = if do_optimize then
				let e = Ssa.apply com e in
				let e = ConstPropagation.apply com e in
				let e = Ssa.unapply com e in
				(* let e = LocalDce.apply com e in *)
				e
		else
			e
		in
		e
	with Exit ->
		e

let run_expression_filters com t =
	match t with
	| TClassDecl c when (has_analyzer_option c.cl_meta "no_ssa") ->
		()
	| TClassDecl c ->
		let process_field cf =
			match cf.cf_expr with
			| Some e when not (has_analyzer_option cf.cf_meta "no_ssa") && not (Meta.has Meta.Extern cf.cf_meta) (* TODO: use is_removable_field *) ->
				cf.cf_expr <- Some (run_ssa com e);
			| _ -> ()
		in
		List.iter process_field c.cl_ordered_fields;
		List.iter process_field c.cl_ordered_statics;
		(match c.cl_constructor with
		| None -> ()
		| Some f -> process_field f);
(* 		(match c.cl_init with
		| None -> ()
		| Some e ->
			c.cl_init <- Some (run_ssa com e)); *)
	| TEnumDecl _ -> ()
	| TTypeDecl _ -> ()
	| TAbstractDecl _ -> ()

let apply com =
	List.iter (run_expression_filters com) com.types
