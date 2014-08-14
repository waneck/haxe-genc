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
				begin match follow e.etype with
					| TAbstract({a_path=(["c"],("ConstSizeArray" | "Struct"))},_) ->
						true
					| TInst({cl_path = [],"Array"},[t]) ->
						begin match follow t with
							| TAbstract({a_path=["c"],"VarArg"},_) ->
								true
							| _ ->
								false
						end
					| TAbstract({a_path = ["c"],"ConstPointer"},[t]) ->
						begin match follow t with
							| TAbstract({a_path=["c"],"Char"},_) ->
								true
							| _ ->
								false
						end
					| _ ->
						loop e;
						true
				end;
			with Exit ->
				false
		in
		let rec loop e =
			match e.eexpr with
			| TBlock el ->
				{e with eexpr = TBlock (block loop el)}
			| TCall({eexpr = TLocal v},_) when Meta.has Meta.Unbound v.v_meta ->
				e
			| TCall({eexpr = TField(_,FStatic({cl_path=["haxe"],"Log"},{cf_name="trace"}))}, _ :: {eexpr = TObjectDecl _} :: _) ->
				(* TODO: obvious hack *)
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
				let e_if e_else = mk (TIf(e_not,e_break,e_else)) com.basic.tvoid p in
				let rec map_continue e =
					match e.eexpr with
					| TContinue ->
						e_if (Some e)
					| TWhile _ | TFor _ ->
						e
					| _ ->
						Type.map_expr map_continue e
				in
				let e2 = map_continue e2 in
				let e_block = if flag = NormalWhile then Type.concat (e_if None) e2 else Type.concat e2 (e_if None) in
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
			| TUnop((Neg | NegBits | Not) as op,flag,e1) ->
				let e1 = bind e1 in
				{e with eexpr = TUnop(op,flag,e1)}
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