open Ast
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
				loop e; true
			with Exit ->
				false
		in
		let rec loop e =
			match e.eexpr with
			| TBlock el ->
				{e with eexpr = TBlock (block loop el)}
			| TCall({eexpr = TLocal v},_) when Meta.has Meta.Unbound v.v_meta ->
				e
			| TCall(e1,el) ->
				let e1 = loop e1 in
				{e with eexpr = TCall(e1,ordered_list el)}
			| TNew(c,tl,el) ->
				{e with eexpr = TNew(c,tl,ordered_list el)}
			| TArrayDecl el ->
				{e with eexpr = TArrayDecl (ordered_list el)}
			| TObjectDecl _ ->
				e
(* 			| TObjectDecl fl ->
				let el = ordered_list (List.map snd fl) in
				{e with eexpr = TObjectDecl (List.map2 (fun (n,_) e -> n,e) fl el)} *)
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
				List.iter (fun (v,_) -> declare v) tf.tf_args;
				{e with eexpr = TFunction {tf with tf_expr = loop tf.tf_expr}}
			| TBinop(OpAssign,{eexpr = TLocal v},e2) ->
				let e2 = loop e2 in
				{e with eexpr = TBinop(OpAssign,assign v e2,e2)}
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
			| TLocal v ->
				let rec find v =
					let e2 = PMap.find v.v_id ssa.ssa_var_values in
					let rec loop e2 = match e2.eexpr with
						| TConst _ ->
							e2
						| TLocal v ->
							find v
						| _ ->
							e
					in
					loop e2
				in
				begin try
					find v
				with Not_found ->
					e
				end
			| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
				let e2 = loop e2 in
				{e with eexpr = TBinop(OpAssign,e1,e2)}
			| _ ->
				Type.map_expr loop e
		in
		loop e
end

let run com e =
	let n = ref (-1) in
	let gen_local t =
		incr n;
		alloc_var ("_" ^ (string_of_int !n)) t
	in
	let e = Simplifier.run com gen_local e in
	let e,ssa = Ssa.apply com e in
	(* let e = ConstPropagation.run ssa e in *)
	let e = Ssa.unapply ssa e in
	e