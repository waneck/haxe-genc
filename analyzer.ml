open Ast
open Common
open Type

module Simplifier = struct
	let mk_block_context com gen_temp =
		let block_el = ref [] in
		let push e = block_el := e :: !block_el in
		let declare_temp t eo p =
			let v = gen_temp t in
			let e = mk (TVar (v,eo)) com.basic.tvoid p in
			push e;
			mk (TLocal v) t p
		in
		let push_block () =
			let cur = !block_el in
			block_el := [];
			fun () ->
				let added = !block_el in
				block_el := cur;
				List.rev added
		in
		let rec block f el =
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
				let e1 = loop e1 in
				let e2 = bind e2 in
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

let run com e =
	let n = ref (-1) in
	let gen_local t =
		incr n;
		alloc_var ("_" ^ (string_of_int !n)) t
	in
	Simplifier.run com gen_local e