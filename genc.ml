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
		s_ -> return string

*)

type dependency_type =
	| DFull
	| DForward
	| DCStd

type function_context = {
	field : tclass_field;
	mutable loop_stack : string option list;
	mutable meta : metadata;
}

type target_architecture = {
	ta_refsize : int;
	ta_tpsize  : int;
	ta_fpsize  : int;
}

type rt_info =
	| RT_ref    of int        (* pos *)
	| RT_vref   of int        (* pos *)
	| RT_pref   of int        (* pos *)
	| RT_valarr of int
	| RT_refarr of int
	| RT_tparr  of int * int
	| RT_tp     of int * int  (* pos idx *)
	| RT_val    of int * int  (* pos skips *)

type field_info =
	| FI_TParm      of int          (*bytes == alignment *)
	| FI_Ref        of int          (*bytes == alignment *)
	| FI_Val        of int          (*bytes == alignment *)
	| FI_VRef       of int          (*bytes == alignment *)
	| FI_Array      of field_info (*tp fi*) * field_info (*struct fi*)
	| FI_RefArray   of field_info (*tp fi*) * field_info (*struct fi*)
	| FI_VRefArray  of field_info (*tp fi*) * field_info (*struct fi*)
	| FI_Struct     of int * int * field_info list (*bytes *  alignment * fields TODO : flatten structs (just add the field info list here) *)
	| FI_CSArr      of field_info * int * int * int (* info * size * bytes * alignment *)

type ptype_info = {

	pt_key         : string;
	pt_type        : t;
	pt_id          : int;

	mutable pt_no_header   : bool;

	mutable pt_has_tps     : bool;

	mutable pt_size        : int;

	mutable pt_field_names : string list;
	mutable pt_field_types : ptype_info list;
	mutable t_field_types  : t list;
	mutable pt_field_info  : field_info list;

	mutable pt_header_field_names : string list;
	mutable pt_header_field_types : ptype_info list;
	mutable t_header_field_types  : t list;
	mutable pt_header_field_info  : field_info list;

	mutable pt_field_offsets: int list;
	mutable pt_rt_info      : rt_info list;

	mutable pt_pos_by_tp    : (string,int) PMap.t; (* option;*)

	mutable pt_required_tps: ( int ) list;

	mutable pt_ifaces      : ptype_info list;
	mutable pt_s_types     : ptype_info list;

	mutable pt_s_id        : int; (* struct id *)
	mutable pt_i_id        : int; (* iface id *)
	mutable pt_s_group     : int; (* struct group *)
	mutable pt_i_group     : int; (* iface  group *)

	mutable pt_is_valref   : bool; (* is a type with exclusively value type var fields *)
	mutable pt_is_baseclass: bool; (* is a baseclass (cancels is_valref) *)
	mutable pt_has_vtable  : bool; (* has a vtable (affects object size ) *)
	mutable pt_has_ifaces  : bool; (* implements interfaces - affects object size if has_vtable == false *)

	mutable pt_n_refs      : int;  (* references *)
	mutable pt_n_vrefs     : int;  (* references to types with exclusively valtype var fields *)
	mutable pt_n_tps       : int;  (* type parameter var fields *)
	mutable pt_n_rawrefs   : int;  (* raw memory pointers, for array and string, have to be annotated, eg. @:gc_rawref *)

	mutable pt_refs        : int list; (* skips in 8 byte steps *)
	mutable pt_vrefs       : int list;
	mutable pt_tps         : int list;
	mutable pt_rawrefs     : int list;

	mutable pt_super       : ptype_info option;
	mutable pt_constrs     : ptype_info list;
}

type gc_types_ctx = {
	(*mutable ptypes : (int,ptype_info) PMap.t;*)

	mutable cur_ptid  : int;

	m_pt_types    : (string,ptype_info) Hashtbl.t;

	m_pt_classes  : (string,ptype_info) Hashtbl.t;
	m_pt_closures : (string,ptype_info) Hashtbl.t;
	m_pt_enums    : (string,ptype_info) Hashtbl.t;
	m_pt_anons    : (string,ptype_info) Hashtbl.t;
}

type hxc = {
	t_typeref : t -> t;
	t_pointer : t -> t;
	t_const_pointer : t -> t;
	t_func_pointer : t -> t;
	t_closure : t -> t;
	t_int64 : t;
	t_uint64 : t;
	t_uint32 : t;
	t_uint16 : t;
	t_uint8  : t;
	t_jmp_buf : t;
	t_vararg : t;

	c_lib : tclass;
	c_boot : tclass;
	c_string : tclass;
	c_array : tclass;
	c_fixed_array : tclass;
	c_exception : tclass;
	c_cstring : tclass;
	c_csetjmp : tclass;
	c_cstdlib : tclass;
	c_cstdio : tclass;
	c_vtable : tclass;
	c_gc     : tclass;
	c_gc_alloc : tclass;
	c_gc_pool : tclass;

	c_gc_classheader : tclass;
	c_gc_enumheader : tclass;
	c_gc_anonheader : tclass;
	c_gc_templates  : tclass;

	cf_deref : tclass_field;
	cf_addressof : tclass_field;
	cf_sizeof : tclass_field;

	arch : target_architecture;
	gc_types : gc_types_ctx;
}

type context = {
	com : Common.context;
	hxc : hxc;
	mutable num_temp_funcs : int;
	mutable num_labels : int;
	mutable num_identified_types : int;
	mutable get_anon_signature : (string,tclass_field) PMap.t -> string;
	mutable type_ids : (string,int) PMap.t;
	mutable type_parameters : (path, texpr) PMap.t;
	mutable init_modules : path list;
	mutable generated_types : type_context list;
	mutable generated_closure_macros : (int,bool) PMap.t;
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
	mutable gstat  : bool;
	mutable gclass : tclass;
	(* call this function instead of Type.map_expr () *)
	mutable map : texpr -> texpr;
	(* tvar_decl -> unit; declares a variable on the current block *)
	mutable declare_var : (tvar * texpr option) -> unit;
	mutable declare_temp : t -> texpr option -> tvar;
	(* runs a filter on the specified class field *)
	mutable run_filter : tclass_field -> bool -> unit;
	(* adds a field to the specified class *)
	mutable add_field : tclass -> tclass_field -> bool -> unit;
	mutable filters : filter list;
}

and filter = gen_context->(texpr->texpr)

let add_c_lib com file =
	com.c_libs <- file :: com.c_libs

type answer =
	| Yes
	| No
	| Maybe

let rec follow t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow t
		| _ -> t)
	| TLazy f ->
		follow (!f())
	| TType (t,tl) ->
		follow (apply_params t.t_params tl t.t_type)
	| TAbstract({a_path= (["c"],"Struct")},_) -> t
	| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
		follow (Abstract.get_underlying_type a pl)
	| _ -> t


(* Helper *)

let path_to_name (pack,name) = match pack with [] -> name | _ -> String.concat "_" pack ^ "_" ^ name

let get_type_id con t =
	let id = Type.s_type (print_context()) (follow t) in
	try
		PMap.find id con.type_ids
	with Not_found ->
		con.num_identified_types <- con.num_identified_types + 1;
		con.type_ids <- PMap.add id con.num_identified_types con.type_ids;
		con.num_identified_types

let sort_anon_fields fields =
	List.sort (fun cf1 cf2 ->
		match Meta.has Meta.Optional cf1.cf_meta, Meta.has Meta.Optional cf2.cf_meta with
		| false,false | true,true -> compare cf1.cf_name cf2.cf_name
		| true, false -> 1
		| false, true -> -1
	) fields

let pmap_to_list pm = PMap.fold (fun v acc -> v :: acc) pm []

let mk_runtime_prefix n = "_hx_" ^ n

let alloc_temp_func con =
	let id = con.num_temp_funcs in
	con.num_temp_funcs <- con.num_temp_funcs + 1;
	let name = mk_runtime_prefix ("func_" ^ (string_of_int id)) in
	name


module Tid = struct

	let print_context() = ref []

	let is_closed a = !(a.a_status) <> Opened

	let rec s_type ctx t =
		match t with
		| TMono r ->
			(match !r with
			| None -> Printf.sprintf "Unknown<%d>" (try List.assq t (!ctx) with Not_found -> let n = List.length !ctx in ctx := (t,n) :: !ctx; n)
			| Some t -> s_type ctx t)
		| TEnum (e,tl) ->
			Ast.s_type_path e.e_path ^ s_type_params ctx tl
		| TInst (c,tl) ->
			Ast.s_type_path c.cl_path ^ s_type_params ctx tl
		| TType (t,tl) ->
			Ast.s_type_path t.t_path ^ s_type_params ctx tl
		| TAbstract (a,tl) ->
			Ast.s_type_path a.a_path ^ s_type_params ctx tl
		| TFun ([],t) ->
			"Void -> " ^ s_fun ctx t false
		| TFun (l,t) ->
			String.concat " -> " (List.map (fun (s,b,t) ->
				(if b then "?" else "") ^ (if s = "" then "" else s ^ " : ") ^ s_fun ctx t true
			) l) ^ " -> " ^ s_fun ctx t false
		| TAnon a ->
		let ordfl = sort_anon_fields (pmap_to_list a.a_fields) in
		let fl = List.fold_left (fun acc f ->
			((if Meta.has Meta.Optional f.cf_meta then " ?" else " ") ^ f.cf_name ^ " : " ^ s_type ctx f.cf_type) :: acc) [] ordfl in
			"{" ^ (if not (is_closed a) then "+" else "") ^  String.concat "," fl ^ " }"
		| TDynamic t2 ->
			"Dynamic" ^ s_type_params ctx (if t == t2 then [] else [t2])
		| TLazy f ->
			s_type ctx (!f())

	and s_fun ctx t void =
		match t with
		| TFun _ ->
			"(" ^ s_type ctx t ^ ")"
		| TAbstract ({ a_path = ([],"Void") },[]) when void ->
			"(" ^ s_type ctx t ^ ")"
		| TMono r ->
			(match !r with
			| None -> s_type ctx t
			| Some t -> s_fun ctx t void)
		| TLazy f ->
			s_fun ctx (!f()) void
		| _ ->
			s_type ctx t

	and s_type_params ctx = function
		| [] -> ""
		| l -> "<" ^ String.concat ", " (List.map (s_type ctx) l) ^ ">"

	let id t = s_type (print_context()) t

	(* returns a string, normalizes type parameters
	   TInst, TAbstract TType & TEnum -> return Ast.s_type_path
	   TAnon                          -> map unique type-parameter names in order of occurence to TN with N = 0 ... N-1
	                                     so e.g. { v:A, x:func.B, y:Array<A> } becomes { v:T0, x:T1, y:Array<T0> }
	   we cant just ignore TPs here because for our purposes here a {x:T,y:V} doesn't equal {x:T,y:T}, and that's bc
	   we have to assign
	*)

	let tp_normalized_id t = (match t with
		| TInst ({cl_path = path},_)
		| TType ({t_path = path},_)
		| TEnum ({e_path = path},_)
		| TAbstract ({a_path = path},_) -> Ast.s_type_path path
		| TAnon (_) ->
			let m : (string,(string list*string)) PMap.t ref  = ref PMap.empty in
			let cur : int ref = ref (1-2) in
			let normalized_tp t =
				let key = (id t) in
				if PMap.mem key !m
					then (PMap.find key !m)
				else begin
					cur :=  !cur + 1;
					let value  = ([],"T" ^ (string_of_int !cur)) in
					m := PMap.add key value !m;
					value
				end in
			let rec loop t = (match t with
				| TInst({cl_kind = KTypeParameter _} as c,tps) ->
					let npath = normalized_tp t in
					TInst({c with cl_path = npath },tps)
				| _ -> Type.map loop t)
			in id (loop t)
		| _ ->
		Printf.printf "assertion 257 failed for %s.\n" (id t);
		(* we use the normal id for everything else for now*)
		id t
		(* assert false  *)
	)

end


module TDB = struct

	let tdb_anons     : (string, t) PMap.t ref = ref PMap.empty
	let tdb_enums     : (string, t) PMap.t ref = ref PMap.empty
	let tdb_classes   : (string, t) PMap.t ref = ref PMap.empty
	let tdb_defs      : (string, t) PMap.t ref = ref PMap.empty
	let tdb_funcs     : (string, t) PMap.t ref = ref PMap.empty
	let tdb_abstracts : (string, t) PMap.t ref = ref PMap.empty
	let tdb_tparms    : (string, t) PMap.t ref = ref PMap.empty
	let tdb_types     : (string, t) PMap.t ref = ref PMap.empty

	let rec add t =
		let tid = (Tid.id t) in
		if PMap.exists tid !tdb_types then () else begin
		tdb_types := PMap.add tid t !tdb_types;
			match t with
			| TAnon ta ->
				tdb_anons     := PMap.add tid t !tdb_anons;
				PMap.fold (fun cf _ -> add cf.cf_type; ()) ta.a_fields ()
			| TInst (tc,tp) ->
				tdb_classes   := PMap.add tid t !tdb_classes;
				add_params tp;
				add_cfl tc.cl_ordered_fields;
				add_cfl tc.cl_ordered_statics
			| TEnum (te,tp) ->
				tdb_enums     := PMap.add tid t !tdb_enums;
				add_params tp;
				PMap.fold (fun ef _ -> add ef.ef_type; ()) te.e_constrs ()
			| TFun (args,ret) ->
				tdb_funcs     := PMap.add tid t !tdb_funcs;
				List.iter (fun (_,_,t) -> add t ) args;
				add ret
			| TType (td,tp) ->
				tdb_defs      := PMap.add tid t !tdb_defs;
				add_params tp;
				add td.t_type
			| TAbstract (ta,tp) ->
				tdb_abstracts := PMap.add tid t !tdb_abstracts;
				add_params tp;
				add ta.a_this
			| TMono oref -> (match !oref with
				| Some t ->  add t
				| _ -> ())
			| TLazy f ->
				add (!f ())
			| _ ->
				Printf.printf "OTHER: %s \n" tid
		end
	and add_params tps = List.iter add tps
	and add_cfl cfl = List.iter (fun cf -> add cf.cf_type) cfl
	(*let anons = *)

	let get_type_by_name n = PMap.find n !tdb_types

	let p_classes () =
		PMap.foldi (fun k _ _ -> Printf.printf "class %s \n" k) !tdb_classes ()

	let iter f = PMap.fold (fun t _ -> f t; ()) !tdb_types ()

	let map f =
		let acc = PMap.fold (fun t acc -> (f t) :: acc) !tdb_types [] in
		List.rev acc


	let tp_arg_name = mk_runtime_prefix "tpinfo"

end

module Expr = struct

	let t_path t = match follow t with
		| TInst(c,_) -> c.cl_path
		| TEnum(e,_) -> e.e_path
		| TAbstract(a,_) -> a.a_path
		| _ -> [],"Dynamic"

	let mk_static_field c cf p =
		let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
		let ethis = mk (TTypeExpr (TClassDecl c)) ta p in
		let t = monomorphs cf.cf_params cf.cf_type in
		mk (TField (ethis,(FStatic (c,cf)))) t p

	let mk_static_call c cf el p =
		let ef = mk_static_field c cf p in
		let tr = match follow ef.etype with
			| TFun(args,tr) -> tr
			| _ -> assert false
		in
		mk (TCall(ef,el)) tr p

	let mk_static_field_2 c n p =
		mk_static_field c (PMap.find n c.cl_statics) p

	let mk_static_call_2 c n el p =
		mk_static_call c (PMap.find n c.cl_statics) el p

	let mk_local v p =
		{ eexpr = TLocal v; etype = v.v_type; epos = p }

	let mk_block com p el =
		let t = match List.rev el with
			| [] -> com.basic.tvoid
			| hd :: _ -> hd.etype
		in
		mk (TBlock el) t p

	let mk_cast e t =
		{ e with eexpr = TCast(e, None); etype = t }

	let mk_deref hxc p e =
		mk_static_call hxc.c_lib hxc.cf_deref [e] p

	let mk_ccode con s p =
		mk_static_call_2 con.hxc.c_lib "cCode" [mk (TConst (TString s)) con.com.basic.tstring p] p

	let mk_int com i p =
		mk (TConst (TInt (Int32.of_int i))) com.basic.tint p

	let mk_int64 hxc i p =
		mk (TConst (TInt (Int32.of_int i))) hxc.t_int64 p

	let mk_string com s p =
		mk (TConst (TString s)) com.basic.tstring p

	let mk_c_macro_call con name el p =
		let e_args = mk (TArrayDecl el) (con.com.basic.tarray t_dynamic) p in
		mk_static_call_2 con.hxc.c_lib "callCMacro" [mk_string con.com name p;e_args] p

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

	let mk_binop op e1 e2 t p =
		{ eexpr=TBinop(op,e1,e2); etype=t; epos=p }

	let mk_obj_decl fields p =
		let fields = List.sort compare fields in
		let t_fields = List.fold_left (fun acc (n,e) ->
			let cf = mk_class_field n e.etype true e.epos (Var {v_read = AccNormal; v_write = AccNormal}) [] in
			PMap.add n cf acc
		) PMap.empty fields in
		let t = TAnon {a_fields = t_fields; a_status = ref Closed} in
		mk (TObjectDecl fields) t p

	let insert_expr e once f =
		let el = match e.eexpr with TBlock el -> el | _ -> [e] in
		let el,found = List.fold_left (fun (acc,found) e ->
			match f e with
			| Some e1 when not once || not found -> e :: e1 :: acc,true
			| _ -> e :: acc,found
		) ([],false) el in
		mk (TBlock (List.rev el)) e.etype e.epos,found

	let add_meta m e =
		mk (TMeta((m,[],e.epos),e)) e.etype e.epos

	let append_global_code hxc s =
		hxc.c_boot.cl_meta <- List.map (fun (m,el,p) -> match m,el with
			| Meta.HeaderCode,[EConst (String s2),p] ->
				Meta.HeaderCode,[EConst (String (s2 ^ s)),p],p
			| _ ->
				(m,el,p)
		) hxc.c_boot.cl_meta

	let prepend_tp_arg_type hxc t =
		let tp_arg_t = hxc.t_int64 in
		(match t with
		| 	TFun(tl, r) ->
			TFun(((TDB.tp_arg_name,false,tp_arg_t) :: tl), r)
		| _  -> t)

end

module Wrap = struct

	(* string wrapping *)
	let wrap_string hxc e =
		Expr.mk_static_call_2 hxc.c_string "HX_STR" [e] e.epos

	(* basic type wrapping *)

	let box_field_name =
		mk_runtime_prefix "value"

	let mk_box_field t =
		Expr.mk_class_field box_field_name t true Ast.null_pos (Var {v_read = AccNormal;v_write=AccNormal}) []

	let mk_box_type t =
		TAnon {
			a_status = ref Closed;
			a_fields = PMap.add box_field_name (mk_box_field t) PMap.empty
		}

	let requires_wrapping t = match follow t with
		| TAbstract({a_path=[],("Int" | "Float" | "Bool")},_) ->
			true
		| _ ->
			false

	let box_basic_value e =
		if is_null e.etype || not (requires_wrapping e.etype) then
			e
		else begin
			let t = follow e.etype in
			let e = Expr.mk_obj_decl [box_field_name,{e with etype = t}] e.epos in
			Expr.mk_cast e t
		end

	let unbox_basic_value e =
		if not (is_null e.etype) || not (requires_wrapping e.etype) then
			e
		else begin
			let t = follow e.etype in
			let cf = mk_box_field t in
			let e = mk (TField(e,FAnon cf)) t e.epos in
			Expr.mk_cast e t
		end

	(* closure wrapping *)

	let wrap_function hxc ethis efunc =
		let c,t = match hxc.t_closure efunc.etype with TInst(c,_) as t -> c,t | _ -> assert false in
		let cf_func = PMap.find "_func" c.cl_fields in
		mk (TNew(c,[efunc.etype],[Expr.mk_cast efunc cf_func.cf_type;ethis])) t efunc.epos
		(*
		let efunc = { efunc with etype = Expr.prepend_tp_arg_type hxc efunc.etype } in
		(match c.cl_constructor with
		| Some tcf ->
			tcf.cf_type <-(match tcf.cf_type with
				|TFun([a,b,c],_) -> tcf.cf_type
				|_               -> Expr.prepend_tp_arg_type hxc tcf.cf_type
				);

		|_ -> assert false)*)

	let wrap_static_function hxc efunc =
		wrap_function hxc (mk (TConst TNull) (mk_mono()) efunc.epos) efunc

	(* dynamic wrapping *)

	let st = s_type (print_context())

	let is_dynamic t = match follow t with
		| TDynamic _ -> true
		| _ -> false

	let wrap_dynamic con e =
		(* con.com.warning (Printf.sprintf "Wrapping dynamic %s" (st e.etype)) e.epos; *)
		e

	let unwrap_dynamic con e t =
		(* con.com.warning (Printf.sprintf "Unwrapping dynamic %s" (s_expr_pretty "" st e)) e.epos; *)
		e
end


(* Filters *)

module Filters = struct

	let add_filter gen filter =
		gen.filters <- filter :: gen.filters

	let run_filters gen e =
		(* local vars / temp vars handling *)
		let declared_vars = ref [] in
		let temp_var_counter = ref (-1) in

		(* temporary var handling *)
		let old_declare = gen.declare_var in
		gen.declare_var <- (fun (tvar,eopt) ->
			declared_vars := (tvar,eopt) :: !declared_vars;
		);
		gen.declare_temp <- (fun t eopt ->
			incr temp_var_counter;
			let v = alloc_var ("_tmp" ^ (string_of_int !temp_var_counter)) t in
			gen.declare_var (v,eopt);
			v
		);

		let ret = List.fold_left (fun e f ->
			let found_block = ref false in
			let run = f gen in
			let mk_var_expr v eo p = {eexpr = TVar(v,eo); etype = gen.gcom.basic.tvoid; epos = p} in
			let rec map e = match e.eexpr with
				| TFunction tf when not !found_block ->
					(* if there were no blocks yet, declare inside the top-level TFunction *)
					let ret = run { e with eexpr = TFunction { tf with tf_expr = mk_block tf.tf_expr } } in
					(match !declared_vars with
					| [] -> ret
					| vars ->
						(* let expr = { eexpr = TVars(List.rev vars); etype = gen.gcom.basic.tvoid; epos = ret.epos } in *)
						let new_vars = List.rev vars in
						let prepend_vars e =  List.fold_left (fun expr (v,eo) -> Type.concat (mk_var_expr v eo ret.epos) expr) e new_vars in
						declared_vars := [];
						match ret.eexpr with
						| TFunction tf ->
							let tf_expr = prepend_vars tf.tf_expr in
							{ ret with eexpr = TFunction { tf with tf_expr = tf_expr } }
						| _ ->
							let expr = prepend_vars ret in
							expr)
				| TBlock(el) ->
					let old_declared = !declared_vars in
					found_block := true;
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
							(List.rev_map (fun (v,eo) -> mk_var_expr v eo e.epos) vars) @ el
					in
					let ret = { e with eexpr = TBlock(el) } in
					declared_vars := old_declared;
					ret
				| _ -> run e
			in

			let last_map = gen.map in
			gen.map <- map;
			let ret = map e in
			gen.map <- last_map;
			ret
		) e gen.filters in
		gen.declare_var <- old_declare;
		ret

	let run_filters_field gen stat cf =
		gen.gfield <- cf;
		gen.gstat  <- stat;
		match cf.cf_expr with
		| None -> ()
		| Some e ->
			cf.cf_expr <- Some (run_filters gen e)

	let mk_gen_context con =
		let rec gen = {
			gcom = con.com;
			gcon = con;
			gfield = null_field;
			gstat  = false;
			gclass = null_class;
			filters = [];
			map = (function _ -> assert false);
			declare_var = (fun _ -> assert false);
			declare_temp = (fun _ _ -> assert false);
			run_filter = (fun _ _ -> assert false);
			add_field = (fun c cf stat ->
				gen.run_filter cf stat;
				if stat then begin
					c.cl_ordered_statics <- cf :: c.cl_ordered_statics;
					c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics;
				end else begin
					c.cl_ordered_fields <- cf :: c.cl_ordered_fields;
					c.cl_fields <- PMap.add cf.cf_name cf c.cl_fields;
				end);
		} in
		gen

	let run_filters_types gen types =
		List.iter (fun md -> match md with
			| TClassDecl c ->
				gen.gclass <- c;
				let added = ref [] in
				let old_run_filter = gen.run_filter in
				gen.run_filter <- (fun cf stat ->
					added := (cf,stat) :: !added);

				let fields = c.cl_ordered_fields in
				let statics = c.cl_ordered_statics in
				Option.may (run_filters_field gen false) c.cl_constructor;
				List.iter (run_filters_field gen false) fields;
				List.iter (run_filters_field gen true) statics;
				gen.gfield <- null_field;
				c.cl_init <- (match c.cl_init with
					| None -> None
					| Some e -> Some (run_filters gen e));

				(* run all added fields *)
				let rec loop () = match !added with
					| [] -> ()
					| (hd,stat) :: tl ->
						added := tl;
						run_filters_field gen stat hd;
						loop ()
				in
				loop();
				gen.run_filter <- old_run_filter
			| _ -> ()
		) types

end


(*
	This filter will take all out-of-place TVars declarations and add to the beginning of each block.
	TPatMatch has some var names sanitized.
*)
module VarDeclarations = struct

	let filter gen = function e ->
		match e.eexpr with
		| TVar ({v_name = "this"},_) ->
			e
		| TVar (v,eo) ->
			gen.declare_var (v,None);
			(match eo with
				| None -> mk (TConst TNull) (mk_mono()) e.epos
				| Some e -> { eexpr = TBinop(Ast.OpAssign, Expr.mk_local v e.epos, gen.map e); etype = e.etype; epos = e.epos })
		| _ ->
			Type.map_expr gen.map e

end


(*
	Transforms (x = value) function arguments to if (x == null) x = value expressions.
	Must run before VarDeclarations or the universe implodes.
*)
module DefaultValues = struct

	type function_mode =
		| Given
		| Mixed
		| Default

	let get_fmode tf t =
		try
			let args = match follow t with TFun(args,_) -> args | _ -> raise Exit in
			let rec loop has_default args1 args2 = match args1,args2 with
				| ((v,co) :: args1),((n,o,t) :: args2) ->
					begin match o,co with
						| true,None
						| true,Some TNull -> Mixed
						| _,Some _ -> loop true args1 args2
						| false,None -> loop has_default args1 args2
					end
				| [],[] ->
					if has_default then Default else Given
				| _ ->
					Mixed
			in
			loop false tf.tf_args args
		with Exit ->
			Mixed

	let fstack = ref []

	let filter gen = function e ->
		match e.eexpr with
		| TFunction tf ->
			let p = e.epos in
			fstack := tf :: !fstack;
			let replace_locals subst e =
				let v_this = ref None in
				let rec replace e = match e.eexpr with
					| TLocal v ->
						begin try
							let vr = List.assq v subst in
							mk (TLocal vr) vr.v_type e.epos
						with Not_found ->
							e
						end
					| TConst TThis ->
						v_this := Some (alloc_var "this" e.etype);
						e
					| _ ->
						Type.map_expr replace e
				in
				replace e,!v_this
			in
			let handle_default_assign need_ret e =
				let subst,el = List.fold_left (fun (subst,el) (v,co) ->
					match co with
					| None ->
						subst,el
					| Some TNull ->
						subst,el
					| Some c ->
						let e_loc_v = Expr.mk_local v p in
						let e_loc_v2,subst = if Wrap.requires_wrapping v.v_type then begin
							let temp = gen.declare_temp (follow v.v_type) None in
							Expr.mk_local temp p,((v,temp) :: subst)
						end else
							e_loc_v,subst
						in
						let econd = Codegen.mk_parent (Codegen.binop OpEq (mk (TConst TNull) (mk_mono())p) e_loc_v gen.gcom.basic.tbool p) in
						let mk_assign e2 = Codegen.binop OpAssign e_loc_v2 e2 e2.etype p in
						let eassign = mk_assign (mk (TConst c) (follow v.v_type) p) in
						let eelse = if Wrap.requires_wrapping v.v_type then begin
							Some (mk_assign (Wrap.unbox_basic_value e_loc_v))
						end else
							None
						in
						let eif = mk (TIf(econd,eassign,eelse)) gen.gcom.basic.tvoid p in
						subst,(eif :: el)
				) ([],[]) tf.tf_args in
				let e_call = (fst (replace_locals subst e)) in
				let e_ret = if need_ret then
					mk (TReturn (Some e_call)) t_dynamic p
				else
					e_call
				in
				let el = e_ret :: el in
				Expr.mk_block gen.gcom p (List.rev el)
			in
			let e = match get_fmode tf e.etype with
				| Default ->
					let is_field_func = match !fstack with [_] -> true | _ -> false in
					let name = if is_field_func then (mk_runtime_prefix ("known_" ^ gen.gfield.cf_name)) else alloc_temp_func gen.gcon in
					let subst,tf_args = List.fold_left (fun (subst,args) (v,_) ->
						let vr = alloc_var v.v_name (follow v.v_type) in
						((v,vr) :: subst),((vr,None) :: args)
					) ([],[]) tf.tf_args in
					let tf_args = List.rev tf_args in
					let e_tf,v_this = replace_locals subst tf.tf_expr in
					let tf_args = match v_this with
						| None -> tf_args
						| Some v -> (v,None) :: tf_args
					in
					let tf_given = {
						tf_args = tf_args;
						tf_type = tf.tf_type;
						tf_expr = gen.map e_tf;
					} in
					let t_cf = TFun(List.map (fun (v,_) -> v.v_name,false,follow v.v_type) tf_args,tf.tf_type) in
					let cf_given = Expr.mk_class_field name t_cf true p (Method MethNormal) [] in
					cf_given.cf_expr <- Some (mk (TFunction tf_given) cf_given.cf_type p);
					gen.add_field gen.gclass cf_given true;
					if is_field_func then gen.gfield.cf_meta <- (Meta.Custom ":known",[(EConst(String name)),p],p) :: gen.gfield.cf_meta;
					let e_args = List.map (fun (v,_) -> Expr.mk_local v p) tf.tf_args in
					let e_args = match v_this with
						| None -> e_args
						| Some v -> (mk (TConst TThis) v.v_type p) :: e_args
					in
					let e_call = Expr.mk_static_call gen.gclass cf_given e_args p in
					let e_call = handle_default_assign true e_call in
					{ e with eexpr = TFunction({tf with tf_expr = e_call})}
				| Given ->
					{e with eexpr = TFunction{tf with tf_expr = gen.map tf.tf_expr}}
				| _ ->
					let e = handle_default_assign false tf.tf_expr in
					{ e with eexpr = TFunction({tf with tf_expr = gen.map e})}
			in
			fstack := List.tl !fstack;
			e
		| TCall({eexpr = TField(_,FStatic({cl_path=["haxe"],"Log"},{cf_name="trace"}))}, e1 :: {eexpr = TObjectDecl fl} :: _) when not !Analyzer.assigns_to_trace ->
			let s = match follow e1.etype with
				| TAbstract({a_path=[],("Int"|"hx_int8"|"hx_int16"|"hx_int32"|"hx_short"|"hx_long")},_) -> "d"
				| TAbstract({a_path=[],("hx_uint8"|"hx_uint16"|"hx_uint32"|"hx_ushort"|"hx_ulong")},_) -> "u"
				| TAbstract({a_path=[],("Float"|"hx_float32")},_) -> "g"
				| TAbstract({a_path=[],"hx_int64"},_) -> "lld"
				| TAbstract({a_path=[],"hx_uint64"},_) -> "llu"
				| TAbstract({a_path=[],("hx_char"|"hx_uchar")},_) -> "c"
				| TAbstract({a_path=[],"Bool"},_) -> "d"
				| TInst({cl_path=[],"String"},_) -> "s"
				| _ ->
					gen.gcom.warning "This will probably not work as expected" e.epos;
					"s"
			in
			let eformat = mk (TConst (TString ("%s:%d: %" ^ s ^ "\\n"))) gen.gcom.basic.tstring e.epos in
			let eargs = mk (TArrayDecl [List.assoc "fileName" fl;List.assoc "lineNumber" fl;gen.map e1]) (gen.gcom.basic.tarray gen.gcon.hxc.t_vararg) e.epos in
			Expr.mk_static_call_2 gen.gcon.hxc.c_cstdio "printf" [eformat;eargs] e.epos
		| _ ->
			Type.map_expr gen.map e

	let rec is_null_expr e = match e.eexpr with
		| TConst TNull -> Yes
		| TConst _ | TObjectDecl _ | TArrayDecl _ | TFunction _ -> No
		| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) -> is_null_expr e1
		| _ ->
			if not (is_nullable e.etype) then No else Maybe

	let mk_known_call con c cf stat el =
		match cf.cf_expr with
		| Some ({eexpr = TFunction tf}) ->
			let rec loop args el = match args,el with
				| (_,Some co) :: args,([] as el | ({eexpr = TConst TNull} :: el)) ->
					(Codegen.mk_const_texpr con.com cf.cf_pos co) :: loop args el
				| _ :: args,e :: el ->
					(* cancel if we cannot tell whether or not the argument is null *)
					if is_null_expr e = Maybe then raise Exit;
					e :: loop args el
				| [],[] ->
					[]
				| _ ->
					assert false
			in
			let name = match Meta.get (Meta.Custom ":known") cf.cf_meta with
				| _,[EConst(String s),_],_ -> s
				| _ -> assert false
			in
			let has_this e =
				let rec loop e = match e.eexpr with
					| TConst TThis -> raise Exit
					| _ -> Type.iter loop e
				in
				try
					loop e;
					false;
				with Exit ->
					true
			in
			let el = if stat then
				loop tf.tf_args el
			else match el with
				| e :: el ->
					if has_this tf.tf_expr then
						e :: loop tf.tf_args el
					else
						loop tf.tf_args el
				| [] ->
					assert false
			in
			Expr.mk_static_call_2 c name el cf.cf_pos
		| _ ->
			raise Exit

	let handle_call_site gen = function e ->
		match e.eexpr with
		| TCall({eexpr = TField(_,FStatic(c,cf))},el) when Meta.has (Meta.Custom ":known") cf.cf_meta ->
			begin try gen.map (mk_known_call gen.gcon c cf true el)
			with Exit -> e end
		| TCall({eexpr = TField(e1,FInstance(c,_,cf))},el) when Meta.has (Meta.Custom ":known") cf.cf_meta ->
			begin try gen.map (mk_known_call gen.gcon c cf false (e1 :: el))
			with Exit -> e end
		| TNew(c,tl,el) ->
			let _,cf = get_constructor (fun cf -> apply_params c.cl_params tl cf.cf_type) c in
			if Meta.has (Meta.Custom ":known") cf.cf_meta then
				begin try gen.map (mk_known_call gen.gcon c cf true el)
				with Exit -> e end
			else e
		| _ ->
			Type.map_expr gen.map e
end


(*
	This filter handles unification cases where AST transformation may be required.
	These occur in the following nodes:

		- TBinop(OpAssign,_,_)
		- TVars
		- TCall and TNew
		- TArrayDecl
		- TObjectDecl
		- TReturn

	It may perform the following transformations:
		- pad TObjectDecl with null for optional arguments
		- use Array as argument list to "rest" argument
		- box and unbox basic types
*)
module TypeChecker = struct

	let rec check gen e t =
		let e = match is_null e.etype,is_null t with
			| true,true
			| false,false -> e
			| true,false -> Wrap.unbox_basic_value e
			| false,true -> Wrap.box_basic_value e
		in
		let e = match Wrap.is_dynamic e.etype,Wrap.is_dynamic t with
			| true,true
			| false,false -> e
			| true,false ->
				begin match follow e.etype,follow t with
					| TMono _,_
					| _,TMono _ -> e
					| _ -> Wrap.unwrap_dynamic gen.gcon e t
				end
			| false,true ->
				begin match follow e.etype,follow t with
					| TMono _,_
					| _,TMono _ -> e
					| _ -> Wrap.wrap_dynamic gen.gcon e
				end
		in
		match e.eexpr,follow t with
		| TObjectDecl fl,(TAnon an as ta) ->
			let fields = sort_anon_fields (pmap_to_list an.a_fields) in
			let fl = List.map (fun cf ->
				try cf.cf_name,check gen (List.assoc cf.cf_name fl) cf.cf_type
				with Not_found -> cf.cf_name,mk (TConst TNull) (mk_mono()) e.epos
			) fields in
			{ e with eexpr = TObjectDecl fl; etype = ta}
		(* literal String assigned to const char* = pass through *)
		| TCall({eexpr = TField(_,FStatic({cl_path = [],"String"}, {cf_name = "HX_STR"}))},[{eexpr = TConst (TString _)} as e]),(TAbstract({a_path = ["c"],"ConstPointer"},[TAbstract({a_path=[],"hx_char"},_)]) | TAbstract({a_path=["c"],"VarArg"},_)) ->
			e
		(* String assigned to const char* or VarArg = unwrap *)
		| _,(TAbstract({a_path=["c"],"VarArg"},_)) when (match follow e.etype with TInst({cl_path = [],"String"},_) -> true | _ -> false) ->
			Expr.mk_static_call_2 gen.gcon.hxc.c_string "raw" [e] e.epos
		| TMeta(m,e1),t ->
			{ e with eexpr = TMeta(m,check gen e1 t)}
		| TParenthesis(e1),t ->
			{ e with eexpr = TParenthesis(check gen e1 t)}
		| _ ->
			e

	let check_call_params gen el tl =
		let rec loop acc el tl = match el,tl with
			| e :: el, (n,_,t) :: tl ->
				(* check for rest argument *)
				begin match e.eexpr with
					| TArrayDecl el2 when n = "rest" && tl = [] && el = [] ->
						let ta = match follow e.etype with
							| TInst({cl_path=[],"Array"},[t]) -> t
							| _ -> t_dynamic
						in
						loop acc el2 (List.map (fun _ -> "rest",false,ta) el2)
					| _ ->
						loop ((check gen (gen.map e) t) :: acc) el tl
				end
			| [], [] ->
				acc
			| [],_ ->
				(* should not happen due to padded nulls *)
				assert false
			(*| [x], [] ->
				 TODO: remove and fix closure_news constructors first argument type
				loop acc [] [] *)
			| _, [] ->
				(* not sure about this one *)
				assert false
		in
		try
		List.rev (loop [] el tl)
		with e -> Printf.printf "check_call_params %s %s FAILED" (s_type_path gen.gclass.cl_path) gen.gfield.cf_name; raise e

	let fstack = ref []
	let is_call_expr = ref false

	let filter gen = function e ->
		match e.eexpr with
		| TBinop(OpAssign,e1,e2) ->
			{e with eexpr = TBinop(OpAssign,gen.map e1,check gen (gen.map e2) e1.etype)}
		| TBinop(OpEq | OpNotEq as op,e1,e2) ->
			{e with eexpr = TBinop(op,gen.map e1,gen.map e2)}
		| TBinop(op,e1,e2) ->
			{e with eexpr = TBinop(op,gen.map (Wrap.unbox_basic_value e1),gen.map (Wrap.unbox_basic_value e2))}
		| TVar(v,eo) ->
			let eo = match eo with
				| None -> None
				| Some e -> Some (check gen (gen.map e) v.v_type)
			in
			{ e with eexpr = TVar(v,eo) }
		| TLocal v ->
			{ e with etype = v.v_type }
		| TCall(e1,el) ->
			is_call_expr := true;
			let e1 = gen.map e1 in
			is_call_expr := false;
			begin match follow e1.etype with
				| TFun(args,_) | TAbstract({a_path = ["c"],"FunctionPointer"},[TFun(args,_)]) ->
					{e with eexpr = TCall(e1,check_call_params gen el args)}
				| _ -> Type.map_expr gen.map e
			end
		| TNew(c,tl,el) ->
			let tcf,_ = get_constructor (fun cf -> apply_params c.cl_params tl cf.cf_type) c in
			begin match follow tcf with
				| TFun(args,_) | TAbstract({a_path = ["c"],"FunctionPointer"},[TFun(args,_)]) ->
					{e with eexpr = TNew(c,tl,check_call_params gen el args)}
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
			begin match !fstack with
				| tf :: _ -> { e with eexpr = TReturn (Some (check gen (gen.map e1) tf.tf_type))}
				| _ -> assert false
			end
		| TCast (e1,None) ->
			if e1.etype != e.etype then
				{e with eexpr = TCast(check gen (gen.map e1) e.etype,None)}
			else
				{e with eexpr = TCast(gen.map e1,None)}
		| TSwitch(e1,cases,def) ->
			let cases = List.map (fun (el,e) -> List.map (fun e -> check gen (gen.map e) e1.etype) el,gen.map e) cases in
			{ e with eexpr = TSwitch(e1,cases,match def with None -> None | Some e -> Some (gen.map e))}
		| TFunction tf ->
			fstack := tf :: !fstack;
			let etf = {e with eexpr = TFunction({tf with tf_expr = gen.map tf.tf_expr})} in
			fstack := List.tl !fstack;
			etf
		| TThrow e1 ->
			{ e with eexpr = TThrow (check gen e1 e1.etype) }
(* 		| TField(e1,(FInstance(_) as fa)) when not !is_call_expr ->
			let e1 = gen.map e1 in
			Expr.mk_cast {e with eexpr = TField(e1,fa)} e.etype *)
		| _ ->
			Type.map_expr gen.map e

end


(*
	- wraps String literals in String
	- translates String OpAdd to String.concat
	- translates String == String to String.equals
*)
module StringHandler = struct
	let is_string t = match follow t with
		| TInst({cl_path = [],"String"},_) -> true
		| _ -> false

	let filter gen e =
		(* if e.epos.pfile = "src/Main.hx" then print_endline (s_expr_pretty "" (s_type (print_context())) e); *)
		match e.eexpr with
		(* always wrap String literal *)
		| TCall({eexpr = TField(_,FStatic({cl_path=[],"String"},{cf_name = "raw"}))},[{eexpr = TConst(TString s)} as e]) ->
			e
		| (TConst (TString s) | TNew({cl_path=[],"String"},[],[{eexpr = TConst(TString s)}])) ->
			(* let s = Ast.s_escape s in *)
			Wrap.wrap_string gen.gcon.hxc (mk (TConst (TString s)) e.etype e.epos)
		(* TODO: why was this here? Breaks Std.string("literal") *)
(* 		| TCall({eexpr = TField(_,FStatic({cl_path=[],"Std"},{cf_name = "string"}))},[e1]) ->
			begin match follow e1.etype with
				| TAbstract({a_path = ["c"],"ConstPointer"},[TAbstract({a_path=[],"hx_char"},_)]) ->
					Wrap.wrap_string gen.gcon.hxc e1
				| _ ->
					e
			end *)
		| TBinop((OpEq | OpNotEq) as op,e1,e2) when is_string e1.etype ->
			Expr.mk_binop op
				(Expr.mk_static_call_2 gen.gcon.hxc.c_string "equals" [gen.map e1; gen.map e2] e1.epos)
				(mk (TConst (TBool true)) gen.gcom.basic.tbool e1.epos)
				e.etype
				e.epos
		| TBinop(OpAdd,e1,e2) when is_string e1.etype ->
			Expr.mk_static_call_2 gen.gcon.hxc.c_string "concat" [gen.map e1; gen.map e2] e1.epos
		| TBinop(OpAssignOp(OpAdd),e1,e2) when is_string e1.etype ->
			(* TODO: we have to cache e1 in a temp var and handle the assignment correctly *)
			Expr.mk_binop
				OpAssign
				e1
				(Expr.mk_static_call_2 gen.gcon.hxc.c_string "concat" [gen.map e1; gen.map e2] e1.epos)
				e1.etype
				e.epos
		| _ ->
			Type.map_expr gen.map e
end

(*
	- converts TPatMatch to TSwitch
	- converts TSwitch on String to an if/else chain
*)
module SwitchHandler = struct
	let filter gen e =
		match e.eexpr with
		| TSwitch(e1,cases,def) when StringHandler.is_string e1.etype ->
			let length_map = Hashtbl.create 0 in
			List.iter (fun (el,e) ->
				List.iter (fun es ->
					match es.eexpr with
					| TConst (TString s) ->
						let l = String.length s in
						let sl = try
							Hashtbl.find length_map l
						with Not_found ->
							let sl = ref [] in
							Hashtbl.replace length_map l sl;
							sl
						in
						sl := ([es],e) :: !sl;
					| _ ->
						()
				) el
			) cases;
			let mk_eq e1 e2 = mk (TBinop(OpEq,e1,e2)) gen.gcon.com.basic.tbool (punion e1.epos e2.epos) in
			let mk_or e1 e2 = mk (TBinop(OpOr,e1,e2)) gen.gcon.com.basic.tbool (punion e1.epos e2.epos) in
			let mk_if (el,e) eo =
				let eif = List.fold_left (fun eacc e -> mk_or eacc (mk_eq e1 e)) (mk_eq e1 (List.hd el)) (List.tl el) in
				mk (TIf(Codegen.mk_parent eif,e,eo)) e.etype e.epos
			in
			let cases = Hashtbl.fold (fun i el acc ->
				let eint = mk (TConst (TInt (Int32.of_int i))) gen.gcom.basic.tint e.epos in
				let fs = match List.fold_left (fun eacc ec -> Some (mk_if ec eacc)) def !el with Some e -> e | None -> assert false in
				([eint],fs) :: acc
			) length_map [] in
			let c_string = match gen.gcom.basic.tstring with TInst(c,_) -> c | _ -> assert false in
			let cf_length = PMap.find "length" c_string.cl_fields in
			let ef = mk (TField(e1,FInstance(c_string,[],cf_length))) gen.gcom.basic.tint e.epos in
			let e = mk (TSwitch(Codegen.mk_parent ef,cases,def)) t_dynamic e.epos in
			gen.map e
		| _ ->
				Type.map_expr gen.map e
end


(*
	This filter turns all non-top TFunction nodes into class fields and creates a c.Closure object
	in their place.

	It also handles calls to closures, i.e. local variables and Var class fields.
*)
module ClosureHandler = struct
	let fstack = ref []

	let ctx_name = mk_runtime_prefix "ctx"

	let mk_closure_field gen tf ethis p =
		let locals = ref PMap.empty in
		let unknown = ref PMap.empty in
		let save_locals () =
			let old = !locals in
			fun () -> locals := old
		in
		let add_local v = if not (PMap.mem v.v_name !locals) then locals := PMap.add v.v_name v !locals in
		let add_unknown v = if not (PMap.mem v.v_name !unknown) then unknown := PMap.add v.v_name v !unknown in
		List.iter (fun (v,_) -> add_local v) tf.tf_args;
		let v_this = alloc_var "this" (match ethis with Some e -> e.etype | _ -> mk_mono()) in
		let t_ctx = mk_mono() in
		let v_ctx = alloc_var ctx_name t_ctx in
		let e_ctx = mk (TLocal v_ctx) v_ctx.v_type p in
		let mk_ctx_field v =
			let ef = mk (TField(e_ctx,FDynamic v.v_name)) v.v_type p in
			Expr.mk_cast ef v.v_type
		in
		let rec loop e = match e.eexpr with
			| TVar(v,eo) ->
				add_local v;
				let eo = match eo with None -> None | Some e -> Some (loop e) in
				{ e with eexpr = TVar(v,eo) }
			| TLocal v ->
				if not (PMap.mem v.v_name !locals) then begin
					add_unknown v;
					mk_ctx_field v;
				end else
					e
			| TFunction tf ->
				let save = save_locals() in
				List.iter (fun (v,_) -> add_local v) tf.tf_args;
				let e = { e with eexpr = TFunction { tf with tf_expr = loop tf.tf_expr } } in
				save();
				e
			| TConst TThis ->
				if not (PMap.mem v_this.v_name !locals) then add_unknown v_this;
				mk_ctx_field v_this
			| _ ->
				Type.map_expr loop e
		in
		let e = loop tf.tf_expr in
		let name = alloc_temp_func gen.gcon in
		let vars,fields = PMap.fold (fun v (vars,fields) ->
			let e = match v.v_name,ethis with
				| "this",Some e -> e
				| _ -> mk (TLocal v) v.v_type p
			in
			(v :: vars),((v.v_name,e) :: fields)
		) !unknown ([],[]) in
		let eobj = Expr.mk_obj_decl fields p in
		Type.unify eobj.etype t_ctx;
		let t = TFun((ctx_name,false,eobj.etype) :: List.map (fun (v,_) -> v.v_name,false,v.v_type) tf.tf_args,tf.tf_type) in
		let cf = Expr.mk_class_field name t true p (Method MethNormal) [] in
		let tf = {
			tf_args = (v_ctx,None) :: tf.tf_args;
			tf_type = tf.tf_type;
			tf_expr = e;
		} in
		cf.cf_expr <- Some (mk (TFunction tf) e.etype e.epos);
		cf,eobj

	let add_closure_field gen c tf ethis p =
		let cf,e_init = mk_closure_field gen tf ethis p in
		gen.add_field c cf true;
		let e_field = mk (TField(e_init,FStatic(c,cf))) cf.cf_type p in
		Wrap.wrap_function gen.gcon.hxc e_init e_field

	let is_call_expr = ref false
	let is_extern = ref false

	let is_native_function_pointer t =
		match t with
			| TAbstract( { a_path = ["c"],"FunctionPointer" }, _ ) -> true
			| _ -> false

	let rec is_closure_expr e =
		not (is_native_function_pointer e.etype) && match e.eexpr with
			| TMeta(_,e1) | TParenthesis(e1) | TCast(e1,None) ->
				is_closure_expr e1
			| TField(_,(FStatic(_,cf) | FInstance(_,_,cf))) ->
				begin match cf.cf_kind with
					| Var _ -> true
					| _ -> false
				end
			| TField(_,FEnum _) ->
				false
			| TConst TSuper ->
				false
			| _ ->
				true

	let filter gen e =

		match e.eexpr with
		| TFunction tf ->
			fstack := tf :: !fstack;
			let e1 = match !fstack with
				| _ :: [] when (match gen.gfield.cf_kind with Method _ -> true | Var _ -> false) ->
					{e with eexpr = TFunction({tf with tf_expr = gen.map tf.tf_expr})}
				| _ ->
					add_closure_field gen gen.gclass tf None e.epos
			in
			fstack := List.tl !fstack;
			e1
		| _ when is_native_function_pointer e.etype ->
			e
		| TCall(e1,el) ->
			let old = !is_call_expr,!is_extern in
			is_call_expr := true;
			is_extern := (match e1.eexpr with TField(_,FStatic({cl_extern = true},_)) -> true | _ -> false);
			let e1 = gen.map e1 in
			is_call_expr := fst old;
			let el = List.map gen.map el in
			let e = if not !is_extern && is_closure_expr e1 then begin
				let args,r = match follow e1.etype with TFun(args,r) -> args,r | _ -> assert false in
				let e = Expr.mk_c_macro_call gen.gcon "HX_CLOSURE_CALL" (e1::el) e.epos in
				Expr.mk_cast e r
			end else
				{e with eexpr = TCall(e1,el)}
			in
			is_extern := snd old;
			e
		| TField(_,FStatic(c,({cf_kind = Method m} as cf))) when not !is_call_expr && not !is_extern ->
			Wrap.wrap_static_function gen.gcon.hxc (Expr.mk_static_field c cf e.epos)
		| TField(e1,FClosure(Some (c,_),{cf_expr = Some {eexpr = TFunction tf}})) ->
			add_closure_field gen c tf (Some e1) e.epos
		| _ ->
			Type.map_expr gen.map e
end

(*
	- translates a[b] to a.__get(b) if such a method exists
	- translates a[b] = c to a.__set(b, c) if such a method exists
	- finds specialization calls and applies their suffix
*)
module ArrayHandler = struct

	let get_type_size hxc tp = match tp with
	| TAbstract ( { a_path =[], "Int" } ,_ )
	| TAbstract ( { a_path =[], ("hx_int32" | "hx_uint32" | "hx_float32") } ,_ ) -> "32",(fun e -> e)
	| TAbstract ( { a_path =[], ("hx_int16" | "hx_uint16" | "hx_short" | "hx_ushort") } ,_ ) -> "16",(fun e -> e)
	| TAbstract ( { a_path =[], ("hx_int8" | "hx_uint8" | "hx_char" | "hx_uchar") } ,_ ) -> "8",(fun e -> e)
	| TAbstract ( { a_path =["c"], ("Int64" | "UInt64") } ,_ )
	| TAbstract ( {a_path = ["c"], "Pointer"}, _ ) -> "64",(fun e -> Expr.mk_cast e (hxc.t_int64))
	(* FIXME: should we include ConstSizeArray here? *)
	| _ -> "64",(fun e -> Expr.mk_cast e (hxc.t_int64))



	let rec mk_specialization_call c n suffix ethis el p =
		let name = if suffix = "" then n else n ^ "_" ^ suffix in
		begin try
			match ethis with
			| None ->
				let cf = PMap.find name c.cl_statics in
				Expr.mk_static_call c cf el p
			| Some (e,tl) ->
				let cf = PMap.find name c.cl_fields in
				let ef = mk (TField(e,FInstance(c,List.map snd c.cl_params,cf))) (apply_params c.cl_params tl cf.cf_type) p in
				mk (TCall(ef,el)) (match follow ef.etype with TFun(_,r) -> r | _ -> assert false) p
		with Not_found when suffix <> "" ->
			mk_specialization_call c n "" ethis el p
		end

	let filter gen e =
		match e.eexpr with
		| TArray(e1, e2) ->
			begin try begin match follow e1.etype with
				| TAbstract({a_path=["c"], "ConstSizeArray"},[t;_])
				| TAbstract({a_path=["c"], "Pointer"},[t]) ->
					{e with eexpr = TArray(gen.map e1, gen.map e2)}
				| TInst(c,[tp]) ->
					let suffix,cast = get_type_size gen.gcon.hxc (follow tp) in
					Expr.mk_cast (mk_specialization_call c "__get" suffix (Some(gen.map e1,[tp])) [gen.map e2] e.epos) tp
				| _ ->
					raise Not_found
			end with Not_found ->
				Expr.mk_cast (Type.map_expr gen.map e) e.etype
			end
		| TBinop( Ast.OpAssign, {eexpr = TArray(e1,e2)}, ev) ->
			begin try begin match follow e1.etype with
				| TInst(c,[tp]) ->
					let suffix,cast = get_type_size gen.gcon.hxc (follow tp) in
					mk_specialization_call c "__set" suffix (Some(e1,[tp])) [gen.map e2; cast (gen.map ev)] e.epos
				| _ ->
					raise Not_found
			end with Not_found ->
				Type.map_expr gen.map e
			end
		| TBinop( Ast.OpAssignOp op, {eexpr = TArray(e1,e2)}, ev) ->
			begin try begin match follow e1.etype with
				| TInst(c,[tp]) ->
					let suffix,cast = get_type_size gen.gcon.hxc (follow tp) in
					let e1 = gen.map e1 in
					let e2 = gen.map e2 in
					let e_get = Expr.mk_cast (mk_specialization_call c "__get" suffix (Some(e1,[tp])) [e2] e.epos) tp in
					let ev = gen.map ev in
					let e_op = Expr.mk_binop op e_get ev e.etype e.epos in
					mk_specialization_call c "__set" suffix (Some(e1,[tp])) [e2; cast (e_op)] e.epos
				| _ ->
					raise Not_found
			end with Not_found ->
				Type.map_expr gen.map e
			end
		(* CRUDE! *)
		| TUnop((Increment | Decrement as op),flag,{eexpr = TArray(e1,e2)}) ->
			begin try begin match follow e1.etype with
				| TInst(c,[tp]) ->
					let suffix,cast = get_type_size gen.gcon.hxc (follow tp) in
					let e1 = gen.map e1 in
					let e2 = gen.map e2 in
					let e_get = Expr.mk_cast (mk_specialization_call c "__get" suffix (Some(e1,[tp])) [e2] e.epos) tp in
					let v_temp = gen.declare_temp e_get.etype None in
					let e_temp = Expr.mk_local v_temp e_get.epos in
					let e_assign = Expr.mk_binop OpAssign e_temp e_get e_get.etype e_get.epos in
					let e_one = Codegen.type_constant gen.gcom (Int "1") e_get.epos in
					let e_op = Expr.mk_binop (if op = Increment then OpAdd else OpSub) e_temp e_one e.etype e.epos in
					let e_set = mk_specialization_call c "__set" suffix (Some(e1,[tp])) [e2; cast (e_op)] e.epos in
					let el = if flag = Prefix then
						[e_assign;e_set]
					else
						[e_assign;e_set;e_temp]
					in
					Expr.mk_block gen.gcom e_set.epos el
				| _ ->
					raise Not_found
			end with Not_found ->
				Type.map_expr gen.map e
			end
		| TCall( ({eexpr = (TField (ethis,FInstance(c,_,({cf_name = cfname })))) }) ,el) ->
			begin try begin match follow ethis.etype with
				| TInst({cl_path = [],"Array"},[tp]) ->
					let suffix,cast = get_type_size gen.gcon.hxc (follow tp) in
					Expr.mk_cast (mk_specialization_call c cfname suffix (Some(ethis,[tp])) (List.map gen.map el) e.epos) e.etype
				| _ ->
					raise Not_found
			end with Not_found ->
				Type.map_expr gen.map e
			end
		| _ ->
			Type.map_expr gen.map e
end


(*
	- TTry is replaced with a TSwitch and uses setjmp
	- TThrow is replaced with a call to longjmp
	- TFor is replaced with TWhile
	- TArrayDecl introduces an init function which is TCalled
*)
module ExprTransformation = struct

	let mk_array_decl gen el t p =
		let tparam = match follow t with
			| TInst(_,[t]) -> t
			| _ -> assert false
		in
		let c_array = gen.gcon.hxc.c_array in
		let v = alloc_var "arr" (TInst(c_array,[tparam])) in
		let eloc = mk (TLocal v) v.v_type p in
		let eret = mk (TReturn (Some (eloc))) t_dynamic p in
		let (vars,einit,arity) = List.fold_left (fun (vl,el,i) e ->
			let v = alloc_var ("v" ^ (string_of_int i)) tparam in
			let e = Expr.mk_binop OpAssign (mk (TArray(eloc,Expr.mk_int gen.gcom i p)) tparam p) (mk (TLocal v) v.v_type p) tparam p in
			(v :: vl,e :: el,i + 1)
		) ([],[eret],0) el in
		let vars = List.rev vars in
		let suffix,_ = ArrayHandler.get_type_size gen.gcon.hxc tparam in
		let enew = ArrayHandler.mk_specialization_call c_array "__new" suffix  None [Expr.mk_int gen.gcon.com arity p] p in
		let evar = mk (TVar (v,Some enew)) gen.gcom.basic.tvoid p in
		let e = mk (TBlock (evar :: einit)) t p in
		let tf = {
			tf_args = List.map (fun v -> v,None) vars;
			tf_type = t;
			tf_expr = e;
		} in
		let name = alloc_temp_func gen.gcon in
		let tfun = TFun (List.map (fun v -> v.v_name,false,v.v_type) vars,t) in
		let cf = Expr.mk_class_field name tfun true p (Method MethNormal) [] in
		let efun = mk (TFunction tf) tfun p in
		cf.cf_expr <- Some efun;

		gen.add_field gen.gclass cf true;
		Expr.mk_static_call gen.gclass cf el p

	let filter gen e =
		match e.eexpr with
		| TTry (e1,cl) ->
			let p = e.epos in
			let hxc = gen.gcon.hxc in
			let epush = Expr.mk_static_call_2 hxc.c_exception "push" [] p in
			let esubj = Codegen.mk_parent (Expr.mk_static_call_2 hxc.c_csetjmp "setjmp" [Expr.mk_deref gen.gcon.hxc p epush] p) in
			let epop = Expr.mk_static_call_2 hxc.c_exception "pop" [] p in
			let loc = gen.declare_temp (hxc.t_pointer hxc.t_jmp_buf) None in
			let epopassign = mk (TVar (loc,Some epop)) gen.gcon.com.basic.tvoid p in
			let ec1,found = Expr.insert_expr (gen.map e1) true (fun e ->
				match e.eexpr with
				| TReturn _ | TBreak _ | TContinue -> Some epop
				| _ -> None
			) in
			let ec1 = if found then ec1 else Type.concat ec1 epop in
			let c1 = [Expr.mk_int gen.gcom 0 e.epos],ec1 in
			let def = ref None in
			let cl = c1 :: (ExtList.List.filter_map (fun (v,e) ->
				let evar = mk (TVar (v,Some (Expr.mk_static_field_2 hxc.c_exception "thrownObject" p))) gen.gcon.com.basic.tvoid p in
				let e = Type.concat evar (Type.concat epopassign (gen.map e)) in
				if v.v_type == t_dynamic then begin
					def := Some e;
					None;
				end else
					Some ([Expr.mk_int gen.gcom (get_type_id gen.gcon v.v_type) e.epos],e)
			) cl) in
			mk (TSwitch(esubj,cl,!def)) e.etype e.epos
		| TThrow e1 ->
			let p = e.epos in
			let eassign = Codegen.binop OpAssign (Expr.mk_static_field_2 gen.gcon.hxc.c_exception "thrownObject" p) e1 e1.etype e1.epos in
			let epeek = Expr.mk_static_call_2 gen.gcon.hxc.c_exception "peek" [] p in
			let el = [Expr.mk_deref gen.gcon.hxc p epeek;Expr.mk_int gen.gcom (get_type_id gen.gcon e1.etype) p] in
			let ejmp = Expr.mk_static_call_2 gen.gcon.hxc.c_csetjmp "longjmp" el p in
			Type.concat eassign ejmp
		| TArrayDecl [] ->
			let c,t = match follow (gen.gcon.com.basic.tarray (mk_mono())) with
				| TInst(c,[t]) -> c,t
				| _ -> assert false
			in
			mk (TNew(c,[t],[])) gen.gcon.com.basic.tvoid e.epos
		| TArrayDecl el ->
			mk_array_decl gen (List.map gen.map el) e.etype e.epos
		| _ ->
			Type.map_expr gen.map e

end

(*
	- translates TFor to TWhile
*)
module ExprTransformation2 = struct

	let filter gen e =
		match e.eexpr with
		| TFor(v,e1,e2) ->
			let e1 = gen.map e1 in
			let vtemp = gen.declare_temp e1.etype None in
			gen.declare_var (v,None);
			let ev = Expr.mk_local vtemp e1.epos in
			let ehasnext = mk (TField(ev,quick_field e1.etype "hasNext")) (tfun [] gen.gcon.com.basic.tbool) e1.epos in
			let ehasnext = mk (TCall(ehasnext,[])) ehasnext.etype ehasnext.epos in
			let enext = mk (TField(ev,quick_field e1.etype "next")) (tfun [] v.v_type) e1.epos in
			let enext = mk (TCall(enext,[])) v.v_type e1.epos in
			let eassign = Expr.mk_binop OpAssign (Expr.mk_local v e.epos) enext v.v_type e.epos in
			let ebody = Type.concat eassign (gen.map e2) in
			mk (TBlock [
				mk (TVar (vtemp,Some e1)) gen.gcom.basic.tvoid e1.epos;
				mk (TWhile((mk (TParenthesis ehasnext) ehasnext.etype ehasnext.epos),ebody,NormalWhile)) gen.gcom.basic.tvoid e1.epos;
			]) gen.gcom.basic.tvoid e.epos
		| _ ->
			Type.map_expr gen.map e
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
			loop_stack = [];
			meta = [];
		};
		dependencies = PMap.empty;
	}

let path_to_file_path (pack,name) = match pack with [] -> name | _ -> String.concat "/" pack ^ "/" ^ name

let close_type_context ctx =
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
	in
	ctx.con.generated_types <- ctx :: ctx.con.generated_types;
	let buf = Buffer.create (Buffer.length ctx.buf_h) in
	let spr = Buffer.add_string buf in
	let n = "_h" ^ path_to_name ctx.type_path in
	let relpath path = path_to_file_path ((get_relative_path ctx.type_path path),snd path) in
	spr (Printf.sprintf "#ifndef %s\n" n);
	spr (Printf.sprintf "#define %s\n" n);
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

let add_dependency ctx dept path =
	if path <> ctx.type_path then try
		let dt = PMap.find path ctx.dependencies in ( match (dt,dept) with
		| DForward,DFull  -> ctx.dependencies <- PMap.add path DFull ctx.dependencies
		| _,_ -> () )
		with Not_found ->
			ctx.dependencies <- PMap.add path dept ctx.dependencies

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
	match c.cl_kind with
	| KTypeParameter _ -> ()
	| _ ->
		if not (check_include_meta ctx c.cl_meta) && not c.cl_extern then
			add_dependency ctx (if Meta.has Meta.Struct c.cl_meta then DFull else DForward) c.cl_path

let add_enum_dependency ctx en =
	if not (check_include_meta ctx en.e_meta) && not en.e_extern then
		add_dependency ctx (if Meta.has Meta.Struct en.e_meta || Meta.has Meta.FlatEnum en.e_meta then DFull else DForward) en.e_path

let add_abstract_dependency ctx a =
	if not (check_include_meta ctx a.a_meta) then
		add_dependency ctx (if Meta.has Meta.Struct a.a_meta then DFull else DForward) a.a_path

let add_type_dependency ctx t = match follow t with
	| TInst(c,_) ->
		add_class_dependency ctx c
	| TEnum(en,_) ->
		add_enum_dependency ctx en
	| TAnon an ->
		add_dependency ctx DFull (["c"],ctx.con.get_anon_signature an.a_fields);
	| TAbstract({a_path = ["c"],"Struct"},_) -> ()
	| TAbstract(a,_) ->
		add_abstract_dependency ctx a
	| TDynamic _ ->
		add_dependency ctx DForward ([],"Dynamic")
	| _ ->
		(* TODO: that doesn't seem quite right *)
		add_dependency ctx DForward ([],"Dynamic")


module VTableHandler = struct

	(*
	let fold_map f c xs =
		let c, ys = List.fold_left ( fun (acc,ys) x ->
			let acc, y  = f acc x in acc, (y :: ys)
		) (c,[]) xs in
		c, List.rev ys
	*)

	type vt_t = (string, tclass_field * int * tclass) PMap.t

	type maps = {
		mutable next    : int;
		mutable cids    : ( string, int ) PMap.t;
		mutable count   : ( int, int ) PMap.t;
		mutable types   : ( int, tclass ) PMap.t;
		mutable vtables : ( int, vt_t ) PMap.t;
	}

	let insert_or_inc con m id  =
		if PMap.exists id m then PMap.add id ((PMap.find id m) + 1) m else (PMap.add id 0 m)

	let get_class_id m c =
		let s  = String.concat ""  ((snd c.cl_path) :: (fst c.cl_path)) in
		let id = m.next in
		if PMap.exists s m.cids
			then (PMap.find s m.cids, m)
			else (	m.cids <- PMap.add s id m.cids; m.next <- id +1; (id,m) )

	(*
	let filterin f i xs =
		let rec loop i xs acc = match xs with
		| x :: xs -> if f(x) then loop (i+1) xs ((i,x) :: acc) else loop i xs acc
		| [] -> (i,acc)
		in loop i xs [] *)

	let get_methods c = List.filter ( fun cf -> match cf.cf_kind with
			| Method (MethNormal) -> true
			| _ -> false ) c.cl_ordered_fields

	let reverse_collect c =
		let next  = ref 0 in
		let idmap = ref PMap.empty in
		let get_id n =
			if PMap.exists n !idmap then
				PMap.find n !idmap
			else
				let id = !next in
				next := !next + 1;
				idmap := PMap.add n id !idmap;
				id
		in
		let rev_chain c =
			let rec loop c acc = match c.cl_super with
			| Some (c,_) ->  loop c ( c :: acc)
			| _ -> acc
			in (loop c [c])
		in
		let add_meta meta meta_item el p =
				if (Meta.has (Meta.Custom meta_item) meta) then meta
				else (Meta.Custom meta_item, el, p) :: meta
		in
		let rec collect sc super acc xs = match xs with
		| []        ->  (sc,super) :: acc
		| c :: tail ->
			let methods = (get_methods c) in
			c.cl_meta <- add_meta c.cl_meta ":hasvtable" [] null_pos;
			let mm = List.fold_left ( fun  m cf ->
				let vidx = (get_id cf.cf_name) in
					( cf.cf_meta <- add_meta cf.cf_meta ":overridden" [EConst(Int (string_of_int vidx)),cf.cf_pos] cf.cf_pos;
					PMap.add cf.cf_name ( cf, vidx ,c) m )
				) PMap.empty methods
			in
			let mm = PMap.foldi ( fun k (scf,vidx,sc) mm ->
				if PMap.mem k mm then mm
				else PMap.add k (scf,vidx,sc) mm
			) super mm
			in
			collect c mm ( (sc,super) :: acc) tail
		in
		let ichain = collect null_class PMap.empty [] (rev_chain c)
		in  ichain (*print_endline (string_of_int (List.length ichain))*)

	let p_ichain xs = List.iter (fun (c,m) ->
		(   print_endline ( "---" ^ (snd c.cl_path));
			(PMap.iter
				(fun _ (cf,midx,c) -> (Printf.printf "class: %s func: %s idx:%d\n" (snd c.cl_path) cf.cf_name midx) )
			m)
		)
	) xs

	let get_class_name cf = match cf.cf_type with
	| TInst (c,_) -> snd c.cl_path
	| _ -> assert false

	let p_methods c = (
		List.iter ( fun cf -> match cf.cf_kind with
			| Method (MethNormal) ->
				print_endline ( " methnormal: " ^ cf.cf_name )
			| _ -> ()
		) c.cl_ordered_fields;
		List.iter ( fun cf -> match cf.cf_kind with
			| Method (MethNormal) ->
				print_endline ( " override: " ^ cf.cf_name  )
			| _ -> ()
		) c.cl_overrides )

	let get_chains con tps =

		let m = List.fold_left ( fun m tp -> match tp with
			| TClassDecl c -> ( match c.cl_super with
				| Some (c1,_) ->
					let (id,m) =  (get_class_id m c)  in
					let (id1,m) =  (get_class_id m c1) in
						m.types <- PMap.add id c m.types;
						m.types <- PMap.add id1 c1 m.types;
						m.count <- (insert_or_inc con m.count id);
						m.count <- (insert_or_inc con m.count id1);
						m
				| None -> m )
			| _ -> m ) { count   = PMap.empty;
						types   = PMap.empty;
						cids    = PMap.empty;
						vtables = PMap.empty;
						next    = 0} tps in

		(* let _ = Analyzer.run_analyzer tps in *)

		let add_vtable con c vtable =
			(* helpers *)
			let clib, cstdlib = con.hxc.c_lib, con.hxc.c_cstdlib in
			let fname   = (mk_runtime_prefix "_vtable") in
			let c_vt    = con.hxc.c_vtable in
			(* let t_vt    = (TInst(c_vt,[])) in *)
			let t_int   = con.com.basic.tint in
			let t_voidp = con.hxc.t_pointer con.com.basic.tvoid in
			let t_vtfp  = con.hxc.t_func_pointer (Type.tfun [con.com.basic.tvoid] con.com.basic.tvoid) in
			let cf_vt = Type.mk_field fname (TInst(con.hxc.c_vtable,[])) null_pos in
			let mk_ccode s  =
				Expr.mk_static_call_2 con.hxc.c_lib "cCode" [mk (TConst (TString s)) con.com.basic.tstring null_pos] null_pos in
			let mk_field c ethis n p = try
				let cf = (PMap.find n c.cl_fields) in
				mk (TField (ethis,(FInstance (c,List.map snd c.cl_params,cf)))) cf.cf_type p
			with Not_found -> assert false
			in
			c.cl_statics <- PMap.add fname cf_vt c.cl_statics;
			c.cl_ordered_statics <- cf_vt :: c.cl_ordered_statics;

			(* 1. add global field for the vtable pointer *)
			let e_vt = Expr.mk_static_field c cf_vt null_pos in

			(* 2. add vtable initialization to cl_init *)

			let e_slot = mk_field c_vt e_vt "slots" null_pos in
			(* 2.1. fill vtable with function pointers*)
			let (mx,l_easgn) = PMap.fold ( fun (cf,vidx,c2) (mx,acc) ->
				let e_fp = Expr.mk_cast (Expr.mk_static_field c2 cf null_pos) t_vtfp in
				let esetidx = Expr.mk_binop OpAssign
					(mk (TArray(e_slot,(Expr.mk_int con.com vidx null_pos))) t_vtfp null_pos) e_fp t_vtfp null_pos in
				(max mx vidx, esetidx :: acc)
			) vtable (0,[]) in

			let sizeof t = Expr.mk_static_call clib con.hxc.cf_sizeof [(mk (TConst TNull) t null_pos)] null_pos in
			let vt_size = mx+1 in
			let e_vtsize = (Expr.mk_int con.com vt_size null_pos) in
			(* sizeof(vtable_t) + vt_size * sizeof(void ( * )())  *)
			(* 2.2 allocate vtable struct (after 2.1 because we have the vtable size now) *)
			let e_lhs = (mk_ccode "sizeof(c_VTable)") in
			let e_lhs = Expr.mk_cast e_lhs t_int in
			let e_allocsize  =
				Expr.mk_binop OpAdd e_lhs (
					Expr.mk_binop OpMult e_vtsize (sizeof t_vtfp) t_int null_pos
				) t_int null_pos in
			let e_alloc = Expr.mk_static_call_2 cstdlib "malloc" [e_allocsize] null_pos in
			let e_assign_ptr = (Expr.mk_binop OpAssign e_vt e_alloc t_voidp null_pos) in
			let e_block =  Expr.mk_block con.com null_pos (e_assign_ptr :: l_easgn) in
			c.cl_init <- ( match c.cl_init with
			| Some code -> Some (Type.concat e_block code)
			| None      -> Some e_block )

		in

		let eochains =
			PMap.foldi (fun  k v acc -> if v = 0 then (PMap.find k m.types) :: acc else acc) m.count [] in
			let gcid c =
				let (id,m) = get_class_id m c in id
			in
			let ifadd c v = if PMap.exists (gcid c) m.vtables then
								false
							else
								let pm = PMap.add (gcid c) v m.vtables in
								let _ = m.vtables <- pm in
								true
			in
			List.iter ( fun c -> (
				(*print_endline (  " end of chain: " ^ (snd c.cl_path)   );*)
				(*p_methods c;*)
				let ichain = (reverse_collect c) in
				(*p_ichain ichain;*)
				List.iter ( fun (c,m) -> if (ifadd c m) then  add_vtable con c m else () ) ichain
				)
			) eochains
end



module GC = struct

	let rec follow t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow t
		| _ -> t)
	| TLazy f ->
		follow (!f())
	| TType (t,tl) ->
		follow (apply_params t.t_params tl t.t_type)
	| TAbstract({a_path= (["c"],"Struct")},[tp]) -> t (*TAbstract(a,[follow tp])*)
	| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
		follow (Abstract.get_underlying_type a pl)
	| _ -> t

	type search_result =
		| SR_self
		| SR_none
		| SR_field of search_result * string
		| SR_f_ret of search_result
		| SR_f_arg of search_result * int
		| SR_tp    of search_result * int * t

	let rec s_sr sr = match sr with
		| SR_self          -> "sr_self"
		| SR_none          -> "sr_none"
		| SR_field(sr,n)   -> Printf.sprintf "sr_field %s %s " n (s_sr sr)
		| SR_f_ret sr      -> Printf.sprintf "sr_f_ret %s" (s_sr sr)
		| SR_f_arg (sr,idx)-> Printf.sprintf "sr_f_arg %d %s " idx (s_sr sr)
		| SR_tp(sr,idx,t)  -> Printf.sprintf "sr_tp %d %s " idx (s_sr sr)

	type caller_tpi =
		| CTPI_known  of int (* value *)
		| CTPI_func   of int (* pos in func  tpinfo *)
		| CTPI_class  of int (* pos in class tpinfo *)
		(*| CTPI_parent*)
		| CTPI_not_found

	let rec s_ctpi v = match v with
		| CTPI_known v -> Printf.sprintf "CTPI_known %d " v
		| CTPI_func v -> Printf.sprintf "CTPI_func %d " v
		| CTPI_class v -> Printf.sprintf "CTPI_class %d " v
		| CTPI_not_found -> "CTPI_not_found "

	type tp_info =
		| TP_Ref
		| TP_Val8
		| TP_Val16
		| TP_Val32
		| TP_Val64
		| TP_Unknown
		| TP_Tp

	let s_tp_info v = match v with
		| TP_Ref ->  "ref "
		| TP_Val8 -> "v8 "
		| TP_Val16 -> "v16 "
		| TP_Val32 -> "v32 "
		| TP_Val64 -> "v64 "
		| TP_Unknown -> "unknown "
		| TP_Tp -> "tp "

	let field_info_to_tpi fi = match fi with
		| FI_TParm _ -> TP_Tp
		| FI_Ref   _ -> TP_Ref
		| FI_Val   1 -> TP_Val8
		| FI_Val   2 -> TP_Val16
		| FI_Val   4 -> TP_Val32
		| FI_Val   8 -> TP_Val64
		| FI_VRefArray _
		| FI_RefArray _
		| FI_Array _ -> Printf.printf "fatal: can't use GCArrayC<T> as type parameter";
			assert false
		| FI_Struct(_,_,_)    -> Printf.printf "fatal: can't use Struct<T> as type parameter";
			assert false
		| FI_CSArr(_,_,_,_) -> Printf.printf "fatal: can't use ConstSizeArray<T> as type parameter";
			assert false
		| _ ->
			assert false

	let tp_info_to_val tpi = match tpi with
		| TP_Ref     -> 0
		| TP_Val8    -> 1
		| TP_Val16   -> 2
		| TP_Val32   -> 3
		| TP_Val64   -> 4
		| TP_Unknown -> 5
		| TP_Tp      ->
			Printf.printf "Bzzzt. Value for TP_Tp is a runtime thing.";
			assert false
		(* | _ -> assert false *)


	let s_rti = function
		| RT_ref    o ->  Printf.sprintf "rt_ref %d " o
		| RT_vref   o ->  Printf.sprintf "rt_vref %d " o
		| RT_pref   o ->  Printf.sprintf "rt_pref %d " o
		| RT_valarr o ->  Printf.sprintf "rt_valarr %d " o
		| RT_refarr o ->  Printf.sprintf "rt_refarr %d " o
		| RT_tparr (o,idx) ->  Printf.sprintf "rt_tparr %d %d " o idx
		| RT_tp    (o,idx) -> Printf.sprintf "rt_tp %d  idx: %d " o idx
		| RT_val   (o,s) -> Printf.sprintf "rt_val %d  " o

	let s_rti_l l = String.concat ", " (List.map s_rti l)

    (* physical type info
		TODO: split ptype_info up when initialization order is clear
    *)

    type 'a ptype =
		| PClass of 'a
		| PEnum  of 'a list
		| PAnon  of 'a


    let ptinfo key t ptid = {

    	pt_key          = key;
    	pt_type         = t;
    	pt_id           = ptid;

		pt_no_header    = false;

    	pt_has_tps      = false;
    	pt_is_valref    = false;
    	pt_is_baseclass = false;
    	pt_has_vtable   = false;
    	pt_has_ifaces   = false;

    	pt_size         = 0;

    	pt_field_names  = [];
    	pt_field_types  = [];
		pt_field_info   = [];
		t_field_types   = [];

		pt_pos_by_tp = PMap.empty;

		pt_header_field_names = [];
		pt_header_field_info  = [];
		pt_header_field_types = [];
		t_header_field_types  = [];

		pt_field_offsets = [];
    	pt_rt_info       = [];

		pt_required_tps = [];

        pt_s_id         = 0;
        pt_s_group      = 0;
        pt_i_id         = 0;
        pt_i_group      = 0;

    	pt_ifaces       = [];
    	pt_s_types      = [];

    	pt_n_refs       = 0;
    	pt_n_vrefs      = 0;
    	pt_n_tps        = 0;
    	pt_n_rawrefs    = 0;

    	pt_refs         = [];
    	pt_vrefs        = [];
    	pt_tps          = [];
    	pt_rawrefs      = [];

    	pt_super        = None;
    	pt_constrs      = [];
    }

	let tp_arg_name = TDB.tp_arg_name

	(* the various contexts required for the GC module, split into
	   - gc_types_ctx    : stores/exposes required info about types
	   - gc_gen_ctx      : provides convenience functions for building expressions
	   - gc_field_ctx    : provides field specific info during rewriting
	*)

	let init_gc_types_ctx () = {
		cur_ptid      = 0;
		m_pt_types    = Hashtbl.create 1000;
		m_pt_classes  = Hashtbl.create  500;
		m_pt_closures = Hashtbl.create  500;
		m_pt_enums    = Hashtbl.create  500;
		m_pt_anons    = Hashtbl.create  500;
	}

	type _gc_field_ctx = {
		gc_con      : context;
		gc_c        : tclass;
		gc_cf       : tclass_field;
		gc_isstatic : bool;
		gc_iscon    : bool;
	}

	(*type tp_map_t = (Type.path,) PMap.t*)

	type gc_frame_ctx =
		| GC_F_init
		| GC_F_static_var   of tclass_field
		| GC_F_static_func  of tclass_field
		| GC_F_constructor  of tclass_field
		| GC_F_method       of tclass_field
		| GC_F_instance_var of tclass_field
		| GC_F_closure      of tfunc * tvar * gc_frame_ctx


	let fold_fields acc c f =
		let set_expr acc cf fctx te =
			let acc,e = f acc fctx te in
			cf.cf_expr <- Some e;
			acc in
		let acc = (match c.cl_init with
			| Some te -> let acc,e = f acc GC_F_init te in
						 c.cl_init <- Some e;
						 acc
			| None -> acc ) in
		let acc = List.fold_left (fun acc cf ->
			(match cf.cf_kind,cf.cf_expr with
				| Var _,(Some te) -> set_expr acc cf (GC_F_static_var cf) te
				| _,(Some te) ->     set_expr acc cf (GC_F_static_func cf) te
				| _,_ -> acc
			)) acc c.cl_ordered_statics in
		let acc = (match c.cl_constructor with
			| Some ( {cf_expr = Some(te)} as cf) ->
				set_expr acc cf (GC_F_constructor cf) te
			| _ -> acc ) in
		let acc = List.fold_left (fun acc cf ->
			(match cf.cf_kind,cf.cf_expr with
				| Var _,(Some te) -> set_expr acc cf (GC_F_instance_var cf) te
				| _,(Some te) ->     set_expr acc cf (GC_F_method cf) te
				| _,_ -> acc
			)) acc c.cl_ordered_fields in
		acc

	let gc_map_field_expressions ctx tctx ectx c f =
		let _ = begin
		(match c.cl_init with
			| Some te 			-> c.cl_init <- Some (f ctx tctx ectx c GC_F_init te)
			| None    			-> ()
		);
		List.iter (fun cf ->
			(match cf.cf_kind,cf.cf_expr with
				| Var _,(Some te) -> cf.cf_expr <- Some (f ctx tctx ectx c (GC_F_static_var cf) te)
				| _,(Some te)     -> cf.cf_expr <- Some (f ctx tctx ectx c (GC_F_static_func cf) te)
				| _,_ -> ()
			)) c.cl_ordered_statics;
		(match c.cl_constructor with
			| Some ( {cf_expr = Some(te)} as cf)
								-> cf.cf_expr <- Some (f ctx tctx ectx c (GC_F_constructor cf) te)
			| _   -> ()
		);
		List.iter (fun cf ->
			(match cf.cf_kind,cf.cf_expr with
				| Var _,(Some te) -> cf.cf_expr <- Some (f ctx tctx ectx c (GC_F_instance_var cf) te)
				| _,(Some te)   -> cf.cf_expr <- Some (f ctx tctx ectx c (GC_F_method cf) te)
				| _,_ -> ()
			)) c.cl_ordered_fields;
		end
		in ()

	let pt_ctx_next_id ctx =
		let nid = ctx.cur_ptid + 1 in
		let _ = ctx.cur_ptid <- nid in
		nid

	let pt_ctx_add_econ ctx t ef =
		let t = follow t in
		let key  = (Tid.tp_normalized_id t) ^ "." ^ ef.ef_name in
		if not (Hashtbl.mem ctx.m_pt_enums key) then
			begin
				let ptid = pt_ctx_next_id ctx in
				let pt = ptinfo key t ptid in
				Hashtbl.add ctx.m_pt_enums key pt;
				pt
			end
		else
			Hashtbl.find ctx.m_pt_enums key

	let pt_ctx_get_econ ctx t ef =
		let t = follow t in
		let key  = (Tid.tp_normalized_id t) ^ "." ^ ef.ef_name in
		Hashtbl.find ctx.m_pt_enums key

	let pt_ctx_add_type ctx t =
		let t = follow t in
		let key  = (Tid.tp_normalized_id t) in
		if not (Hashtbl.mem ctx.m_pt_types key) then
			begin
				let ptid = pt_ctx_next_id ctx in
				let pt = ptinfo key t ptid in
				Hashtbl.add ctx.m_pt_types key pt;
				pt
			end
		else
			Hashtbl.find ctx.m_pt_types key

	let pt_ctx_add_anon ctx t =
		let t = follow t in
		let key  = (Tid.tp_normalized_id t) in
		if not (Hashtbl.mem ctx.m_pt_anons key) then
			begin
				let ptid = pt_ctx_next_id ctx in
				let pt = ptinfo key t ptid in
				Hashtbl.add ctx.m_pt_anons key pt;
				pt
			end
		else
			Hashtbl.find ctx.m_pt_anons key

	let pt_ctx_get_anon ctx t =
		let t = follow t in
		let key  = (Tid.tp_normalized_id t) in
		Hashtbl.find ctx.m_pt_anons key

	let pt_ctx_get_type ctx t =
		let t = follow t in
		let key  = (Tid.tp_normalized_id t) in
		Hashtbl.find ctx.m_pt_types key

	let pt_ctx_get_type_by_string ctx key =
		Hashtbl.find ctx.m_pt_types key



	type gc_gen_ctx = {

		mutable gc_obj_tp_info : (texpr -> texpr);
		mutable gc_func_tp_info : (unit -> texpr);
		mutable or_expr : (texpr list -> texpr);

		mutable gc_pool_alloc : (int -> texpr);
		mutable gc_area_alloc : (texpr -> texpr);

		mutable gc_alloc_ptype : (ptype_info -> texpr -> texpr);
		mutable gc_alloc_ptype_tps : (ptype_info -> texpr -> texpr -> texpr);

		mutable gc_tp_known_expr : (int (*TODO:int64*)-> int -> texpr);

		mutable gc_tp_by_pos_lt_expr : (texpr -> int -> int -> texpr);
		mutable gc_tp_by_pos_gt_expr : (texpr -> int -> int -> texpr);
		mutable gc_tp_by_pos_eq_expr : (texpr -> int -> int -> texpr);

		(*mutable gc_field_ctx : gc_field_ctx;*)

	}

	let par_filter xs ys f =
		List.rev
			(List.fold_left2 (fun xs b c -> if (f b c) then b :: xs else xs ) xs ys [])


	let make_instance = function
		| TClassDecl c -> TInst (c,List.map snd c.cl_params)
		| TEnumDecl e -> TEnum (e,List.map snd e.e_params)
		| TTypeDecl t -> TType (t,List.map snd t.t_params)
		| TAbstractDecl a -> TAbstract (a,List.map snd a.a_params)

	let s_ctx_id ctx = Tid.id (make_instance (TClassDecl ctx.gc_c)) ^ "." ^ ctx.gc_cf.cf_name

	let debug = false

	let p_info ctx s = if debug then Printf.printf "%s : %s\n" s (s_ctx_id ctx) else ()

	let compose f g x = g (f x)

	let and_filter f g x = (f x) && (g x)

	let or_filter f g x = (f x) || (g x)

	let iter_i f l = List.fold_left (fun idx v -> f idx v; idx+1) 0 l

	let  map_i f l =
		let acc,_ = List.fold_left (fun (acc,idx) v -> (f idx v) :: acc,idx+1) ([],0) l in List.rev acc

	let find_i f l =
		let rec loop idx xs = match xs with
			| x :: xs -> if (f x) then idx
						else loop (idx+1) xs
			| _ 	  -> -1
	in loop 0 l

	let var_filter cfb = match (fst cfb).cf_kind with Var _ -> true | _ -> false

	let get_vars cfl = List.filter var_filter cfl

	let method_filter = compose var_filter not

	let no_main_filter = and_filter (fun cfb -> if (fst cfb).cf_name = "main" then false else true) method_filter

	let gmethod_filter = and_filter method_filter (fun cfb -> match (fst cfb).cf_params with [] -> false | _ -> true )

		let fold_left3 f acc a b c =
		let rec loop acc a b c = match a,b,c with
			| a :: ax, b :: bx, c :: cx -> loop (f acc a b c) ax bx cx
			| _,_,_ 					-> acc
		in loop acc a b c

	let fold_left_rev3 a b c f = List.rev (fold_left3 f [] a b c)
(*
	fold_left3 (fun acc a b c -> (a+b+c) :: acc) [] [1;2;3] [4;5;6] [7;8;9]

	fold_left_rev4 [1;2;3] [4;5;6] [7;8;9] [10;11;12] (fun acc a b c d -> (a+b+c+d) :: acc)
*)

	let fold_left4 f acc a b c d =
		let rec loop acc a b c d = match a,b,c,d with
			| (a :: ax, b :: bx, c :: cx, d :: dx) -> loop (f acc a b c d) ax bx cx dx
			| _,_,_,_ -> acc
		in loop acc a b c d

	let fold_left_rev4 a b c d f = List.rev (fold_left4 f [] a b c d)

	let fold_left5 f acc a b c d e =
		let rec loop acc a b c d e = match a,b,c,d,e with
			| (a :: ax, b :: bx, c :: cx, d :: dx, e :: ex) -> loop (f acc a b c d e) ax bx cx dx ex
			| _ -> acc
		in loop acc a b c d e

	let fold_left_rev5 a b c d e fn = List.rev (fold_left5 fn [] a b c d e)

	let fold_left6 fn acc a b c d e f =
		let rec loop acc a b c d e f = match a,b,c,d,e,f with
			| (a :: ax, b :: bx, c :: cx, d :: dx, e :: ex, f:: fx) -> loop (fn acc a b c d e f) ax bx cx dx ex fx
			| _ -> acc
		in loop acc a b c d e f

	let fold_left_rev6 a b c d e f fn = List.rev (fold_left6 fn [] a b c d e f)

	let s_tkind t = (match t with
		| TInst _ -> "TInst"
		| TEnum _ -> "TEnum"
		| TAbstract _ -> "TAbstract"
		| TAnon _ -> "TAnon"
		| TLazy _ -> "TLazy"
		| TType _ -> "TType"
		| TMono _ -> "TMono"
		| TFun  _ -> "TFun"
		| TDynamic _ -> "TDynamic"
	)

	(* expression generation *)

	let mk_var n t = { v_id= -1;v_name=n;v_type=t;v_capture=false;v_extra=None;v_meta=[] }

	let get_class t = match t with TInst(c,_) -> c | _ -> assert false
	let get_anon t = match t with TAnon(a) -> a | _ -> assert false



	let gc_tp_known_expr : (int (*TODO:int64*)-> int -> texpr) ref = ref (fun v p     -> assert false)
	let gc_tp_by_pos_lt_expr : (texpr -> int -> int -> texpr) ref = ref  (fun v sp dp -> assert false)
	let gc_tp_by_pos_gt_expr : (texpr -> int -> int -> texpr) ref = ref  (fun v sp dp -> assert false)
	let gc_tp_by_pos_eq_expr : (texpr -> int -> int -> texpr) ref = ref  (fun v sp dp -> assert false)

	let gc_obj_tp_info : (texpr -> texpr) ref = ref (fun _ -> assert false)
	let gc_func_tp_info : (unit -> texpr) ref = ref (fun () -> assert false)
	let or_expr : (texpr list -> texpr) ref = ref ( fun _ -> assert false )

	let gc_pool_alloc : (int -> texpr) ref = ref (fun _ -> assert false)
	let gc_area_alloc : (texpr -> texpr) ref = ref (fun _ -> assert false)

	let _or_expr t el =
		let mk_or e0 e1 =
			Expr.mk_binop OpOr e0 e1 t null_pos in
		let rec loop el = match el with
			| [e1]           ->  e1
			|  e1 :: el ->  mk_or e1 (loop el)
			| _ -> assert false
		in loop el

	let gc_initialized : bool ref = ref false

	let init_gc_funcs ctx =
		if !gc_initialized then () else
		let hxc = ctx.gc_con.hxc in
		let com = ctx.gc_con.com in
		let t_int64 = hxc.t_int64 in
		let t_int64_p = hxc.t_pointer t_int64 in
		let mk_int = fun v -> Expr.mk_int com v null_pos in
		let mk_int64 = fun v -> Expr.mk_int64 hxc v null_pos in
		let c_gc = hxc.c_gc in
		let mk_binop op e1 e2 =
			Expr.mk_binop op e1 e2 t_int64 null_pos
		in
		let mk_field c ethis n =
				let cf = (PMap.find n c.cl_fields) in
				mk (TField (ethis,(FInstance (c,List.map (fun (_,t) -> t) c.cl_params,cf)))) cf.cf_type null_pos
		in
		let mk_call c ethis n el =
			let ef = mk_field c ethis n in
			let rt = match ef.etype with | TFun(_,r) -> r | _ -> assert false in
			mk (TCall(ef,el)) rt null_pos
		in
		let get_address te =
			Expr.mk_static_call_2 hxc.c_lib "getAddress" [te] null_pos
		in
		(* let by_pos_call te source_pos dest_pos n =
			Expr.mk_static_call_2 c_gc n [te;(mk_int source_pos);(mk_int dest_pos)] null_pos
		in *)
		let mk_parens te = mk (TParenthesis(te)) te.etype null_pos in
		let mask_pos pos =
			mk_binop OpShl (mk_int64 7) (mk_int (pos*3))
		in
		let by_pos te s d =
			mk_parens (
				if      s < d then  mk_binop OpAnd (mk_parens(mask_pos d)) (mk_parens(mk_binop OpShl te (mk_int((d-s)*3))))
				else if s = d then  mk_binop OpAnd (mk_parens(mask_pos d)) (mk_parens te)
				else                mk_binop OpAnd (mk_parens(mask_pos d)) (mk_parens (mk_binop OpShr te (mk_int((s-d)*3))))
			)
		in
		let known_tp v d =
			mk_parens (mk_binop OpShl (mk_int64 v) (mk_int(d*3)))
		in
		gc_tp_by_pos_lt_expr :=
			(fun te s d -> by_pos te s d);
		gc_tp_by_pos_gt_expr :=
			(fun te s d -> by_pos te s d);
		gc_tp_by_pos_eq_expr :=
			(fun te s d -> by_pos te s d);
		gc_tp_known_expr :=
			(fun v dest_pos -> known_tp v dest_pos);
		gc_obj_tp_info :=
			(fun te -> Expr.mk_static_call_2 c_gc "obj_tp_info" [Expr.mk_cast te t_int64_p] null_pos );
		gc_func_tp_info :=
			(fun () -> Expr.mk_local (mk_var (tp_arg_name) t_int64) null_pos);
		or_expr :=
			(fun el -> _or_expr t_int64 el);

		let area = (Expr.mk_static_field_2 c_gc "main_area" null_pos) in
		let c_area = get_class area.etype in
		let pools = (mk_field c_area area "pools") in

		let t_pool = TDB.get_type_by_name "c.gc.Pool" in
		let c_pool = get_class t_pool in
		let get_pool idx = get_address (mk (TArray(pools,mk_int idx)) t_pool null_pos) in
		gc_pool_alloc :=
			(fun idx -> mk_call c_pool (get_pool idx) "alloc" [] );
		gc_area_alloc :=
			(fun te -> mk_call c_area area "alloc" [te] );

		gc_initialized := true;
		()

	(* TODO sync this reliably with Allocator implementation(s) *)

	let log2 x =
		let log = Pervasives.log in
		int_of_float ((log (float_of_int x)) /. (log 2.0))

	let next_pow_2 v =
		let r = 1 lsl (log2 v) in
		if  v > r then r lsl 1 else r

	let get_pool_idx (size : int) =
		let shift =  log2 (next_pow_2 size) in
		if shift < 5 then 0 else shift - 4

	(*let gc_pool_alloc_ptype ctx tctx ectx pt =
		let size,algn = type_to_size_alignment hxc t None in
		!gc_pool_alloc (get_pool_idx size)
		ectx.gc_all*)

	let init_gc_gen_ctx ctx =
		let hxc       = ctx.hxc in
		let com       = ctx.com in
		let t_int64   = hxc.t_int64 in
		let t_int64_p = hxc.t_pointer t_int64 in
		let mk_int    = fun v -> Expr.mk_int com v null_pos in
		let mk_int64  = fun v -> Expr.mk_int64 hxc v null_pos in
		let c_gc      = hxc.c_gc in
		let mk_binop op e1 e2 =
			Expr.mk_binop op e1 e2 t_int64 null_pos
		in
		let mk_field c ethis n =
				let cf = (PMap.find n c.cl_fields) in
				mk (TField (ethis,(FInstance (c,List.map (fun (_,t) -> t) c.cl_params,cf)))) cf.cf_type null_pos
		in
		let mk_call c ethis n el =
			let ef = mk_field c ethis n in
			let rt = match ef.etype with | TFun(_,r) -> r | _ -> assert false in
			mk (TCall(ef,el)) rt null_pos
		in

		let get_address te =
			Expr.mk_static_call_2 hxc.c_lib "getAddress" [te] null_pos
		in
		let mk_parens te = mk (TParenthesis(te)) te.etype null_pos in
		let mask_pos pos =
			mk_binop OpShl (mk_int64 7) (mk_int (pos*3))
		in
		let by_pos te s d =
			mk_parens (
				if      s < d then  mk_binop OpAnd (mk_parens(mask_pos d)) (mk_parens(mk_binop OpShl te (mk_int((d-s)*3))))
				else if s = d then  mk_binop OpAnd (mk_parens(mask_pos d)) (mk_parens te)
				else                mk_binop OpAnd (mk_parens(mask_pos d)) (mk_parens (mk_binop OpShr te (mk_int((s-d)*3))))
			)
		in
		let known_tp v d =
			mk_parens (mk_binop OpShl (mk_int64 v) (mk_int(d*3)))
		in
		let area = (Expr.mk_static_field_2 c_gc "main_area" null_pos) in

		let c_area = get_class area.etype in

		let pools = (mk_field c_area area "pools") in

		let c_pool       = hxc.c_gc_pool in
		let t_pool       = make_instance (TClassDecl c_pool) in
		let get_pool idx = get_address (mk (TArray(pools,mk_int idx)) t_pool null_pos)

		in {

			gc_obj_tp_info   = (fun te -> Expr.mk_static_call_2 c_gc "obj_tp_info" [Expr.mk_cast te t_int64_p] null_pos );
			gc_func_tp_info  = (fun () -> Expr.mk_local (mk_var (tp_arg_name) t_int64) null_pos);
			or_expr          = (fun el -> _or_expr t_int64 el);

			gc_pool_alloc    = (fun idx -> mk_call c_pool (get_pool idx) "alloc" [] );
			gc_area_alloc    = (fun te -> mk_call c_area area "alloc" [te] );

			gc_alloc_ptype   =  (fun pt te_header ->
				mk_call c_pool (get_pool (get_pool_idx pt.pt_size)) "alloc_w_header" [te_header]);
			gc_alloc_ptype_tps = (fun pt te_header te_tps ->
				mk_call c_pool (get_pool (get_pool_idx pt.pt_size)) "alloc_w_header_tps" [te_header;te_tps]);

			gc_tp_known_expr = (fun v dest_pos -> known_tp v dest_pos);

			gc_tp_by_pos_lt_expr = (fun te s d -> by_pos te s d);
			gc_tp_by_pos_gt_expr = (fun te s d -> by_pos te s d);
			gc_tp_by_pos_eq_expr = (fun te s d -> by_pos te s d);

		}

	let tp_info_to_expr ctx dpos tpi =
		init_gc_funcs ctx;
		let by_pos_call te spos dpos =
			if      spos < dpos then !gc_tp_by_pos_lt_expr te spos dpos
			else if spos = dpos then !gc_tp_by_pos_eq_expr te spos dpos
			else                     !gc_tp_by_pos_gt_expr te spos dpos
		in
		match tpi with
		| CTPI_known v     -> !gc_tp_known_expr v dpos
		| CTPI_func  spos  ->  by_pos_call ((!gc_func_tp_info) ()) spos dpos
		| CTPI_class spos  ->  by_pos_call (!gc_obj_tp_info (mk (TConst TThis) (mk_mono()) null_pos)) spos dpos
		| CTPI_not_found   -> !gc_tp_known_expr 5 dpos (* TODO don't hardcode this here *)

	let tp_info_add_func_offset offset tpi = match tpi with
		| CTPI_func s -> CTPI_func (s+offset)
		| x -> x

	let or_tp_info el =	!or_expr el

	(* struct size,alignment and field info *)

	let get_xs_types xs = List.map (fun f -> f.cf_type) xs

	let get_xs_names xs = List.map (fun f -> f.cf_name) xs

	let types_of_tparams l = List.map (fun (_,t)  -> t) l

	let filter_header cf =
		if cf.cf_name = (mk_runtime_prefix "header") || cf.cf_name = (mk_runtime_prefix "vtable")
			then false else true

	(* get all instance fields, including super class fields, excluding headers *)
	let get_class_var_fields c =
		let rec loop c = (match c.cl_super with
				| Some (sc,stp) ->
						loop sc @ c.cl_ordered_fields
				| _ -> c.cl_ordered_fields)
		in List.filter
			(and_filter filter_header (fun f-> match f.cf_kind with Var _ -> true | _ -> false ))
			(loop c)

	let get_anon_fields a = (List.rev (pmap_to_list a.a_fields))

	let get_enum_xs_names t = match t with
		| TEnum(_,_)    -> []
		| TFun (args,_) -> List.map  (fun (n,_,_) -> n ) args
		| _ -> assert false

	let get_enum_xs t = match t with
		| TEnum(_,_)    -> []
		| TFun (args,_) -> List.map  (fun (_,_,t) -> t ) args
		| _ -> assert false

	let get_enum_fields e = (List.map (fun f -> get_enum_xs f.ef_type ) (pmap_to_list e.e_constrs))

	let get_enum_names e  = (List.map (fun f -> get_enum_xs_names f.ef_type ) (pmap_to_list e.e_constrs))

	let rec get_const_size_arr_size t = (match t with
		| TAbstract({a_path = ["c"],"ConstSizeArray"},[t;const]) ->
			let size = (match follow const with
			| TInst({ cl_path=[],name },_) when String.length name > 1 && String.get name 0 = 'I' ->
				String.sub name 1 (String.length name - 1)
			| _ ->
				"1")
			in int_of_string size
		| _ -> assert false)


	(*
		get_field_info:
      	obtain info about kind and size of a field
      	TODO: parameterize for 32 bit platforms
	 *)

	and get_field_info hxc tp = (match tp with

		| TLazy _ -> get_field_info hxc (Type.follow tp)
		| TMono r when (match !r with Some _ -> true | _ -> false) ->
			get_field_info hxc (Type.follow tp)
		| TAbstract ( { a_path =[], "Int" } ,_ )
		| TAbstract ( { a_path =[], "Bool" } ,_ )
		| TAbstract ( { a_path =[], ("hx_int32" | "hx_uint32" | "hx_float32") } ,_ ) ->
			FI_Val 4
		| TAbstract ( { a_path =[], ("hx_int16" | "hx_uint16" | "hx_short" | "hx_ushort") } ,_ ) ->
			FI_Val 2
		| TAbstract ( { a_path =[], ("hx_int8" | "hx_uint8" | "hx_char" | "hx_uchar") } ,_ ) ->
			FI_Val 1
		| TAbstract ( { a_path =["c"], ("Int64" | "UInt64") } ,_ )
		| TAbstract ( { a_path =[], ("Float") } ,_ ) ->
			FI_Val 8
		| TAbstract ( { a_path = ["c"], "Pointer"}, _ )
		| TAbstract ( { a_path = ["c"], "ConstPointer"}, _ ) ->
			FI_Val hxc.arch.ta_refsize
		| TAbstract ( { a_path = ["c"], "FunctionPointer"}, _ ) ->
			FI_Val hxc.arch.ta_fpsize
		| TAbstract({a_path=(["c"],("GCArray"))},[t]) ->
			FI_Array ((get_field_info hxc t),(get_field_info hxc (follow tp)))
		| TAbstract({a_path=(["c"],("GCRefArray"))},[t]) ->
			FI_RefArray ((get_field_info hxc t),(get_field_info hxc (follow tp)))
		| TAbstract({a_path=(["c"],("GCVRefArray"))},[t]) ->
			FI_VRefArray ((get_field_info hxc t), (get_field_info hxc (follow tp)))
		| TAbstract({a_path=(["c"],("GCVRef"))},[t]) ->
			FI_VRef hxc.arch.ta_refsize
		| TAbstract({a_path=(["c"],("ConstSizeArray"))},[t;n]) ->
			let arrsz = get_const_size_arr_size tp in
			let finfo = get_field_info hxc t in
			(match finfo with
			| FI_Val v
			| FI_TParm v (*I don't think this is valid TODO*)
			| FI_Ref v
			| FI_VRef v ->
				FI_CSArr(finfo, arrsz, arrsz*v, v)
			| FI_Array(_,fi)
			| FI_RefArray (_,fi)
			| FI_VRefArray (_,fi) -> (match fi with
				| FI_Struct (sz,algn,fil) -> FI_CSArr(finfo, arrsz, arrsz*sz, algn)
				| _ -> assert false ) (*proper error TODO*)
			| FI_Struct (sz,algn,fil) ->
				FI_CSArr(finfo, arrsz, arrsz*sz, algn)
			| FI_CSArr  (fi,sz,bytes,algn) ->
				FI_CSArr(finfo, arrsz, arrsz*bytes, algn))
		| TAbstract({a_path=(["c"],("Struct"))},[t]) -> (match t with
			| TInst (tc,tp) ->
				let fields = (get_class_var_fields tc) in
				let size,algn = get_fields_size_algn hxc fields in

				FI_Struct(size,algn, get_struct_fields_info hxc fields)
			| TAnon (a) ->
				let fields = (get_anon_fields a) in
				let size,algn = get_fields_size_algn hxc fields in
				FI_Struct(size,algn, get_struct_fields_info hxc fields)
			| _ -> assert false ) (*TODO proper error*)

		| TAbstract((a),_) when (Meta.has Meta.CoreType a.a_meta) ->
			Printf.printf "What do we do with %s? TAbstract((a),_) when (Meta.has Meta.CoreType a.a_meta) -> \n" (Tid.id tp);
			FI_Ref hxc.arch.ta_refsize
		| TAbstract (ta,tp) ->
			get_field_info hxc (Abstract.get_underlying_type ta tp)
		| TInst ({cl_kind=KTypeParameter _},_) ->
			FI_TParm hxc.arch.ta_tpsize
		| TType (_,_) ->
			get_field_info hxc (Type.follow tp)
		| _ ->
			FI_Ref hxc.arch.ta_refsize )


	(*
		for a list of fields, follow C alignment rules to determine offsets of fields in a struct
		returns
			- the total size (including padding)
			- the max alignment used (e.g. for a struct consisting of uint16_t's only that'd be 2 bytes
				(this is somewhat redundant, because the max alignment will always be 8 bytes due to the header)
			- the list of offsets of the respective fields
	 *)
	and struct_alignment l =
		let next (cur_pos,max_algn,acc) (sz,algn) =
			let         r = cur_pos mod algn in
			let start_pos = if r = 0 then cur_pos else cur_pos + (algn-r) in
			let next_pos  = start_pos + sz in
			next_pos, (if max_algn >= algn then max_algn else algn) , start_pos :: acc
		in
		let end_pos,max_algn,offsets = List.fold_left next (0,1,[]) l in
		let r    =  end_pos mod max_algn in
		let size =  end_pos + if r = 0 then 0 else (max_algn - r) in
		size, max_algn, List.rev offsets

	(*
		field_info_to_size_algn
	*)

	and field_info_to_size_algn f = match f with
		| FI_VRef v
		| FI_Val v
		| FI_TParm v
		| FI_Ref v              -> v,v
		| FI_Array (_,fi)
		| FI_RefArray (_,fi)
		| FI_VRefArray (_,fi)   -> field_info_to_size_algn fi
		| FI_CSArr(f,n,bytes,a) -> bytes,a
		| FI_Struct(sz,algn,_)  -> sz,algn

	and field_info_has_refs xs = List.exists (fun fi -> match fi with
		| FI_Ref _            -> true
		| FI_VRef _           -> true
		| FI_Array _
		| FI_RefArray _
		| FI_VRefArray _      -> true
		| FI_Struct(_,_,xs)   -> field_info_has_refs xs
		| FI_CSArr (fi,_,_,_) -> field_info_has_refs [fi]
		| _ -> false
	) xs

	(* 	only use for anons - for classes, even if no field has a TP type, we need storage for them.
 		for enums this could be used if we decide to add TP storage on a per-constructor basis *)

	and field_info_has_tps xs = List.exists (fun fi -> match fi with
		| FI_TParm _          -> true
		(* | FI_Struct(_,_,xs)   -> field_info_has_tps xs - hm what do we do with Struct<T<A,B,C>>  ?? TODO *)
		(*| FI_CSArr (fi,_,_,_) ->  assert false *) (* -> field_info_has_tps [fi] /// we could make this work, but not now TODO *)
		| _ -> false
	) xs

	and get_xs_types xs = List.map (fun f -> f.cf_type) xs

	and get_fields_size_algn hxc cfields =
		let fields                = List.map (get_field_info hxc) (get_xs_types cfields) in
		let sz_algn_xs            = List.map field_info_to_size_algn fields in
		let size,max_algn,offsets = struct_alignment sz_algn_xs in
		size,max_algn

	and get_struct_fields_info hxc cfields = List.map (get_field_info hxc) (get_xs_types cfields)

	let s_field_info f = match f with
		| FI_Ref v          ->  " ref"
		| FI_VRef v         ->  " vref"
		| FI_TParm v        ->  " tp"
		| FI_Array _
		| FI_RefArray _
		| FI_VRefArray _    -> " gc_array"
		| FI_Val v          ->  " val("^ (string_of_int v) ^ ")"
		| FI_Struct(s,a,fil)    ->  " struct("^ (string_of_int s) ^ ")"
		| FI_CSArr(f,n,b,a) ->  " csarr("^ (string_of_int b) ^ ")"

	let get_info hxc xs = List.map (get_field_info hxc) xs

	let get_alignment hxc xs =
		(*let finfo_xs     = (get_info hxc xs) in*)
		let sz_algn_xs   = List.map field_info_to_size_algn xs in
		let size,max_algn,offsets = struct_alignment sz_algn_xs in
		(xs,offsets,size)

	(* takes a type - either a class, an enum or an anon

		we're skipping type ids, to be able to obtain any types info with a single array access

		the first set of N tpinfo is e.g. 16 bytes, we give them the first N ids, then if the second set
		is e.g. max 32 bytes we skip each second id, for 64 bytes we skip each 2nd third and fourth id, etc.

	*)


	(* TODO add val ref *)

	let find_tp st xs =
		let rec loop idx xs = match xs with
			| (_,t) :: _ when st = (Tid.id t) -> idx
			| tp :: xs -> loop (idx+1) xs
			| _ -> (-1)
		in loop 0 xs

	let is_tp = function TInst({cl_kind=KTypeParameter _},_) -> true | _ -> false


	let _types_to_runtime_info hxc t types tps offset_start =
		(* let hxc = ctx.gc_con.hxc in *)

		let pos_by_tp =
			let indices = map_i ( fun idx _ -> idx ) tps in
			let m = List.fold_left2 (
				fun m idx t ->
					let tid = (Tid.id t) in
					if not (PMap.exists tid m) then
						PMap.add (Tid.id t) idx m
					else
						m
				) PMap.empty indices tps in m
		in

		let field_info, offsets, size =  get_alignment hxc (List.map (get_field_info hxc) types) in
		let offsets = List.map (fun i -> i + offset_start) offsets in
		let acc = List.rev ( List.fold_left2 ( fun acc t (offs,fi) ->
			(match fi,(offs mod 8) with
			| FI_Val    v,0 -> RT_val(offs,0) :: acc
			| FI_Ref    v,0 -> (RT_ref offs) :: acc
			| FI_TParm  v,0 ->
					let idx = PMap.find (Tid.id t) pos_by_tp in
					RT_tp(offs,idx) :: acc
			| FI_Struct(s,a,fil),_ ->
				RT_val(offs,0)  :: acc(*TODO make struct members known to the gc*)
			| FI_CSArr(f,n,b,a),_ ->
				RT_val(offs,0)  :: acc  (*TODO make array members known to the gc*)
			| FI_Val    v,_ -> RT_val(offs,0) :: acc
			| _ ->
				Printf.printf "non-val field not 8 byte aligned.. bad\n";
				acc
			)

		) [] types (List.map2 (fun o f -> o,f ) offsets field_info))
		in
		(*Printf.printf "RTI :: %s :: %s \n" (Tid.id t) (s_rti_l acc);*)
		acc

	let types_to_runtime_info hxc t types tps = _types_to_runtime_info hxc t types tps 0


	(*
	    return the types of all fields, prepend the fields of respective headers,

	 *)

	let prepend_header_fields hxc t = (match follow t with
		| TInst(c,tps) ->
			let hfields = get_class_var_fields hxc.c_gc_classheader in
			(*Printf.printf "hfl %d \n" (List.length hfields);*)
			let cfields = get_class_var_fields c in
			hfields @ cfields
		| TEnum(e,tps) ->
			(* let hfields = get_class_var_fields hxc.c_gc_enumheader in *)
			[]
		| TAnon(a) ->
			let hfields = get_class_var_fields hxc.c_gc_anonheader in
			(*Printf.printf "hfl %d \n" (List.length hfields);*)
			let cfields = get_anon_fields a in
			hfields @ cfields
		| _ -> assert false
	)

	let type_to_runtime_info hxc t anon_tps_opt = match t with
		| TInst(c,tps) ->
			(* let types =  get_xs_types (get_class_var_fields c) in *)
			let types =  get_xs_types (prepend_header_fields hxc t) in
			types_to_runtime_info hxc t types (types_of_tparams c.cl_params)

		| TEnum(e,tps) ->
			[]
			(*_types_to_runtime_info ctx (types_of_tparams e.e_params)*)
		| TAnon(a) ->
			let types =  get_xs_types (prepend_header_fields hxc t) in
			(match anon_tps_opt with
				| Some(tps) ->
				  types_to_runtime_info hxc t types (tps)
				| None ->
				  types_to_runtime_info hxc t types []
			)
		| _ -> []


	(* TODO deal with prepending header fields better *)
	let type_to_size_alignment hxc t efopt =
		(*let hxc = ctx.gc_con.hxc in *)
		let f t = (match t,efopt with
			| TInst(c,_),None ->
				let cfields = get_class_var_fields c in
				let types = get_xs_types cfields in
				let types = hxc.t_int64 :: hxc.t_int64 :: types in (*TODO here*)
				get_info hxc types
			| TEnum(e,_),Some(ef) ->
				get_info hxc (hxc.t_int64 :: (get_enum_xs ef.ef_type)) (*TODO here*)
			| TAnon(a),None ->
				let types = hxc.t_int64 :: (get_xs_types (get_anon_fields a)) in (*TODO here*)
				get_info hxc types
			| _ -> assert false)
		in
		let info,algn,size = get_alignment hxc (f t) in
		if debug then Printf.printf "%s:\nsize: %d\ninfo: %s\nalgn: %s\n\n"
				(Tid.id t)
				size
				(String.concat ", " (List.map s_field_info info))
				(String.concat ", " (List.map string_of_int algn)) else ();
		size,algn

	(*  end struct size,alignment and field info *)


	(* TODO sync this reliably with Allocator implementation(s) *)

	let log2 x =
		let log = Pervasives.log in
		int_of_float ((log (float_of_int x)) /. (log 2.0))

	let next_pow_2 v =
		let r = 1 lsl (log2 v) in
		if  v > r then r lsl 1 else r

	let get_pool_idx (size : int) =
		let shift =  log2 (next_pow_2 size) in
		if shift < 5 then 0 else shift - 4

	let gc_pool_alloc_type hxc t =
		let size,algn = type_to_size_alignment hxc t None in
		!gc_pool_alloc (get_pool_idx size)


	(* instantiations - anything that allocates memory. *)
	let instantiations ctx te = match te with
		| { eexpr = TField(_,FStatic(ct,cf)); etype = TFun(_,_) } -> ()
		| { eexpr = TField(_,FClosure(ct,cf)) } -> ()
		| { eexpr = TFunction(tf) } -> ()
		| { eexpr = TConst(TString s) } -> ()
		| { eexpr = TBinop(OpAdd,te1,te2) } when StringHandler.is_string te.etype -> ()
		| { eexpr = TNew(c,params,args) } -> ()
		| { eexpr = TArrayDecl(el) } -> ()
		| { eexpr = TObjectDecl(nvs) } -> ()
		| { eexpr = TCall({eexpr=TField(_,FEnum(et,ef))},el); etype = t } -> ()
		| _ -> ()

	(* assignments - for write barrier etc. *)
	let assignments ctx te = match te with
		|  { eexpr = TBinop(OpAssign,{eexpr=TField(te1,(FInstance(_,_,cf)|FStatic(_,cf)|FAnon(cf)))},te2) } -> ()
		|  { eexpr = TBinop(OpAssign,{eexpr=TArray(te1,teidx)},te2) } -> ()
		| _ -> ()

	(* calls - add tpinfo here *)
	let calls ctx te = match te with
		|  { eexpr = TCall({eexpr=TField(te1,fa)},el) } -> ()
		|  { eexpr = TCall(te1,el) } -> ()
		| _ -> ()

	let get_types te =
		let types : Type.t list ref = ref [] in
		let rec loop te =
			types := (te.etype :: !types);
			Type.iter loop te
		in (loop te);
		!types

	let get_field_expressions c =
		let rec loop xs acc = (match xs with
			x :: xs -> (match x.cf_expr with
				| Some te -> loop xs (te :: acc)
				| None -> loop xs acc )
			| _ -> acc)
		in loop ( c.cl_ordered_fields @ c.cl_ordered_statics ) []

	let get_class_instance_fields c = c.cl_ordered_fields
		(* let rec loop c = (match c.cl_super with
				| Some (sc,stp) -> loop sc @ c.cl_ordered_fields
				| _ -> c.cl_ordered_fields)
		in loop c *)


	let get_class_static_fields c = c.cl_ordered_statics
		(* let rec loop c = (match c.cl_super with
				| Some (sc,stp) -> loop sc @ c.cl_ordered_statics
				| _ -> c.cl_ordered_statics)
		in loop c *)


	(*let smethod_filter = and_filter method_filter (fun cf -> cf)*)

	let prepend_arg f v = { f with tf_args = (v,None) :: f.tf_args }

	let pm_length m = PMap.fold (fun _ n -> n+1) m 0



	let get_func t = match t with TFun(args,r) -> (List.map(fun (_,_,t) -> t) args) ,r | _ -> assert false


	(* The following are the functions that deal with looking up type parameter information

		there are two different use-cases to consider
			1. allocating a  class, enum, anon or closure
				only requires calling the appropriate allocation function, doesn't require modifying
			2. calling a function


	 *)

	let type_to_callsite_tp_info c fctx t =
		match fctx with
		(* in __init__ and static var initialization, there cannot be any type parameters from an outer frame *)
		| GC_F_init            -> ()

		| GC_F_static_var   cf -> ()
		(* in an instance var initialization there can only be class type parameters *)
		| GC_F_instance_var cf -> ()
		(* in a static function there can only be type parameters passed to the function as argument *)
		| GC_F_static_func  cf -> ()
		(* in both constructor and method there can be both class and function type parameters *)
		| GC_F_constructor  cf
		| GC_F_method       cf -> ()
		| GC_F_closure      _  -> ()



	(* for every callees type parameter, return the first search_result found or SR_none *)
	let get_tp_pos_info_from_callee in_t tps =
		let id = Tid.id in
		let enter_search_tp f t =
			let rec search_tp t =
				if f t then SR_self
				else (match (Type.follow t) with
				| TInst(_,xs) | TEnum(_,xs) |  TAbstract(_,xs) | TType (_,xs) ->
					let rec loop idx xs = (match xs with
						| t :: xs -> (match search_tp t with
										| SR_none -> loop (idx+1) xs
										| sr      -> SR_tp(sr,idx,t) )
						| _ ->  SR_none )
					in loop 0 xs
				| TFun (xs,rt) ->
					( match search_tp rt with
					| SR_none ->
						let rec loop idx xs = ( match xs with
							| (_,opt,t) :: xs -> ( match search_tp t with
											| SR_none -> loop (idx+1) xs
											| sr      -> SR_f_arg(sr,idx) )
							| _ ->  SR_none )
						in loop 0 xs
					| sr -> SR_f_ret sr
					)
				| TAnon({a_fields = xs}) ->
					let rec loop xs = match xs with
						| cf :: xs -> (	match search_tp cf.cf_type with
										| SR_none -> loop xs
										| sr      -> SR_field (sr,cf.cf_name) )
						| _ ->  SR_none
					in loop (pmap_to_list xs)
				| TDynamic _ -> (* what's t here ??? *)
					Printf.printf ":( ";
					SR_none
				| _ -> SR_none )
			in
			search_tp t
		in List.map ( fun t ->
			let f = ( fun t2 -> id t = id t2 ) in
			enter_search_tp f in_t
		) tps


	let type_to_callsite_tpi ctx t = (match (Type.follow t) with
		| TInst({cl_kind = KTypeParameter _}, _) ->
			let st  = Tid.id t in
			let idx = find_tp st ctx.gc_cf.cf_params in
			if idx > -1 then
				CTPI_func idx
			else if not ctx.gc_isstatic then
				CTPI_not_found
			else
				let idx = find_tp st ctx.gc_c.cl_params in
				if idx > -1 then
					CTPI_class idx
				else
					CTPI_not_found
		| _ ->  (* we have a concrete type - get its tp info value and wrap that into a CTPI_known *)
			let fi  = get_field_info ctx.gc_con.hxc t in
			let tpi = field_info_to_tpi fi in
			let v   = tp_info_to_val tpi in
			if debug then Printf.printf "type: %s fi: %s tpi: %s val: %d \n"(Tid.id t) (s_field_info fi) (s_tp_info tpi) v else ();
			CTPI_known v
	)

	let sr_to_ctpi ctx rtype eargs_ctx sr =
		let rec extract_type t sr =
			(match (Type.follow t),sr with
			| TMono _,             SR_self          -> CTPI_not_found
			| t,                   SR_self          -> type_to_callsite_tpi ctx t
			| _,                   SR_none          -> CTPI_not_found

			| TAnon({a_fields=xs}),SR_field(sr,n)   -> (try let cf = PMap.find n xs
														in extract_type cf.cf_type sr
														with Not_found -> CTPI_not_found)

			| TFun(xs,_),          SR_f_arg(sr,idx) -> (try let (_,_,t) = List.nth xs idx
														in extract_type t sr
														with  _ -> CTPI_not_found (* | _ -> assert false *))
														(*with ExtList.List.Invalid_index _ -> CTPI_not_found | _ -> assert false)*)
			| TFun(_,r),           SR_f_ret(sr)     -> extract_type r sr

			| (TInst(_,xs)
			| TEnum(_,xs)
			| TAbstract(_,xs)
			| TType(_,xs)),        SR_tp(sr,idx,t)  -> (try let t = List.nth xs idx
														in extract_type t sr
														with _ -> CTPI_not_found (* | _ -> assert false *))
			| _ -> assert false
		) in
		extract_type
			(TFun((List.map (fun e -> ("_",false,e.etype)) eargs_ctx),rtype))
			sr

	let sr_has_any xs =
		(List.length (List.filter (fun sr -> match sr with SR_none -> false | _ -> true ) xs)) > 0

	let prepend_arg_type con t = Expr.prepend_tp_arg_type con.hxc t

	(* if we're in a constructor, our source func parameters have an offset *)
	let adjust_callsite_tpi_positions ctx callsite_tpi =
		if ctx.gc_iscon then
			List.map (tp_info_add_func_offset (List.length ctx.gc_cf.cf_params)) callsite_tpi
		else
			callsite_tpi

	(* build the type info to pass to the callee or constructed type *)
	let _add_tp_info_arg_known ctx fopt con_tps =

		let callsite_tpi = (match fopt with (* are we calling a function/constructor at all, or are we instantiating something else ? *)
			| Some (cf,rtype,eargs_l) -> (*yes*)
				let sr_l = get_tp_pos_info_from_callee cf.cf_type (List.map snd cf.cf_params) in
				p_info ctx (String.concat " :: " (List.map s_sr sr_l));
				(*  WARNING : note that we haven't actually added the argument at the CALLSITE so far,
					so argument indices ARE ONE POS OFF! We fix that by using a dummy argument list where
					we prepend one argument *)
				let eargs_ctx = (Expr.mk_int ctx.gc_con.com 0 null_pos) :: eargs_l in
				List.map (sr_to_ctpi ctx rtype eargs_ctx) sr_l
			| _ -> (*no*)
				[])
		in
		(* to get the source of TP positions right we have to distinguish a few cases,
			because we pass constructor type parameters for the class with the
			type parameters for the constructor function in the same bitfield
			 - in a constructor, calling a constructor - offset both source and dest
			 - in a function calling a constructor     - offset dest
			 - in a constructor calling a function     - offset source
			 - in a function calling a function        - offset none  *)

		(* if we're in a constructor, our source func parameters have an offset *)
		let callsite_tpi = adjust_callsite_tpi_positions ctx callsite_tpi in

		let callsite_tpi = match con_tps with (* calling a constructor with function type parameters ? *)
			| []  -> (*we're not calling `new` or it doesn't have function type parameters*)
				callsite_tpi
			| tps -> (*we're calling `new` so we prepend the class type parameters *)
				(List.map (type_to_callsite_tpi ctx) tps) @ callsite_tpi
		in

		p_info ctx (String.concat ":ctx:" (List.map s_ctpi callsite_tpi));

		(* Now generate the expressions that build the tpinfo for the call *)

		(* 1. first build individual expressions per type parameter*)
		let e_tpi_l = map_i (tp_info_to_expr ctx) callsite_tpi in

		(* 2. OR them up *)
		let e_tpinfo = or_tp_info e_tpi_l in e_tpinfo

	let _add_tp_info_arg_callsite ctx ecall e1 el cfopt =
		(*let _ = get_tp_info_from_context ctx el in*)
		let e_tpinfo = (match cfopt with
		| Some cf -> (*we know what we're calling, not a closure*)
			_add_tp_info_arg_known ctx (Some(cf,ecall.etype,el)) []
		| None -> (*we've no idea what we're calling, IOW a closure, this is the fun part TODO *)
			Expr.mk_int ctx.gc_con.com 0 null_pos
		) in (* here we can add the actual argument to the call *)
		{ ecall with eexpr=TCall(
			{ e1 with etype = prepend_arg_type ctx.gc_con e1.etype },
			e_tpinfo :: el
		)}

	let add_tp_info_callsite ctx te =
		let rec loop te = match te.eexpr with
		| TCall({eexpr=TField(eenum,FEnum(et,ef))} as efield, el) ->
			(match te.etype with
				| TEnum(_,[])  -> te
				| TEnum(_,tps) ->
					p_info ctx ("add enum info, calling " ^ ef.ef_name ^ " of " ^ (s_type_path et.e_path));
					let sr_l = get_tp_pos_info_from_callee ef.ef_type (List.map snd et.e_params) in
					(* the following is essentially the same as _add_tp_info_arg_known, but
					   in case the constructor doesn't have any type parameter arguments we don't add info *)
					if not (sr_has_any sr_l) then
						te
					else
						(*let rtype =  ctx.gc_con.com.basic.tvoid in (*fake return type*)
						let eargs_ctx = (Expr.mk_int ctx.gc_con.com 0 null_pos) :: el in
						let callsite_tpi = List.map (sr_to_ctpi ctx rtype eargs_ctx) sr_l in*)
						let callsite_tpi = (List.map (type_to_callsite_tpi ctx) tps) in
						p_info ctx (String.concat ":ctx:" (List.map s_ctpi callsite_tpi));
						let e_tpi_l = map_i (tp_info_to_expr ctx) callsite_tpi in
						let e_tpinfo = or_tp_info e_tpi_l in
						let efield = { efield with etype = prepend_arg_type ctx.gc_con efield.etype } in
						{ te with eexpr = TCall(efield, (e_tpinfo :: el) ) }
				| _ -> assert false

			)
		| TCall(({eexpr=TField(_,(FStatic(({cl_extern = false} as c),cf) | FInstance(c,_,cf)))} as e1),el) ->
			(* c.gc.Memory allocation *)
			(match c with
				| {cl_kind = KAbstractImpl { a_path=["c";"gc"],"Memory"; a_params=[(s,t)]}}
					->  p_info ctx ("MEMCALL" ^ cf.cf_name ^ " " ^ (Tid.id te.etype));
						let tp = (match te.etype with
							| TAbstract(_,[t]) -> t
							| _ -> assert false ) in
						let size,algn = type_to_size_alignment ctx.gc_con.hxc tp None in
						!gc_pool_alloc (get_pool_idx size)
			| _ ->
			(match cf.cf_params with
					| []        ->   te
					| _  ->
						p_info ctx ("add call info, calling " ^ cf.cf_name ^ " ");
						_add_tp_info_arg_callsite ctx te e1 el (Some cf)
			))
		| TCall(e1,el) when ClosureHandler.is_closure_expr e1 ->
			p_info ctx "add call info closure";
			let clo_t = ctx.gc_con.hxc.t_closure (mk_mono()) in
			let clo_class = get_class clo_t in
			let _has_tp = (PMap.find "_has_tp" clo_class.cl_fields) in
			let econd = mk (TField(e1,FInstance(clo_class,List.map (fun (_,t) -> t) clo_class.cl_params,_has_tp))) ctx.gc_con.com.basic.tbool null_pos in
			{ te with eexpr = TIf(econd,_add_tp_info_arg_callsite ctx te e1 el None, Some te) }
		| TNew(c,ctps,el) ->
			(* for now we pass both class TPs and constructor TPs in the same argument,
				(and nobody uses constructor TPs anyway) this is a little convoluted - *)
			let cf = match c.cl_constructor with Some cf -> cf | _ -> assert false in
			(match ctps,cf.cf_params with
				| ( [],[])        ->   te
				| (ctps,ftps)        ->   (* just constructor TPs *)
					p_info ctx ("add new info, calling " ^ cf.cf_name ^ " of " ^ (s_type_path c.cl_path));
					let e_tpinfo =  _add_tp_info_arg_known ctx (Some(cf,te.etype,el)) ctps in
					{ te with eexpr = TNew(c,ctps, (e_tpinfo :: el) ) }
			)
		| TObjectDecl(el) ->
			let te = {te with eexpr = TObjectDecl (List.map (fun (n,e) -> n,loop e) el) } in
			let a = get_anon te.etype in
			let tfields = get_anon_fields a in

			(* extract the type parameters from the field types *)
			let rec loop xs acc = (match xs with
				| ({cf_type = TInst({cl_kind=KTypeParameter _},_)} as cf) :: xs ->
					loop xs (cf :: acc)
				| x :: xs -> loop xs acc
				| _ -> List.rev acc ) in
			(match loop tfields [] with
				| [] ->
					let _ = type_to_runtime_info ctx.gc_con.hxc (TAnon(a)) (Some []) in ();
					te
				| tp_fields ->
					let tps = get_xs_types tp_fields in
					let callsite_tpi = (List.map (type_to_callsite_tpi ctx) tps) in
					p_info ctx (String.concat ":ctx:" (List.map s_ctpi callsite_tpi));
					let e_tpi_l = map_i (tp_info_to_expr ctx) callsite_tpi in
					let e_tpinfo = or_tp_info e_tpi_l in

					let nfield = Type.mk_field tp_arg_name ctx.gc_con.hxc.t_int64 null_pos in
					a.a_fields <- PMap.add tp_arg_name nfield a.a_fields;

					let _ = type_to_runtime_info ctx.gc_con.hxc (TAnon(a)) (Some tps) in ();

					{ te with
						eexpr = TObjectDecl( (tp_arg_name,e_tpinfo) :: el );
						etype = TAnon(a)
					}
			)
		| _ -> Type.map_expr loop te
		in loop te

	let add_tp_info_to_function ctx te = match te.eexpr with
		| TFunction f ->
			p_info ctx "add arg info";
			let tp_arg_t = ctx.gc_con.hxc.t_int64 in
			{ te with
				eexpr = TFunction ( prepend_arg f (mk_var tp_arg_name tp_arg_t ));
				etype =  prepend_arg_type ctx.gc_con te.etype
			}
		| _ -> assert false

	let add_tp_info_enum_con con et ef =
		let sr_l = get_tp_pos_info_from_callee ef.ef_type (List.map snd et.e_params) in
		if not (sr_has_any sr_l) then
			ef.ef_type
		else
			prepend_arg_type con ef.ef_type


	let map_fields con c f filter fields iscon =
		let rec loop cfl = (match cfl with
			| (cf,s) :: cfl ->
				let ctx = {gc_con=con;gc_c=c;gc_cf=cf;gc_isstatic=s;gc_iscon=iscon} in (match cf.cf_expr with
					| Some te ->
						let e = f ctx te in
						cf.cf_expr <- Some e;
						cf.cf_type <- e.etype
					| _ -> ()
				);
				loop cfl
			| _ -> ())
		in loop (List.filter filter fields)

	let no_filter _ = true

	let fields_b b l = List.map (fun f -> (f,b)) l

	let type_decl_info con t =

		let ct = (make_instance t) in

		let _ = TDB.add ct in (match t with
		| TClassDecl    tc ->
			if debug then Printf.printf "CLASS:::%s\n" (Tid.id ct);
			let _ = type_to_runtime_info con.hxc ct None in ();
			List.iter (fun t -> TDB.add t) (List.flatten (List.map get_types (get_field_expressions tc)));
			let all_fields = ((get_class_instance_fields tc) @ (get_class_static_fields tc)) in
			List.iter  (fun t -> TDB.add t) (List.map ( fun f -> f.cf_type) all_fields );
			(*
			Printf.printf "\n anons: %d" (pm_length !TDB.tdb_anons);
			Printf.printf "\n classes: %d" (pm_length !TDB.tdb_classes);
			Printf.printf "\n abs: %d" (pm_length !TDB.tdb_abstracts);
			Printf.printf "\n enums: %d" (pm_length !TDB.tdb_enums);
			Printf.printf "\n tdefs: %d" (pm_length !TDB.tdb_defs);
			Printf.printf "\n funcs: %d" (pm_length !TDB.tdb_funcs);
			Printf.printf "\n all: %d" (pm_length !TDB.tdb_types);
			Printf.printf "\nclass %s fields: %s"
				(s_type_path tc.cl_path)
				(String.concat ", " (List.map (fun f -> f.cf_name) (get_class_instance_fields tc) ));
				()
			*)
		| TEnumDecl te -> ()
		| TTypeDecl td ->
			TDB.add td.t_type
		| TAbstractDecl ta ->
			TDB.add ta.a_this)



	let run_snd con td = (match td with
		| TClassDecl tc ->
			let cons = match tc.cl_constructor with
				| Some cf->	let _ = match tc.cl_params,cf.cf_params with
							| ([],[]) -> ()
							| _       ->
								if debug then Printf.printf "class con add arg %s\n" (s_type_path tc.cl_path) else ();
								map_fields con tc add_tp_info_to_function no_filter [cf,false] true
							in [(cf,false)]
				| _ -> []
			in
			(match tc.cl_init with
				| Some te ->
					let ctx = {  gc_con=con;
						         gc_c =tc;
								 gc_cf= mk_field "__init__" con.com.basic.tvoid null_pos;
								 gc_isstatic=true;
								 gc_iscon=false
					} in
					tc.cl_init <- (Some (add_tp_info_callsite ctx te))
				| _ -> ());

			let stats = ( fields_b true tc.cl_ordered_statics ) in
			let insts = ( fields_b false tc.cl_ordered_fields ) in
			let both  = insts @ stats in

			map_fields con tc add_tp_info_to_function gmethod_filter both false;
			map_fields con tc add_tp_info_callsite no_filter both false;
			map_fields con tc add_tp_info_callsite no_filter cons true;

		| TEnumDecl et ->
			if (List.length et.e_params) > 0 then begin
				let set_type n =
					let ef = PMap.find n et.e_constrs in
					let ef = { ef with ef_type = (prepend_arg_type con ef.ef_type) } in
					et.e_constrs <- PMap.add n ef et.e_constrs
				in List.iter set_type et.e_names
				end
			else ()
		| _ -> ())



		(* 2.1 *)
		let match_caller_tp_with_callsite_type = ()

	type k_info = string * field_info list

	let k_map : (k_info,int) PMap.t ref = ref PMap.empty

	let p_alignment hxc =

		let show_xs what n xs =
			let info,algn,size = get_alignment hxc xs in
			if not (size > 0) then () else
				Printf.printf "%s: %d instances\nsize: %d\ninfo: %s\nalgn: %s\n\n"
				what
				n
				size
				(*(String.concat ", " (get_xs_names cfields))*)
				(String.concat ", " (List.map s_field_info info))
				(String.concat ", " (List.map string_of_int algn))
		in
		let to_fi_key t = (match t with
			| TInst(c,_) ->
				let cfields = get_class_var_fields c in
				let types = get_xs_types cfields in
				[Tid.id t, (get_info hxc types)]
			| TEnum(e,_) ->
				List.map (fun l -> (Tid.id t),(get_info hxc l)) (get_enum_fields e)
			| TAnon(a) -> [(Tid.id t),(get_info hxc (get_xs_types (get_anon_fields a)))]
			| _ -> [])
		in

		let collect t =
			List.iter ( fun k -> if PMap.mem k !k_map
				then k_map := PMap.add k ((PMap.find k !k_map) + 1) !k_map
				else k_map := PMap.add k 1 !k_map )
			(to_fi_key t)
		in
		let show () =
			PMap.iter (fun (what,xs) n -> show_xs what n xs ) !k_map
		in
		TDB.iter collect;
		show ();
		Printf.printf "\n%d unique types \n" (pm_length !TDB.tdb_types);
		Printf.printf "\n%d unique types WRT to GC marking\n" (pm_length !k_map)

	let ptypes_types pts = List.map (fun pt -> pt.pt_type) pts

	(*
		returns:
		rtinfo for all fields, flattening Struct<T> T's fields into the rt_info.
		notes:
		ConstSizeArray<T> should be handled the same, but figuring out more space efficient encoding is a TODO.
		For now, GC considers ConstSizeArray as one big value, so using val-only types should be OK.
		This has to be thoroughly tested with various C compilers, 32 bit also is a TODO.
	*)



	let pt_super_chain pt =
    	let rec loop pt acc = (match pt.pt_super with
    		| Some(pt) -> loop pt (pt :: acc)
    		| _        -> acc)
    	in pt :: List.rev (loop pt [])

    let pt_has_tps pt =
    	List.exists (fun pts -> pts.pt_has_tps ) (pt_super_chain pt)

    let pt_interfaces pt =
    	let xxs = List.map ( fun pt -> pt.pt_ifaces ) ( pt :: (pt_super_chain pt) ) in
    	List.flatten xxs

    let pt_tps pt = (match pt.pt_type with
		| TInst(c,_) -> (types_of_tparams c.cl_params)
		| TEnum(e,_) -> (types_of_tparams e.e_params)
		| TAnon(a)   ->
				List.rev ( List.fold_left (fun acc pt ->
					(match pt.pt_type with
						| TInst({cl_kind = KTypeParameter _},_) -> pt.pt_type :: acc
						| _ -> acc ) )
					[] pt.pt_field_types )
		| _ -> []
	)

    let pt_all_tps pt = (match pt.pt_type with
		| TInst(c,_) ->
			List.fold_left
				(fun xs pt -> (pt_tps pt) @ xs) [] (pt :: (pt_super_chain pt))
		| _ -> pt_tps pt )

    let pt_pos_by_tp pt =
		let tps = pt_all_tps pt in
		let indices = map_i ( fun idx _ -> idx ) tps in
		let m = List.fold_left2 (
			fun m idx t ->
				let tid = (Tid.id t) in
				if not (PMap.exists tid m) then
					PMap.add (Tid.id t) idx m
				else
					m
			) PMap.empty indices tps
		in m



	let pt_get_body_and_header_field_types pt =
		if pt.pt_no_header then (*only required for the header classes*)
			pt.pt_field_types,pt.t_field_types
		else
			pt.pt_header_field_types @ pt.pt_field_types,
			pt.t_header_field_types @ pt.t_field_types

	let pt_get_tp_pos pt t =
		begin
			try let pos = PMap.find (Tid.id t) pt.pt_pos_by_tp in
				pos
			with Not_found ->
				(Printf.printf "error: didn't find TP %s of PT %s in map (%s)\n"
				(Tid.id t)
				(Tid.id pt.pt_type)
				(String.concat "," ( PMap.foldi (fun a b xs -> a :: xs ) pt.pt_pos_by_tp [] )) );
				assert false
		end

	(* TODO exclude header fields based on requirements to reduce size *)
	let pt_init_header_field_types tctx pt =
		let hd_pts,hd_ts =
			if pt.pt_no_header then
				[],[]
			else
				let pt_head = (match pt.pt_type with
				| TInst _ -> pt_ctx_get_type_by_string tctx "c.gc.ClassHeader"
				| TEnum _ -> pt_ctx_get_type_by_string tctx "c.gc.EnumHeader"
				| TAnon _ -> pt_ctx_get_type_by_string tctx "c.gc.AnonHeader" | _ -> assert false)
				in pt_head.pt_field_types, pt_head.t_field_types
		in
		pt.pt_header_field_types <- hd_pts;
		pt.t_header_field_types  <- hd_ts;
		()

	let ref_kind pt offs = (if pt.pt_is_valref then (RT_vref offs) else (RT_ref offs))



	let _get_struct_tp t = (match t with
		| TAbstract({a_path=(["c"],("Struct"))},[t]) -> t
		| _ -> assert false )

	let get_struct_tp tctx t = (match t with
		| TAbstract({a_path=(["c"],("Struct"))},[t]) -> pt_ctx_get_type tctx t
		| _ -> assert false )

	let get_struct_tp_with_pt tctx t_in =
		let t = _get_struct_tp t_in in t,(pt_ctx_get_type tctx t)

	let get_gc_array_tp t =
		let stp = follow (_get_struct_tp t) in (match stp with
			| TInst({cl_path=(["c"],("GCArrayC"))},[t]) -> t
			| _ ->
				Printf.printf "assert: should be a GCArrayC: %s\n" (Tid.id stp);
				assert false )

	(*let get_tp_pos t =
			begin try let pos = PMap.find (Tid.id t) pos_by_tp in
					pos
				with Not_found ->
					(Printf.printf "error: didn't find %s in %s\n"
					(Tid.id t)
					(String.concat "," ( PMap.foldi (fun a b xs -> a :: xs ) pos_by_tp [] )) );
					assert false
				end
		in*)

	let pt_iter_ptypes f xs =
		List.iter (fun pt -> match pt.pt_type with
			| TAnon _
			| TInst _ -> f pt
			| TEnum _ -> List.iter f pt.pt_constrs
			| _ -> assert false
		)

	let pt_print stype pt =
		let acc = fold_left_rev3 pt.pt_field_names pt.t_field_types pt.pt_field_offsets
				  (fun acc       n                 t                offs               ->
				(Printf.sprintf "    name: %s type:%s offset: %d\n" n (stype t) offs) :: acc
			)
		in
		let fields = String.concat "" acc in
		Printf.printf "TYPE %s\n%s\n" pt.pt_key fields;
		()

	let rec _ptypes_to_runtime_info hxc tctx t_in pt_in offset_start =

		let ptypes,
			types = pt_get_body_and_header_field_types pt_in in
		let field_info,
			offsets,
			size = get_alignment hxc (List.map (get_field_info hxc) types) in
		let offsets = List.map (fun i -> i + offset_start) offsets in

			(* replace the first fields RT_vref with the appropriate RT_???arr - it marks GCArrays kind, so the GC knows how
			   to handle it - could be a lot cleaner TODO *)
		let gc_arr_replace_first_field field t_in offs acc =
			let t, pt = get_struct_tp_with_pt tctx t_in in
			(match _ptypes_to_runtime_info hxc tctx t pt offs with
				| (hd :: xs),_ -> List.rev (field :: xs) @ acc
				| _            ->
					Printf.printf "assertion failed, must be GCArrayC: %s \n" (Tid.id t_in);
					assert false ) in (* is a struct with at least two fields *)

		(*Printf.printf "processing %s ::>%s<::\n" (Tid.id pt_in.pt_type) (String.concat ":" pt_in.pt_field_names);*)
		let acc = fold_left_rev4 types ptypes offsets field_info
				  (fun acc       t     pt     offs    fi         ->
			let t = follow t in (* skip non-core abstracts *)
			(match fi,(offs mod 8) with
			| FI_Val    v,0       ->     RT_val(offs,0) :: acc
			| FI_Ref    v,0       -> (ref_kind pt offs) :: acc
			| FI_VRef   v,0       ->     (RT_vref offs) :: acc
			| FI_Array  (fi,_),0  -> let refk = (match fi with
				| FI_Ref _ ->
					(match ref_kind (pt_ctx_get_type tctx (get_gc_array_tp t)) offs with
					| RT_ref offs  -> RT_refarr offs
					| RT_vref offs -> RT_valarr offs | _ -> assert false)
				| FI_VRef _  -> RT_valarr offs
				| FI_TParm _ -> RT_tparr (offs, (pt_get_tp_pos pt_in (get_gc_array_tp t)))
				| FI_Val   _ -> RT_vref offs
				| _ -> assert false
				) in
				gc_arr_replace_first_field refk t offs acc
			| FI_RefArray (fi,_),0 ->
				gc_arr_replace_first_field (RT_refarr offs) t offs acc
			| FI_VRefArray (fi,_),0 ->
				gc_arr_replace_first_field (RT_valarr offs) t offs acc
			| FI_TParm  v,0 ->
				RT_tp(offs,(pt_get_tp_pos pt_in t)) :: acc
			| FI_Struct(s,a,fil),_ -> begin
				if pt.pt_has_tps then
					( Printf.printf "error: Struct<T> where T has type parameters isnt supported yet"; (*TODO*)
					assert false )
				else
					Printf.printf "struct: %s\n" (Tid.id t);
					let pt = get_struct_tp tctx t in
					let acc_struct,_ = (_ptypes_to_runtime_info hxc tctx t pt offs) in
					(List.rev acc_struct) @ acc
				end
			| FI_CSArr(f,n,b,a),_ ->
				Printf.printf "warning: ConstSizeArray<T> isnt supported by GC yet";
				RT_val(offs,0)  :: acc  (*TODO make array members known to the gc*)
			| FI_Val    v,_ ->
				RT_val(offs,0) :: acc
			| _,algn ->
				Printf.printf "non-val field not 8 byte aligned : %s : %s %d.. bad\n" (s_field_info fi) (Tid.id t) algn;
				acc
			)
		) (*[] t_pt_l offs_fi_l)*)
		in
		Printf.printf "GCRTI::%s SZ:%d::TPS:%s::fields:%s\n"
			(Tid.id t_in) (size) (if pt_in.pt_has_tps then "Y" else "N") (s_rti_l acc);
		acc,size





	let pt_ptypes_to_runtime_info ctx tctx pt =
		_ptypes_to_runtime_info ctx.hxc tctx pt.pt_type pt 0




	(* - - - *)


	let m_ptypes : (string,int) Hashtbl.t = Hashtbl.create 1000

    let ptypes : ptype_info DynArray.t = DynArray.create ()

    let pt_add m_ptypes ptypes key t isecon =
		if Hashtbl.mem m_ptypes key then
			let ptidx = Hashtbl.find m_ptypes key in
			(DynArray.get ptypes ptidx),false
		else
			let ptid = DynArray.length ptypes in
			Hashtbl.add m_ptypes key ptid;
			let pt = (ptinfo key t ptid) in
			DynArray.add ptypes pt;
			pt,true

    let pt_add_econ m_ptypes ptypes t ef =
    	let t = follow t in
    	let key  = (Tid.tp_normalized_id t) ^ "." ^ ef.ef_name in
    		pt_add m_ptypes ptypes key t true

	let pt_add_type m_ptypes ptypes t =
		let t = follow t in
		let key  = (Tid.tp_normalized_id t) in
			pt_add m_ptypes ptypes key t false


	(* ************************************************* *)


	(* let pt_init t =  *)



	let expressions_pass_0 ctx tctx ectx c fctx te =
		let rec loop te = match te with
			| { eexpr = TObjectDecl(nvs) }
				(* anon constructor call *) ->
				let _ = pt_ctx_add_anon tctx te.etype in ()

			(* missing : closure instatiations
			| { eexpr = TField(_,FStatic(ct,cf)); etype = TFun(_,_) } -> ()
			| { eexpr = TField(_,FClosure(ct,cf)) } -> ()
			| { eexpr = TFunction(tf) } -> ()
			*)
			| _ -> Type.iter loop te
		in
		loop te;
		te


	(* type parameters 2. *)

	let e_get_address ctx te =
		Expr.mk_static_call_2 ctx.hxc.c_lib "getAddress" [te] null_pos

	let is_same_class t1 t2 = (match follow t1,follow t2 with
		| TInst({cl_path=p1},_),TInst({cl_path=p2},_) -> p1 = p2
		| _ -> assert false
	)


	(**********************************************)
	(*********************************************

		TP collection - should be merged with the closure scope thing, proof of concept for now.

	**********************************************)
	let print_all_tps ctx tctx ectx c fctx te =
		let ft t = (match t with
			| TInst({cl_kind=KTypeParameter _},_) ->
				(Printf.printf "found TP: %s " (Tid.id t); t)
			| _ -> t) in
		let fv v = v  in
		let rec loop te = match te with | _ -> Type.map_expr_type loop ft fv te in
		loop te


	(**********************************************)
	(**********************************************)


	(**********************************************)
	(**********************************************

		specializer

	 **********************************************)

	let spec_func_name_postfix hxc tpi = match tpi with
		| TP_Tp    ->  ""
		| TP_Ref              (*TODO 64 bit specific *)
		| TP_Val64 ->  "_u64"
		| TP_Val32 ->  "_u32"
		| TP_Val16 ->  "_u16"
		| TP_Val8  ->  "_u8"
		| TP_Unknown -> assert false

	(* specialize call :
		specialize call does a few things:
		for now, we only specialize calls to @:hxcSpecialize methods of @:hxcSpecialize classes
		also
	 *)
	let has_spec_meta m = has_meta Meta.HxCSpecialize m

	let type_to_tp_info hxc t = field_info_to_tpi (get_field_info hxc t)

	let specialize_call hxc te =
		let rec loop te = match te.eexpr with
			| TCall(({ eexpr = TField(e_inst,FInstance(c,tps,cf))} as e1), el)
				when has_spec_meta cf.cf_meta && has_spec_meta c.cl_meta &&
					(match follow e1.etype with TFun _ -> true | _ ->  false ) ->
					print_endline ("matched call to " ^ cf.cf_name ^ " in " ^ (snd c.cl_path) );
					let cf_name = (match (follow e_inst.etype) with
						| TInst(_,[t]) ->
							let postfix = spec_func_name_postfix hxc (type_to_tp_info hxc t) in
							print_endline ("found " ^ cf.cf_name ^ " in " ^ (snd c.cl_path) ^ postfix);
							cf.cf_name ^ postfix
						| _ -> assert false) in
					let cf = try PMap.find cf_name c.cl_fields with Not_found ->
					print_endline ("didn't find " ^ cf_name ^ " in " ^ (snd c.cl_path));
					assert false in
					{ te with
						eexpr = TCall({ e1 with
							eexpr = TField(e_inst,FInstance(c,tps,cf));
							etype = cf.cf_type
						}, el)
					}
			| TCall(({ eexpr = TField(e_inst,FInstance(c,tps,cf))}), el) ->
				print_endline ("skipped call to " ^ cf.cf_name ^ " in " ^ (snd c.cl_path) );
				Type.map_expr loop te
			| _ -> Type.map_expr loop te
		in loop te

	let get_cl_path t = (match t with | TInst(c,_) -> c.cl_path | _ -> ([],"") )

	let specialize_function c cf cf_expr cf_name_postfix tps =
		let m = PMap.empty in
		let m = List.fold_left (fun m (t_src,t_dest) -> PMap.add (get_cl_path t_src) t_dest m) m tps in
		let ft t = let rec loop t = (match t with
				| TInst({cl_path=path;cl_kind=KTypeParameter _},_) ->
					let t_dest = ( try PMap.find path m with Not_found -> (*Printf.printf "--- NOT FOUND : %s --- " (Tid.id t);*)
					t ) in t_dest (*Printf.printf "TDEST %s -> %s %s.%s :: " (Tid.id t_dest) (Tid.id t) (String.concat "." (fst path)) (snd path);*)
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
		}

	let _specialize_class c t_dest_l =
		let spec_cf_l = List.fold_left (fun cfl cf ->
			(match c.cl_params,cf.cf_kind,cf.cf_expr with
				| [(_,t_src)],(Method _),Some(cf_expr) when has_meta Meta.HxCSpecialize cf.cf_meta ->
					(List.map ( fun (t_dest,postfix) ->
						let cf = specialize_function c cf cf_expr postfix [(t_src,t_dest)] in cf
					) t_dest_l ) @ cfl
				| _ -> cfl )) [] c.cl_ordered_fields in
		c.cl_ordered_fields <- spec_cf_l @ c.cl_ordered_fields;
		c.cl_fields <- List.fold_left ( fun cfm cf -> PMap.add cf.cf_name cf cfm ) c.cl_fields spec_cf_l;
		List.iter (fun cf ->
			(match cf.cf_expr with Some(e) -> print_endline (Tid.id e.etype) | _ -> ());
			()) c.cl_ordered_fields;
		()

	let specialize_class hxc c =
		let _ = _specialize_class c [hxc.t_uint64,"_u64";hxc.t_uint32,"_u32";hxc.t_uint16,"_u16";hxc.t_uint8,"_u8"] in
		()
	let specialize_calls hxc c =
		c.cl_ordered_fields <- List.map ( fun cf ->
			let cf = (match cf.cf_expr with
				| Some te -> let te = specialize_call hxc te in { cf with cf_expr = Some te }
				| _ -> cf) in
			c.cl_fields <- PMap.add cf.cf_name cf c.cl_fields;
		cf ) c.cl_ordered_fields;
		c.cl_ordered_statics <- List.map ( fun cf ->
			let cf = (match cf.cf_expr with
				| Some te -> let te = specialize_call hxc te in { cf with cf_expr = Some te }
				| _ -> cf) in
			c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics;
		cf ) c.cl_ordered_statics;
		()

	(**********************************************)
	(**********************************************)



	(*type scope =
		| FScope of int * (int,tvar) PMap.t * scope list
		| LScope of int * (int,tvar) PMap.t * scope list*)

	module PSet = Set.Make(
		struct
			let compare = Pervasives.compare
			type t = int
		end )

	type scope_kind =
		| SK_root
		| SK_loop of scope
		| SK_func of scope
		| SK_func_loop of scope

	and scope = {
		s_kind   : scope_kind;
		s_vars   : (int,tvar) PMap.t;
		s_used   : PSet.t;
		s_rest   : PSet.t;
		s_scopes : scope list;
	}

	let s_name_or_id m v = try ((PMap.find v m).v_name ^":"^ string_of_int v)with Not_found -> (string_of_int v)
	let s_vars m = String.concat "::" (List.map (fun v -> (v.v_name ^":"^ string_of_int v.v_id)) (pmap_to_list m))
	let s_used s = String.concat "::" (PSet.fold (fun v acc -> (s_name_or_id s.s_vars v) :: acc) s.s_used [])
	let s_rest s = String.concat "::" (PSet.fold (fun v acc -> (s_name_or_id s.s_vars v) :: acc) s.s_rest [])

	let rec s_scope ind s = match s.s_kind with
		| SK_root ->
			Printf.sprintf "\nroot: used:[%s] rest:[%s] vars:[%s] [%s]" (s_used s) (s_rest s) (s_vars s.s_vars) (s_concat s.s_scopes ind)
		| SK_loop _ ->
			Printf.sprintf "\n%*sloop: used:[%s] rest:[%s] vars:[%s] [%s]" (ind*3) "" (s_used s) (s_rest s) (s_vars s.s_vars) (s_concat s.s_scopes ind)
		| SK_func _ ->
			Printf.sprintf "\n%*sfunc: used:[%s] rest:[%s] vars:[%s] [%s]" (ind*3) "" (s_used s) (s_rest s) (s_vars s.s_vars) (s_concat s.s_scopes ind)
		| SK_func_loop _ ->
			Printf.sprintf "\n%*sfunc_loop: used:[%s] rest:[%s] vars:[%s] [%s]" (ind*3) "" (s_used s) (s_rest s) (s_vars s.s_vars) (s_concat s.s_scopes ind)

	and s_concat sl indent = String.concat "" (List.map (s_scope (indent+1)) sl)

	type scope_acc = {
		sc_vused : (int,tvar) Hashtbl.t
	}

	let init_scope k = {
		s_kind   = k;
		s_vars   = PMap.empty;
		s_used   = PSet.empty;
		s_rest   = PSet.empty;
		s_scopes = [];
	}

	let init_function_scope k tf =
		let scope = init_scope k in
		{ scope with s_vars = List.fold_left (fun m (v,_) -> PMap.add v.v_id v m) PMap.empty tf.tf_args }

	let find_vid_in_current_function s vid =
		let rec loop s = match s.s_kind with
		| SK_func_loop parent ->
			(if PMap.mem vid s.s_vars then true else loop parent)
		| SK_func _ ->
			(if PMap.mem vid s.s_vars then true else false)
		| _ -> true (* any remaining vars must be in some loop or root *)
		in loop s

	let scope_merge outer inner =
		let used,rest = PSet.fold (fun vid (used,rest) ->
			if PMap.mem vid outer.s_vars then
				PSet.add vid used,rest
			else
				used,PSet.add vid rest
		) inner.s_rest (outer.s_used,outer.s_rest)  in
		let inner = { inner with s_scopes = (List.rev inner.s_scopes) } in
		{ outer with s_used = used; s_rest = rest; s_scopes = inner :: outer.s_scopes }

	let loop_scope_kind s = match s.s_kind with
		| SK_func p
		| SK_func_loop p -> SK_func_loop s
		| SK_loop p      -> SK_loop s
		| SK_root        -> SK_loop s

	let scopes te =
		let rec loop s te = match te.eexpr with
		| TVar (v,eo) ->
			let s = Type.fold_expr_eo loop s eo in
			(*Printf.printf "svars[%s]" (s_vars s.s_vars);*)
			{ s with s_vars = PMap.add v.v_id v s.s_vars  }
		| TLocal v -> (match s.s_kind with
			| SK_func _
			| SK_func_loop _ ->
				(if not (find_vid_in_current_function s v.v_id) then
					{ s with s_rest = PSet.add v.v_id s.s_rest  }
				else s)
			| _ -> s)
		| TWhile (e1,e2,_) ->
			let inner = loop (loop (init_scope (loop_scope_kind s)) e1) e2 in
			if (s == inner) then assert false else
			scope_merge s inner
		| TFor (v,e1,e2)   ->
			let inner = loop (loop (init_scope (loop_scope_kind s)) e1) e2 in
			if (s == inner) then assert false else
			scope_merge s inner
		| TFunction tf ->
			let inner = loop (init_function_scope (SK_func s) tf) tf.tf_expr in
			if (s == inner) then assert false else
			scope_merge s inner
		| _ -> Type.fold_expr loop s te
		in
		let root_scope = (match te.eexpr with
			| TFunction tf -> loop (init_function_scope SK_root tf) tf.tf_expr
			| _ -> loop (init_scope SK_root) te)
		in
		Printf.printf "scope: %s\n." (s_scope 0 root_scope)


	(**********************************************)
	(**********************************************)

	let expressions_pass_1 ctx tctx ectx c fctx te =

		let rec loop te = match te.eexpr with
			| TVar(v,eo) ->
				(match follow v.v_type with
					| TAbstract({a_path=["c"],"ConstSizeArray"},[t;_]) ->
						(*Hack to prevent ConstSizeArray from being an lvalue-type*)
						let t = ctx.hxc.t_pointer t in
						v.v_type <- t;
						te
						(*{ te with eexpr=TVar({ v with v_type=t },eo) }*)
					| _ -> te )
			| TCall({eexpr=TField(eenum,FEnum(et,ef))} as efield, el) ->
				let _ = efield in
				(* enum constructor call *)

				(* 1. identify required TPs *)
				(try
					let pt = pt_ctx_get_econ tctx te.etype ef in
					Printf.printf "Found enum %s\n" pt.pt_key;
					(match te.etype with
						| TEnum(et,[])  -> ()
						| TEnum(et,tps) ->
							(*  we have 1+ tps, now we check whether this particular constructor requires runtime
								information about any of them:
								for every field of the constructor that is typed as a TP, we need to know
								1. the index of that TP
								2. the offset of that field (8-bytes aligned index into the object for 64 bit, 4-byte-aligned for 32 bit systems)
								3. the type corresponding to the TP at the callsite
								however at this very place we only need 1. and 3.
								(2. will be handled when we build the static part of the GC runtime info)	*)
								Printf.printf "enum tps: %s\n" (s_rti_l pt.pt_rt_info);

							Printf.printf "enum tps: %s %s\n" pt.pt_key (String.concat "-" (List.map Tid.id tps));
							(*et.e_params;*)
							()

						| _ -> ()
					);
					te
				with
					| Not_found ->
						Printf.printf "didn't find xxx enum";
						te)

				(* 2. fetch TPs from environment *)
				(* 3. build expression and insert argument into call *)

			| TObjectDecl(el) ->
				(* anon constructor call *)

				let pt = pt_ctx_get_anon tctx te.etype in
				(*  we have 1+ tps, now we check whether this anon requires runtime
					information about any of them:
					for every field of the anon that is typed as a TP, we need to know
					1. the index of that TP
					2. the offset of that field (8-bytes aligned index into the object for 64-bit & 4-byte-aligned for 32-bit systems)
					3. the type corresponding to the TP at the callsite (this is trivial in this case, since the anon is typed
					   in terms of callsite types already)
				*)
				Printf.printf "Found anon %s\n" pt.pt_key;
				te

			| TCall(({eexpr=TField(_,(FStatic(({cl_extern = false} as c),cf) | FInstance(c,_,cf)))}),el) ->
				let _ = c in
				(* function or method call *)
				(*  we have 1+ tps, we have to pass all of them:
					for every TP we need to know
					1. the index of that TP in the TP argument
					2. the type corresponding to the TP at the callsite
				*)
				te

			| TNew(c,ctps,el) ->
				(* class constructor call *)
				(*  we have 1+ tps, we have to pass all of them:
					for every TP, we need to know
					1. the index of that TP in the class headers TP info field
					2. the type corresponding to the TP at the callsite
				*)
				(try
					let pt = pt_ctx_get_type tctx te.etype in
					Printf.printf "Found class %s of sz %d\n" pt.pt_key pt.pt_size;
					te
				with
					| Not_found ->
						Printf.printf "didn't find xxx class %s\n" (Tid.id te.etype);
						te)

			| TCall(e1,el) when ClosureHandler.is_closure_expr e1 ->
				(* closure call *)
				te

			| TBinop(OpAssign,({eexpr=TField(efield,(FInstance(_,_,cf)|FStatic(_,cf)|FAnon(cf)))} as te1),te2) ->
				(* field assignment - write barrier *)
				let _ = te1 in
				te
				(* test grey flag and if not set, test mark bit,
					if set (black), push object on the grey stack *)

			| _ -> Type.map_expr loop te
		in
			(*Printf.printf "expr00\n";*)
			loop te

	let run con =
		let hxc = con.hxc in

		let mtypes           = con.com.types in
		(*
		let pt_add_existed   = pt_add_type m_ptypes ptypes in
		let pt_add t         = fst (pt_add_existed t) in
		let pt_add_econ t ef = fst (pt_add_econ m_ptypes ptypes t ef) in
		*)
		let ctx  = con in
		let tctx = init_gc_types_ctx () in
		let ectx = init_gc_gen_ctx con in

		let pt_add      = (pt_ctx_add_type tctx) in
		(*let pt_get      = (pt_ctx_get_type tctx) in*)
		let pt_add_econ = (pt_ctx_add_econ tctx) in

		let handle_expressions f pt  : unit =
			match pt.pt_type with
			| TInst(c,tps) -> gc_map_field_expressions con tctx ectx c f
			| _ -> ()
		in

		(* 	first pass over the types  *)
		let pt_pass_0 pt field_types field_names =

			(* fetch ptype_info for every field *)
			let field_ptypes  =  List.map pt_add field_types in
			pt.t_field_types  <- field_types;
			pt.pt_field_types <- field_ptypes;

			(* store names in order - important for potential optimizations reordering fields, not used currently *)
			pt.pt_field_names <- field_names;

			(* store field info - required for determining object size, alignment, value-refs and more *)
			pt.pt_field_info  <- get_info hxc field_types;

			(* detect whether we have an all-values type *)
			pt.pt_is_valref   <- not (field_info_has_refs pt.pt_field_info);

			(* get super class ptype_info *)
			pt.pt_super  <- (match pt.pt_type with
				| TInst({ cl_super =  (Some ( (c,tps) ) ) },_) ->
					   Some ( pt_add (TInst(c,tps)) )
				| _ -> None );

			(* do we have a vtable?
			   pass 0:
			   if we have a superclass, set to true here and in superclass
			   pass 1:
			   already done, right?
			*)
			pt.pt_has_vtable <- (match pt.pt_super with
				| Some pts -> pts.pt_has_vtable <- true; true
				| _ -> pt.pt_has_vtable (* avoid changing this in case it has been modified by a child class, mutable is bad :) *)
			);

			(*  do we implement any interfaces? And if, which interfaces do we implement?
				pass 0:
				we fetch interfaces for this type instance here
				pass 1:
				- do we implement any? grab interfaces using pt_interfaces and set to true in case the returned list is non-empty
				- which do we implement? done - we fetch all interfaces including those from superclasses using pt_interfaces
				  when we need them
			*)
			pt.pt_ifaces <- (match pt.pt_type with
				| TInst({cl_implements = ifaces},_ ) ->
					   List.map (fun (c,tps)-> (pt_add (TInst(c,tps)))) ifaces
				| _ -> [] );

			(* do we have type parameters ?
			   classes:
			 	if ANY class in our inheritance tree does, we do too,
				and EVERY class in the inheritance tree has to reserve space for type parameters
				pass 0:
				hence, if THIS class has type parameters, we set pt_has_tps to true
				pass 1:
				we check if any super class has type parameters, if yes, set pt_has_tps to true
				for THIS and all super classes
			   anons:
			   	decided on an individual basis, checking field info, since in haxe the language,
			   	there are no TPs in anons
			   enums:
			   	decided on an individual basis, we /could/ avoid reserving
			   	space for type parameters for constructors that don't use them

			 *)

			pt.pt_has_tps <- (match pt.pt_type with
				| TInst(c, (x :: xs) ) -> true
				| TEnum(e, (x :: xs) ) -> true
				| TAnon _              -> ( field_info_has_tps pt.pt_field_info )
				| _                    -> false
			);


			()
		in
		let pt_pass_0_init t =
			let t = follow t in (match t with
				| TInst(c,tps)     ->
					let pt           = pt_add t in
					let fields       = get_class_var_fields c in
					let field_names  = get_xs_names fields in
					let field_types  = get_xs_types fields in
					pt.pt_no_header <- has_meta Meta.HxCNoHeader c.cl_meta;
					pt_pass_0 pt field_types field_names;

					if has_meta Meta.HxCSpecialize c.cl_meta then
					specialize_class hxc c else ();

					Some pt
				| TEnum(e,tps)     ->
					let pt = pt_add t in
					let efields_l    = get_enum_fields e in
					(* since the ptypes information is about physical types, we add on ptype_info for each constructor *)
					pt.pt_constrs <- List.map ( fun ef ->
						let pt           = pt_add_econ t ef in
						let field_types  = get_enum_xs ef.ef_type in
						let field_names  = get_enum_xs_names ef.ef_type in
						pt_pass_0 pt field_types field_names;
						pt
					) (List.map (fun n ->  PMap.find n e.e_constrs) e.e_names );
					(* detect whether we have an all-values type
					   for enums this is only true if there is no ref/vref/tp type in any constructor *)
					let field_types  = List.flatten efields_l in
					pt_pass_0 pt field_types [];
					Some pt
				| TAnon(a)         ->
					let pt           = pt_add t in
					let fields       = get_anon_fields a in
					let field_names  = get_xs_names fields in
					let field_types  = get_xs_types fields in
					pt_pass_0 pt field_types field_names;
					Some pt
				| TAbstract ( { a_this = a_this } as a,_) when not (Meta.has Meta.CoreType a.a_meta) ->
					 None (* add_type a_this *)
				| TAbstract(_,_) -> None
					(* TODO FIXME WARNING !!!! assuming all @:coreTypes are value types, I think we have to do this because
					ptype_info  *)
				| _ -> None );
		in

		(*	second pass over the types *)
		let pt_pass_1 pt = (match pt.pt_type with
			| TInst (ct,tps) ->
				let super_cs = (pt_super_chain pt) in
				(* pass 1 over pt_has_tps, see pass 0 *)
				let has_tps = (pt_has_tps pt) || pt.pt_has_tps in
				List.iter (fun pts -> if has_tps then pts.pt_has_tps <- true else ()) super_cs;

				(* does THIS implement any interfaces? *)
				pt.pt_has_ifaces <- (match (pt_interfaces pt) with
					| [] -> false
					| _  -> true
				);
				Printf.printf "class type \n\t\t%s \n" (Tid.id pt.pt_type);

				let pos_by_tp = pt_pos_by_tp pt in
				pt.pt_pos_by_tp <- pos_by_tp;

				pt_init_header_field_types tctx pt;

				let rtinfo,size = pt_ptypes_to_runtime_info ctx tctx pt in
				pt.pt_rt_info <- rtinfo;
				pt.pt_size    <- size;

				specialize_calls hxc ct;

				(*if has_meta Meta.HxCSpecialize ct.cl_meta then
					specialize_class hxc ct else ();*)

			| TEnum (et,tps) ->
				List.iter (fun pt ->
					let pos_by_tp = pt_pos_by_tp pt in
					pt.pt_pos_by_tp <- pos_by_tp;
					(*let hfields = get_class_var_fields hxc.c_gc_enumheader in*)
					pt_init_header_field_types tctx pt;

					let rtinfo,size = pt_ptypes_to_runtime_info ctx tctx pt in
					(*let rtinfo = pt_ptypes_to_runtime_info hxc pt pt.pt_field_types tps in*)
					pt.pt_rt_info <- rtinfo;
					pt.pt_size    <- size;
				) pt.pt_constrs;
			| TAnon (a) ->
				Printf.printf "an ANON \n %s \n" (Tid.id pt.pt_type);
				let pos_by_tp = pt_pos_by_tp pt in
				pt.pt_pos_by_tp <- pos_by_tp;
				pt_init_header_field_types tctx pt;
				let rtinfo,size = pt_ptypes_to_runtime_info ctx tctx pt in
				pt.pt_rt_info <- rtinfo;
				pt.pt_size    <- size;
			| _ ->
				Printf.printf "assertion 4421 failed for %s %s .\n" (Tid.id pt.pt_type) (s_tkind pt.pt_type);
				assert false
			)
		in

		let pt_pass_1_prioritized_types ptypes =
			let ptypes = [
				(pt_ctx_get_type_by_string tctx "c.gc.ClassHeader");
				(pt_ctx_get_type_by_string tctx "c.gc.EnumHeader");
				(pt_ctx_get_type_by_string tctx "c.gc.AnonHeader")] @ ptypes
			in
			List.iter pt_pass_1 ptypes
		in

		let types = List.map make_instance mtypes in

		(* pass 0 *)
		let ptypes = (List.map pt_pass_0_init types) in

		(* drop None's *)
		let ptypes = List.fold_left ( fun acc opt -> (match opt with Some v -> v :: acc | _ -> acc)) [] ptypes in

		(* expressions pass 0 *)
		List.iter (handle_expressions expressions_pass_0) ptypes;

		(* prepare header fields and possibly other special stuff for pass 1 *)
		pt_pass_1_prioritized_types [];

		(* pass 1 over types *)
		List.iter pt_pass_1 ptypes;

		(* expressions pass 1 *)
		List.iter (handle_expressions expressions_pass_1) ptypes;

		List.iter (handle_expressions print_all_tps) ptypes;

		(*List.iter (handle_expressions par_map_expr) ptypes;*)

		()


	let collect_info con =

		let _ = run con in


		List.iter (type_decl_info con) con.com.types;
		List.iter (run_snd con) con.com.types;

		(* p_alignment con.hxc; *)
		()

		(*TDB.p_classes ()*)

		(*	GC module - what it is supposed to do, and how it goes about that.

		Field information

		---- pass 1 ----

		- collect all types that have to be instantiated. Due to anons and closures, this
		   has to traverse all expressions.
		-  prepend header fields to anons, classes, closures
		-  prepend enum constructor header fields to enum constructors !(postpone this TODO)!
		-  For all types, create provisional field information - are fields refs, values or typeparameters?
		-  Using that field information we group types into three groups:
		   1 reference types (types that have references)
		   2 value types     (types that only have values as fields)
		   3 tp types        (types that have type parameter fields)
		   Due to inheritance, this is not entirely trivial, there might be a base class that only has value fields
		   but derived classes have references. Since it's legal to assign a derived class instance to a field typed
		   as baseclass, we have to consider the complete inheritance tree. This value/reftype distiction gives a
		   significant performance advantage in the marking phase of the GC. Don't use inheritance.

		---- pass 2 ----

		-  Using that knowledge, in a second pass, we transform the provisional field info to the full field info,
		   classifying fields into:
		   1 refref types
		   2 refval types
		   3 reftp  types
		   4 value  types
		-  from the full field info, for each anon,class and enum constructor (physical type), we obtain the GC runtime information
		   that describes at which offset which kind of reference is located.
		-  we build a map that assigns an GC layout ID to every such unique descriptor
		-  we build a map that assigns the respective GC layout ID to each physical type
		   regardless of the actual types, we have groups of types that share a GC layout ID. Here we forego an obvious optimization:
		   reordering fields to match an existing memory layout
		   we could do that later, but have to compare the advantage in static information size (which directly translates into less
		   cache pollution at runtime, and hence is very significant ) to that of reordering fields WRT structural type information.
		-  for each GC layout ID we calculate the required size of the static information
		-  now we sort the IDs by size, and assign an index to each, which will be written to the gc_idx header field on every
		   instantiation. To be able to index into an array, we skip indices as required.

		Type Parameter Information - this deserves some explanation:

		we pass type parameter info as first argument to every function, method & constructor that needs them
		the info is encoded in a 64 bit bitfield with 3 bits per type parameter

		classes, anons and enums have a header field that stores the tp info that the GC is interested in.
		classes also store tp info for passing it on, even if the class itself doesn't have a field typed as
		a respective TP.

		when we call a function that requires tp info, there are only 3 possible sources where we can get it from:
		1. it's been passed to the current frame as tp info function argument
		2, it's stored in the class header
		3. it's statically known because the concrete type is known at the callsite
		class tp info of subclasses has to be appended to the parents class tp info TODO
		so that for a class B<Z> extends A<X,Y> the order of tp info would be: [ A_X, A_Y, B_Z]

		OK -so what do we have to do precisely?

		1. add tp_info argument to every
				- function, method that has type parameters
				- constructor function of types that have type parameters
			this involves adding the 64 bit field to the respective function and to its type

		2. add tp_info argument to every function, method & constructor call
			2.1 - lookup the call-site types we have to pass the information for
				in case we're calling a function
					this requires matching the call-site argument type and the respective function argument type,
						(e.g. if the callee arg type is { abc : T } and the caller arg type is { abc : Foo }, we know
						 that type parameter T is Foo at the call-site )
					that's done using the data structure of type search_result returned by
					get_tp_pos_info_from_callee
					which is then resolved to call-site tp info (type: caller_tpi ) by
					sr_to_ctpi
				in case we need callsite_tpi for constructor (class/enum/anon type parameters),
					we know the call-site types that match the callees type parameters, and use
					type_to_callsite_tpi to get the caller_tpi data directly.
			2.2 - adjust the caller_tpi source positions according to the following rules:
				in case the type parameters originate from the class header
					add an offset for all parent classes' caller_tpi's in the inheritance chain
				in case the type parameters originate from the tpi argument to a class constructor,
					adjust the function arg caller_tpi offsets for the class header fields
			2.3 - arrange the caller_tpi list according to the following rules:
				in case we're calling an anon or enum constructor, a function or method:
					pass the list as is (first listed type parameters first)
				in case we're calling a class constructor
					pass the caller_tpi list for the base class first,
					then, down the inheritance chain the caller_tpi for the subclasses
					then, the constructor functions type parameters
			2.4 - build the expression according to the caller_tpi list
				for function args and class header-originating caller_tpi
					(CTPI_class & CTPI_func) generate the code fetching them from those sources
				for known type parameters (CTPI_known) generate the int value, hardcoded
				for CTPI_not_found generate a special hardcoded value indicating this unfortunate fact
					should we generate a warning here? TODO

		3. store tp_info into class, anon, enum headers

		4. use tp info to dispatch functions/operations that have to be aware of the tp argument bit width
		   (AKA Array<T> methods),

		type information:


	*)




end

(* Helper *)

let rec is_value_type t =
	match t with
	| TType({t_path=[],"Null"},[t]) ->
		false
	| TMono r ->
		begin match !r with
			| Some t -> is_value_type t
			| _ -> false
		end
	| TLazy f ->
		is_value_type (!f())
	| TType (t,tl) ->
		is_value_type (apply_params t.t_params tl t.t_type)
	| TAbstract({a_path=[],"Class"},_) ->
		false
	| TAbstract({ a_impl = None }, _) ->
		true
	| TInst(c,_) ->
		has_meta Meta.Struct c.cl_meta
	| TEnum(en,_) ->
		Meta.has Meta.FlatEnum en.e_meta
	| TAbstract(a,tl) ->
		if has_meta Meta.NotNull a.a_meta then
			true
		else if Meta.has Meta.CoreType a.a_meta then
			false
		else
			is_value_type (Abstract.get_underlying_type a tl)
	| _ ->
		false

let begin_loop ctx =
	ctx.fctx.loop_stack <- None :: ctx.fctx.loop_stack;
	fun () ->
		match ctx.fctx.loop_stack with
		| ls :: l ->
			begin match ls with
				| None -> ()
				| Some s ->
					newline ctx;
					print ctx "%s: {}" s
			end;
			ctx.fctx.loop_stack <- l;
		| _ ->
			assert false

let get_native_name meta =
	try begin
		match Meta.get Meta.Native meta with
			| _,[EConst (String s),_],_ -> Some s
			| _,_,_ -> None
	end with Not_found ->
		None

let full_field_name c cf =
	if Meta.has Meta.Plain cf.cf_meta then cf.cf_name
	else match get_native_name cf.cf_meta with
		| Some n -> n
		| None -> (path_to_name c.cl_path) ^ "_" ^ cf.cf_name

let full_enum_field_name en ef = (path_to_name en.e_path) ^ "_" ^ ef.ef_name

let get_typeref_name name =
	Printf.sprintf "%s_%s" name (mk_runtime_prefix "typeref")

let monofy_class c = TInst(c,List.map (fun _ -> mk_mono()) c.cl_params)

let keywords =
	let h = Hashtbl.create 0 in
	List.iter (fun s -> Hashtbl.add h s ()) [
		"auto";"break";"case";"char";"const";"continue";" default";"do";"double";
		"else";"enum";"extern";"float";"for";"goto";"if";"int";
		"long";"register";"return";"short";"signed";"sizeof";"static";"struct";
		"switch";"typedef";"union";"unsigned";"void";"volatile";"while";
	];
	h

let escape_name n =
	if Hashtbl.mem keywords n then mk_runtime_prefix n else n


(* Type signature *)

let rec s_type ctx t =
	if is_null t then
		s_type ctx (Wrap.mk_box_type (follow t))
	else match follow t with
	| TAbstract({a_path = [],"Int"},[]) -> "int"
	| TAbstract({a_path = [],"Float"},[]) -> "double"
	| TAbstract({a_path = [],"Void"},[]) -> "void"
	| TAbstract({a_path = ["c"],"Pointer"},[t]) -> (match follow t with
		| TInst({cl_kind = KTypeParameter _},_) ->
			"char*" (* we will manipulate an array of type parameters like an array of bytes *)
		| _ -> s_type ctx t ^ "*")
	| TAbstract({a_path = ["c"],"ConstPointer"},[t]) -> "const " ^ (s_type ctx t) ^ "*"
	| TAbstract({a_path = ["c"],"FunctionPointer"},[TFun(args,ret) as t]) ->
		add_type_dependency ctx (ctx.con.hxc.t_closure t);
		Printf.sprintf "%s (*)(%s)" (s_type ctx ret) (String.concat "," (List.map (fun (_,_,t) -> s_type ctx t) args))
	| TAbstract({a_path = ["c"],"Struct"},[t]) ->
		(match t with
		| TInst (c,_) ->
			add_dependency ctx DFull c.cl_path;
			path_to_name c.cl_path
		| _ -> assert false )
	| TInst(({cl_path = ["c"],"TypeReference"} as c),_) ->
		add_class_dependency ctx c;
		"const " ^ (path_to_name c.cl_path) ^ "*"
	| TAbstract({a_path = [],"Bool"},[]) -> "int"
	| TAbstract( a, tps ) when Meta.has (Meta.Custom ":int") a.a_meta ->
		let (meta,el,epos) = Meta.get (Meta.Custom ":int") a.a_meta in
		(match el with
			| [(EConst (String s),_)] -> ( match s with
			| "int64" -> "hx_int64"
			| "int32" -> "hx_int32"
			| "int16" -> "hx_int16"
			| "int8"  -> "hx_int8"
			| "uint64" -> "hx_uint64"
			| "uint32" -> "hx_uint32"
			| "uint16" -> "hx_uint16"
			| "uint8" -> "hx_uint8"
			| _ -> s)
			| _ -> assert false;
	)
	| TInst({cl_kind = KTypeParameter _} as c,_) ->
		(* HACK HACK HACK HACK *)
		if c.cl_path = (["c";"TypeReference"],"T") then "const void*"
		else "void*"
	| TInst(c,_) ->
		let ptr = if is_value_type t then "" else "*" in
		add_class_dependency ctx c;
		(path_to_name c.cl_path) ^ ptr
	| TEnum(en,_) ->
		let ptr = if is_value_type t then "" else "*" in
		add_enum_dependency ctx en;
		(path_to_name en.e_path) ^ ptr
	| TAbstract(a,_) when Meta.has Meta.Native a.a_meta ->
		let ptr = if is_value_type t then "" else "*" in
		(path_to_name a.a_path) ^ ptr
	| TAnon a ->
		begin match !(a.a_status) with
		| Statics c -> "Class_" ^ (path_to_name c.cl_path) ^ "*"
		| EnumStatics en -> "Enum_" ^ (path_to_name en.e_path) ^ "*"
		| AbstractStatics a -> "Anon_" ^ (path_to_name a.a_path) ^ "*"
		| _ ->
			let signature = ctx.con.get_anon_signature a.a_fields in
			add_dependency ctx DFull (["c"],signature);
			"c_" ^ signature ^ "*"
		end
	| TFun(args,ret) ->
		let t = ctx.con.hxc.t_closure t in
		add_type_dependency ctx t;
		s_type ctx t
	| _ -> "void*"

let rec s_type_with_name ctx t n =
	match follow t with
	| TFun(args,ret) ->
		let t = ctx.con.hxc.t_closure t in
		add_type_dependency ctx t;
		s_type_with_name ctx t n
	| TAbstract({a_path = ["c"],"Pointer"},[t]) ->
		begin match follow t with
			| TInst({cl_kind = KTypeParameter _},_) -> "char* " ^ n (* TODO: ??? *)
			| _ -> (s_type_with_name ctx t ("*" ^ n))
		end
	| TAbstract({a_path = ["c"],"FunctionPointer"},[TFun(args,ret) as t]) ->
		add_type_dependency ctx (ctx.con.hxc.t_closure t);
		Printf.sprintf "%s (*%s)(%s)" (s_type ctx ret) (escape_name n) (String.concat "," (List.map (fun (_,_,t) -> s_type ctx t) args))
	| TAbstract({a_path = ["c"],"ConstSizeArray"},[t;const]) ->
		let size = match follow const with
			| TInst({ cl_path=[],name },_) when String.length name > 1 && String.get name 0 = 'I' ->
				String.sub name 1 (String.length name - 1)
			| _ ->
				"1"
		in
		(s_type_with_name ctx t ((escape_name n) ^ "["^ size ^"]"))
	| _ ->
		(s_type ctx t) ^ " " ^ (escape_name n)


(* Expr generation *)

let rec generate_call ctx e need_val e1 el = match e1.eexpr,el with
	| TField(_,FStatic({cl_path = ["c"],"Lib"}, cf)),(e1 :: el) ->
		begin match cf.cf_name with
		| "getAddress" ->
			spr ctx "&(";
			generate_expr ctx true e1;
			spr ctx ")"
		| "dereference" ->
			if not need_val then generate_expr ctx true e1
			else begin
				spr ctx "*(";
				generate_expr ctx true e1;
				spr ctx ")"
			end
		| "sizeof" ->
			(* get TypeReference's type *)
			let t = match follow e1.etype with
				| TInst({cl_path = ["c"],"TypeReference"},[t]) -> follow t
				| t -> t
			in
			print ctx "sizeof(%s)" (s_type ctx t);
		| "alloca" ->
			spr ctx "ALLOCA(";
			generate_expr ctx true e1;
			spr ctx ")"
		| "cCode" ->
			let rec loop e1 = match e1.eexpr with
				| TConst (TString s) -> s
				| TCast (e1, None) -> loop e1
				| TMeta (_, e1) -> loop e1
				| TCall({eexpr = TField(_,FStatic({cl_path = [],"String"},
					{cf_name = ("HX_STR"|"raw")}))},
					[{eexpr = TConst (TString s)}]) -> s
				| _ ->
				let _ = print_endline (s_expr (Type.s_type (print_context())) e ) in
				let _ = print_endline (s_expr (Type.s_type (print_context())) e1 ) in
					assert false
			in
			let code = loop e1 in
			spr ctx code;
		| "callCMacro" ->
			begin match e1.eexpr,el with
				| TConst (TString name),el ->
					let name = match name with
						| "HX_CLOSURE_CALL" ->
							add_dependency ctx DForward (["c"],"Closure");
							let i = List.length el - 1 in
							if not (PMap.mem i ctx.con.generated_closure_macros) then begin
								let args = ExtList.List.init i (fun i -> "arg" ^ (string_of_int i)) in
								let args = args in
								let s_if = "(target)->_this != NULL" in
								let s_then = Printf.sprintf "(target)->_func(%s)" (String.concat ", " ("(target)->_this" :: args)) in
								let s_else = Printf.sprintf "(target)->_func(%s)" (String.concat ", " args) in
								let s = Printf.sprintf "#define HX_CLOSURE_CALL%i(%s)\\\n\t(%s ? %s : %s)\n" i (String.concat ", " ("target" :: args)) s_if s_then s_else in
								Expr.append_global_code ctx.con.hxc s;
								ctx.con.generated_closure_macros <- PMap.add i true ctx.con.generated_closure_macros;
							end;
							Printf.sprintf "HX_CLOSURE_CALL%i" i
						| _ ->
							name
					in
					spr ctx name;
					spr ctx "(";
					concat ctx "," (generate_expr ctx true) el;
					spr ctx ")"
				| _ ->
					assert false
			end
		| _ ->
			ctx.con.com.error ("Unknown Lib function: " ^ cf.cf_name) e.epos
		end
	| TField(_,FStatic({cl_path = ["c"],"Lib"}, {cf_name="callMain"})),[] ->
		add_dependency ctx DFull (["c"],"Init");
		begin match ctx.con.com.main with
			| Some e -> generate_expr ctx false e
			| None -> ()
		end
	| TField(_,FStatic(c,({cf_name = name} as cf))),el when Meta.has Meta.Plain cf.cf_meta ->
		add_class_dependency ctx c;
		ignore(check_include_meta ctx c.cl_meta);
		print ctx "%s(" name;
		concat ctx "," (generate_expr ctx true) el;
		spr ctx ")";
	| TField(_,FStatic(c,cf)),el when Meta.has Meta.Native cf.cf_meta ->
		add_class_dependency ctx c;
		let name = match get_native_name cf.cf_meta with
			| Some s -> s
			| None -> ctx.con.com.error "String argument expected for @:native" e.epos; "_"
		in
		print ctx "%s(" name;
		concat ctx "," (generate_expr ctx true) el;
		spr ctx ")";
	| TField({eexpr = TConst TSuper} as e1, FInstance(c,_,cf)),el ->
		generate_expr ctx need_val (Expr.mk_static_call c cf (e1 :: el) e.epos)
	| TField(e1,FInstance(c,_,cf)),el when not (ClosureHandler.is_native_function_pointer cf.cf_type) ->
		add_class_dependency ctx c;
		let _ = if not (Meta.has (Meta.Custom ":overridden") cf.cf_meta) then
			spr ctx (full_field_name c cf)
		else
			let (meta,el,epos) = Meta.get (Meta.Custom ":overridden") cf.cf_meta in
			add_class_dependency ctx ctx.con.hxc.c_vtable;
			(match (meta,el,pos) with
			| (_,[EConst(Int idx),p],_) ->
				let oldbuf = ctx.buf in
				let buf = Buffer.create 0 in ctx.buf <- buf; generate_expr ctx true e1; (*TODO don't be lazy*)
				let s = Buffer.contents buf in
				let _ = ctx.buf <- oldbuf in
				let s = s ^ "->" ^ (mk_runtime_prefix "vtable") ^ "->slots["^idx^"]" in
				let ecode = Expr.mk_ccode ctx.con s null_pos in
				let t_this = match cf.cf_type with
				| TFun (ts, r) -> TFun ( ("",false,(e1.etype))  :: ts, r )
				| _ -> assert false
				in
				let cast = Expr.mk_cast ecode (ctx.con.hxc.t_func_pointer t_this) in
				generate_expr ctx true cast
			| _ -> assert false )
		in
		spr ctx "(";
		generate_expr ctx true e1;
		List.iter (fun e ->
			spr ctx ",";
			generate_expr ctx true e
		) el;
		spr ctx ")"
	| TField(_,FEnum(en,ef)),el ->
		print ctx "new_%s(" (full_enum_field_name en ef);
		concat ctx "," (generate_expr ctx true) el;
		spr ctx ")"
	| TConst (TSuper),el ->
		let csup = match follow e1.etype with
			| TInst(c,_) -> c
			| _ -> assert false
		in
		let n = (mk_runtime_prefix "initInstance") in
		let e = Expr.mk_static_call_2 csup n ((Expr.mk_local (alloc_var "this" e1.etype) e1.epos) :: el) e1.epos in
		generate_expr ctx false e
	| _ ->
		generate_expr ctx true e1;
		spr ctx "(";
		concat ctx "," (generate_expr ctx true) el;
		spr ctx ")"

and generate_constant ctx e = function
	| TString s ->
		print ctx "\"%s\"" s;
	| TInt i ->
		print ctx "%ld" i
	| TFloat s ->
		print ctx "%s" s
	| TNull ->
		spr ctx "NULL"
	| TSuper ->
		spr ctx "this"
	| TBool true ->
		spr ctx "1"
	| TBool false ->
		spr ctx "0"
	| TThis ->
		spr ctx "this"

and is_c_struct_abstract t =
	(match t with | TAbstract({a_path= ["c"],"Struct"},_) -> true | _ -> false )

and generate_expr ctx need_val e = match e.eexpr with
	| TConst c ->
		generate_constant ctx e c;
		(match e.etype with | TAbstract({a_path= ["c"],("Int64" | "UInt64") },_) -> spr ctx "L" | _ -> ())
	| TArray(e1, e2) ->
		generate_expr ctx need_val e1;
		spr ctx "[";
		generate_expr ctx true e2;
		spr ctx "]";
	| TBlock([])  ->
		(* if need_val then *)
		spr ctx "{ }"
	| TBlock el when need_val ->
		spr ctx "(";
		concat ctx "," (generate_expr ctx true) el;
		spr ctx ")"
	| TBlock(el) ->
		spr ctx "{";
		let b = open_block ctx in
		List.iter (fun e ->
			newline ctx;
			generate_expr ctx false e;
		) el;
		b();
		newline ctx;
		spr ctx "}";
	| TCall(e1,el) ->
		generate_call ctx e true e1 el
	| TTypeExpr (TClassDecl c) ->
		print ctx "&%s" (get_typeref_name (path_to_name c.cl_path));
	| TTypeExpr (TEnumDecl e) ->
		add_enum_dependency ctx e;
		spr ctx (path_to_name e.e_path);
	| TTypeExpr (TTypeDecl _ | TAbstractDecl _) ->
		(* shouldn't happen? *)
		assert false
	| TField(_,FStatic(c,cf)) ->
		add_class_dependency ctx c;
		spr ctx (full_field_name c cf)
	| TField(_,FEnum(en,ef)) when Meta.has Meta.FlatEnum en.e_meta ->
		spr ctx (full_enum_field_name en ef)
	| TField(_,FEnum(en,ef)) ->
		add_enum_dependency ctx en;
		print ctx "new_%s()" (full_enum_field_name en ef)
	| TField(e1,FDynamic "index") when (match follow e1.etype with TEnum(en,_) -> Meta.has Meta.FlatEnum en.e_meta | _ -> false) ->
		generate_expr ctx need_val e1
(* 	| TField(e1,FDynamic s) ->
		ctx.con.com.warning "dynamic" e.epos;
		generate_expr ctx true e1;
		print ctx "->%s" s; *)
	| TField(e1,fa) ->
		add_type_dependency ctx e.etype;
		add_type_dependency ctx e1.etype;
		let n = field_name fa in
		spr ctx "(";
		generate_expr ctx true e1;
		(if (is_value_type e1.etype) || (is_c_struct_abstract e1.etype) then
			print ctx ").%s" (escape_name n)
		else
			print ctx ")->%s" (escape_name n));
	| TLocal v ->
		spr ctx (escape_name v.v_name);
	| TObjectDecl fl ->
		let s = match follow e.etype with
			| TAnon an ->
				let signature = ctx.con.get_anon_signature an.a_fields in
				add_dependency ctx DFull (["c"],signature);
				signature
			| _ -> assert false
		in
		print ctx "new_c_%s(" s;
		concat ctx "," (generate_expr ctx true) (List.map (fun (_,e) -> add_type_dependency ctx e.etype; e) fl);
		spr ctx ")";
	| TNew(c,tl,el) ->
		add_class_dependency ctx c;
		spr ctx (full_field_name c (match c.cl_constructor with None -> assert false | Some cf -> cf));
		spr ctx "(";
		concat ctx "," (generate_expr ctx true) el;
		spr ctx ")";
	| TReturn None ->
		spr ctx "return"
	| TReturn (Some e1) ->
		spr ctx "return ";
		generate_expr ctx true e1;
	| TVar(v,eo) ->
		spr ctx (s_type_with_name ctx v.v_type v.v_name);
		begin match eo with
			| None -> ()
			| Some e ->
				spr ctx " = ";
				generate_expr ctx true e;
		end
	| TWhile(e1,e2,NormalWhile) ->
		spr ctx "while";
		generate_expr ctx true e1;
		let l = begin_loop ctx in
		generate_expr ctx false (mk_block e2);
		l()
	| TWhile(e1,e2,DoWhile) ->
		spr ctx "do";
		let l = begin_loop ctx in
		generate_expr ctx false (mk_block e2);
		spr ctx " while";
		generate_expr ctx true e1;
		l()
	| TContinue ->
		spr ctx "continue";
	| TMeta((Meta.MergeBlock,_,_), {eexpr=TBlock(el)}) ->
		List.iter (fun e ->
			newline ctx;
			generate_expr ctx false e;
		) el;
	| TMeta((Meta.Custom ":really",_,_), {eexpr = TBreak}) ->
		spr ctx "break";
	| TMeta((Meta.Custom ":goto",_,_), {eexpr = TConst (TInt i)}) ->
		print ctx "goto %s_%ld" (mk_runtime_prefix "label") i
	| TMeta((Meta.Custom ":label",_,_), {eexpr = TConst (TInt i)}) ->
		print ctx "%s_%ld: {}" (mk_runtime_prefix "label") i
	| TBreak ->
		let label = match ctx.fctx.loop_stack with
			| (Some s) :: _ -> s
			| None :: l ->
				let s = Printf.sprintf "%s_%i" (mk_runtime_prefix "label") ctx.con.num_labels in
				ctx.con.num_labels <- ctx.con.num_labels + 1;
				ctx.fctx.loop_stack <- (Some s) :: l;
				s
			| [] ->
				assert false
		in
		print ctx "goto %s" label;
	| TIf(e1,e2,Some e3) when need_val ->
		spr ctx "(";
		generate_expr ctx true e1;
		spr ctx " ? ";
		generate_expr ctx true e2;
		spr ctx " : ";
		generate_expr ctx true e3;
		spr ctx ")"
	| TIf(e1,e2,e3) ->
		spr ctx "if";
		generate_expr ctx true e1;
		let empty_block_or_recurse e = match e.eexpr with
			| TBlock [] -> spr ctx " { }"
			| _ -> generate_expr ctx false (mk_block e)
		in
		empty_block_or_recurse e2;
		begin match e3 with
			| None -> ()
			| Some e3 ->
				spr ctx " else ";
				empty_block_or_recurse e3;
		end
	| TSwitch(e1,cases,edef) ->
		spr ctx "switch";
		generate_expr ctx true e1;
		spr ctx "{";
		let generate_case_expr e =
			let e = if Meta.has (Meta.Custom ":patternMatching") ctx.fctx.meta then e
			else Type.concat e (Expr.add_meta (Meta.Custom ":really") (mk TBreak e.etype e.epos)) in
			generate_expr ctx false e
		in
		let b = open_block ctx in
		List.iter (fun (el,e) ->
			newline ctx;
			spr ctx "case ";
			concat ctx "," (generate_expr ctx true) el;
			spr ctx ": ";
			generate_case_expr e;
		) cases;
		begin match edef with
			| None -> ()
			| Some e ->
				newline ctx;
				spr ctx "default: ";
				generate_case_expr e;
		end;
		b();
		newline ctx;
		spr ctx "}";
	| TBinop(OpAssign,e1,e2) ->
		generate_expr ctx need_val e1;
		spr ctx " = ";
		(* TODO: I don't think that should be here... *)
		let e2 = if type_iseq e1.etype e2.etype then e2 else Expr.mk_cast e2 e1.etype in
		generate_expr ctx true e2;
	| TBinop(op,e1,e2) ->
		generate_expr ctx true e1;
		print ctx " %s " (match op with OpUShr -> ">>" | OpAssignOp OpUShr -> ">>=" | _ -> s_binop op);
		generate_expr ctx true e2;
	| TUnop(op,Prefix,e1) ->
		spr ctx (s_unop op);
		generate_expr ctx true e1;
	| TUnop(op,Postfix,e1) ->
		generate_expr ctx true e1;
		spr ctx (s_unop op);
	| TParenthesis e1 ->
		spr ctx "(";
		generate_expr ctx need_val e1;
		spr ctx ")";
	| TMeta(m,e) ->
		ctx.fctx.meta <- m :: ctx.fctx.meta;
		let e1 = generate_expr ctx need_val e in
		ctx.fctx.meta <- List.tl ctx.fctx.meta;
		e1
	| TCast(e1,_) when not need_val ->
		generate_expr ctx need_val e1
	| TCast(e1,_) ->
		begin match follow e1.etype with
		| TInst(c,_) when Meta.has Meta.Struct c.cl_meta -> generate_expr ctx true e1;
		| TAbstract({a_path = ["c"],"Pointer"},[t]) when ((s_type ctx e.etype) = "int") -> generate_expr ctx true e1;
		| _ ->
			print ctx "((%s) (" (s_type ctx e.etype);
			generate_expr ctx true e1;
			spr ctx "))"
		end
	| TEnumParameter (e1,ef,i) ->
		generate_expr ctx true e1;
		begin match follow e1.etype with
			| TEnum(en,_) ->
				add_enum_dependency ctx en;
				(*let i = if i > 0 then i - 1 else i in (*REMOVE TODO*)*)
				let s,_,_ = match ef.ef_type with TFun(args,_) -> List.nth args i | _ -> assert false in

				print ctx "->args.%s.%s" ef.ef_name s;
				(*print ctx "->args.%s.%s" ef.ef_name "onk";*)
			| _ ->
				assert false
		end
	| TArrayDecl _ | TTry _ | TFor _ | TThrow _ | TFunction _ ->
		(* removed by filters *)
		assert false


(* Type generation *)

let generate_function_header ctx c cf stat =
	let tf = match cf.cf_expr with
		| Some ({eexpr = TFunction tf}) -> tf
		| None ->
			assert false
		| Some e ->
			print_endline ((s_type_path c.cl_path) ^ "." ^ cf.cf_name ^ ": " ^ (s_expr_pretty "" (Type.s_type (print_context())) e));
			assert false
	in
	(match c.cl_path with
		| (_,"Spec") ->
			print_endline ((s_type_path c.cl_path) ^ "." ^ cf.cf_name ^ ": " ^ (s_expr (Type.s_type (print_context())) tf.tf_expr))
		| _ -> 	());
	let sargs = List.map (fun (v,_) -> s_type_with_name ctx v.v_type v.v_name) tf.tf_args in
	let sargs = if stat then sargs else (s_type_with_name ctx (monofy_class c) "this") :: sargs in
	print ctx "%s(%s)" (s_type_with_name ctx tf.tf_type (full_field_name c cf)) (String.concat "," sargs)

let generate_typeref_forward ctx path =
	print ctx "extern const c_TypeReference %s" (get_typeref_name (path_to_name path))

let generate_typeref_declaration ctx mt =
	let path = t_path mt in
	let name = path_to_name path in
	let ctor,alloc,super = match mt with
		| TClassDecl c ->
			let s_alloc = try
				full_field_name c (PMap.find (mk_runtime_prefix "alloc") c.cl_statics)
			with Not_found ->
				"NULL"
			in
			let s_ctor = match c.cl_constructor with
				| Some cf -> full_field_name c cf
				| None -> "NULL"
			in
			let s_super = match c.cl_super with
				| None -> "NULL"
				| Some (csup,_) ->
					add_class_dependency ctx csup;
					"&" ^ (get_typeref_name (path_to_name csup.cl_path))
			in
			s_ctor,s_alloc,s_super
		| _ ->
			"NULL","NULL","NULL"
	in
	print ctx "const c_TypeReference %s = {\n" (get_typeref_name name);
	print ctx "\t\"%s\",\n" (s_type_path path);
	spr ctx "\tNULL,\n";
	print ctx "\tsizeof(%s),\n" name;
	print ctx "\t%s,\n" ctor;
	print ctx "\t%s,\n" alloc;
	print ctx "\t%s\n" super;
	spr ctx "};\n"
(* 	let path = Expr.t_path t in
	if is_value_type t then
		print ctx "const %s %s__default = { 0 }; //default" (s_type ctx t) (path_to_name path)
	else
		print ctx "const void* %s__default = NULL; //default" (path_to_name path);
	newline ctx;
	let nullval = Printf.sprintf "&%s__default" (path_to_name path) in
	Printf.sprintf "const typeref %s__typeref = { \"%s\", %s, sizeof(%s) }; //typeref declaration" (path_to_name path) (s_type_path path) nullval (s_type ctx t) *)

let generate_method ctx c cf stat =
	let e = match cf.cf_expr with
		| None -> None
		| Some {eexpr = TFunction tf} -> Some tf.tf_expr
		| Some e -> Some e
	in
	ctx.fctx <- {
		field = cf;
		loop_stack = [];
		meta = [];
	};
	let rec loop e = match e.eexpr with
		| TBlock [{eexpr = TBlock _} as e1] ->
			loop e1
		| _ ->
			Type.map_expr loop e
	in
	generate_function_header ctx c cf stat;
	begin match e with
		| None -> ()
		| Some e -> match loop e with
			| {eexpr = TBlock [] } -> spr ctx "{ }"
			| e -> generate_expr ctx false e
	end;
	newline ctx;
	spr ctx "\n"

let generate_header_fields ctx =
	let v = Var {v_read=AccNormal;v_write=AccNormal} in
	let cf_vt = Expr.mk_class_field (mk_runtime_prefix "vtable" )
		(TInst(ctx.con.hxc.c_vtable,[])) false null_pos v [] in
	let cf_hd = Expr.mk_class_field (mk_runtime_prefix "header" )
		(ctx.con.hxc.t_int64) false null_pos v [] in
	[cf_vt;cf_hd]

let generate_class ctx c =
	let vars = DynArray.create () in
	let svars = DynArray.create () in
	let methods = DynArray.create () in

	(* split fields into member vars, static vars and functions *)
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ -> ()
		| Method m ->  DynArray.add methods (cf,false)
	) c.cl_ordered_fields;
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ -> DynArray.add svars cf
		| Method _ -> DynArray.add methods (cf,true)
	) c.cl_ordered_statics;

	let rec loop c =
		begin match c.cl_super with
		| None -> ()
		| Some (csup,_) -> loop csup
		end;
		List.iter (fun cf -> match cf.cf_kind with
			| Var _ ->
				if cf.cf_name <> (mk_runtime_prefix "header") && cf.cf_name <> (mk_runtime_prefix "vtable") then DynArray.add vars cf
			| Method m ->  ()
		) c.cl_ordered_fields;
	in
	loop c;

	let path = path_to_name c.cl_path in

	if not (Meta.has (Meta.Custom ":noVTable") c.cl_meta) then
		List.iter(fun v ->
			DynArray.insert vars 0 v;
			c.cl_fields <- PMap.add v.cf_name v c.cl_fields;
		) (generate_header_fields ctx);

	(* add constructor as function *)
	begin match c.cl_constructor with
		| None -> ()
		| Some cf -> DynArray.add methods (cf,true);
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

	ctx.buf <- ctx.buf_c;

	generate_typeref_declaration ctx (TClassDecl c);

	(* generate static vars *)
	if not (DynArray.empty svars) then begin
		spr ctx "\n// static vars\n";
		DynArray.iter (fun cf ->
			spr ctx (s_type_with_name ctx cf.cf_type (full_field_name c cf));
			newline ctx;
		) svars;
	end;

	spr ctx "\n";

	(* generate function implementations *)
	if not (DynArray.empty methods) then begin
		DynArray.iter (fun (cf,stat) ->
			generate_method ctx c cf stat;
		) methods;
	end;

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
		spr ctx "\n// member var structure\n";
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
	end else begin
		print ctx "typedef struct %s { void* dummy; } %s" path path;
		newline ctx;
	end;

	(* generate static vars *)
	if not (DynArray.empty svars) then begin
		spr ctx "\n// static vars\n";
		DynArray.iter (fun cf ->
		spr ctx (s_type_with_name ctx cf.cf_type (full_field_name c cf));
		newline ctx
	) svars
	end;

	(* generate forward declarations of functions *)
	if not (DynArray.empty methods) then begin
		spr ctx "\n// forward declarations\n";
		DynArray.iter (fun (cf,stat) ->
			generate_function_header ctx c cf stat;
			newline ctx;
		) methods;
	end;

	add_dependency ctx DForward (["c"],"TypeReference");
	generate_typeref_forward ctx c.cl_path;
	newline ctx

let generate_flat_enum ctx en =
	ctx.buf <- ctx.buf_h;
	let ctors = List.map (fun s -> PMap.find s en.e_constrs) en.e_names in
	let path = path_to_name en.e_path in
	print ctx "typedef enum %s {\n\t" path;
	let f ef = spr ctx (full_enum_field_name en ef) in
	concat ctx ",\n\t" f ctors;
	print ctx "\n} %s;" path

let generate_enum ctx en =
	ctx.buf <- ctx.buf_h;
(* 	add_dependency ctx DForward ([],"typeref");
	spr ctx (generate_typeref_forward ctx en.e_path); *)
	(* newline ctx; *)

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
	print ctx "int %s" (mk_runtime_prefix "header");
	newline ctx;
	spr ctx "unsigned int index";
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
	(* spr ctx (generate_typedef_declaration ctx (TEnum(en,List.map snd en.e_params))); *)
	(* newline ctx; *)

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

let generate_type con mt = match mt with
	| TClassDecl {cl_kind = KAbstractImpl a} when Meta.has Meta.MultiType a.a_meta ->
		()
	| TClassDecl c when not c.cl_extern && not c.cl_interface ->
		let ctx = mk_type_context con c.cl_path  in
		generate_class ctx c;
		close_type_context ctx;
	| TEnumDecl en when not en.e_extern ->
		let ctx = mk_type_context con en.e_path  in
		if Meta.has Meta.FlatEnum en.e_meta then
			generate_flat_enum ctx en
		else
			generate_enum ctx en;
		close_type_context ctx;
	| TAbstractDecl { a_path = [],"Void" } -> ()
	| TAbstractDecl ({ a_path = ["c"],"Struct" } as a )
	| TAbstractDecl a when Meta.has Meta.CoreType a.a_meta ->
		let ctx = mk_type_context con a.a_path in
		ctx.buf <- ctx.buf_c;
		spr ctx " "; (* write something so the .c file is generated *)
		close_type_context ctx
	| _ ->
		()

let generate_anon con name fields =
	let ctx = mk_type_context con (["c"],name) in
	let name = "c_" ^ name in
	begin match fields with
	| [] ->
		print ctx "typedef int %s" name;
		newline ctx
	| fields ->
		spr ctx "// forward declaration";
		newline ctx;
		print ctx "typedef struct %s %s" name name;
		newline ctx;

		spr ctx "// structure";

		newline ctx;
		print ctx "typedef struct %s {" name;
		let b = open_block ctx in
		List.iter (fun cf ->
			newline ctx;
			spr ctx (s_type_with_name ctx cf.cf_type cf.cf_name);
		) fields;
		b();
		newline ctx;
		print ctx "} %s" name;
		newline ctx;
	end;

	spr ctx "// constructor forward declaration";
	newline ctx;
	print ctx "%s* new_%s(%s)" name name (String.concat "," (List.map (fun cf -> s_type_with_name ctx cf.cf_type cf.cf_name) fields));
	newline ctx;

	ctx.buf <- ctx.buf_c;

	spr ctx "// constructor definition";
	newline ctx;
	print ctx "%s* new_%s(%s) {" name name (String.concat "," (List.map (fun cf -> s_type_with_name ctx cf.cf_type cf.cf_name) fields));
	let b = open_block ctx in
	newline ctx;
	print ctx "%s* %s = (%s*) malloc(sizeof(%s))" name (mk_runtime_prefix "this") name name;
	List.iter (fun cf ->
		newline ctx;
		print ctx "%s->%s = %s" (mk_runtime_prefix "this") cf.cf_name cf.cf_name;
	) fields;
	newline ctx;
	print ctx "return %s" (mk_runtime_prefix "this");
	b();
	newline ctx;
	spr ctx "}";
	close_type_context ctx

let generate_init_file con =
	let ctx = mk_type_context con (["c"],"Init") in
	ctx.buf <- ctx.buf_c;
	spr ctx "void _hx_init() {";
	let b = open_block ctx in
	List.iter (fun path ->
		add_dependency ctx DForward path;
		newline ctx;
		print ctx "%s__hx_init()" (path_to_name path);
	) con.init_modules;
	b();
	newline ctx;
	spr ctx "}";
	ctx.buf <- ctx.buf_h;
	spr ctx "void _hx_init();";
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
	output_string ch ("\tLDFLAGS += -lm \n");
	List.iter (fun lib ->
		output_string ch ("\tLDFLAGS += -l" ^ lib ^ " \n")
	) con.com.c_libs;
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
	List.iter (fun ctx ->
		if Buffer.length ctx.buf_c > 0 then
			output_string ch (Printf.sprintf "%s.$(OBJEXT) " (relpath ctx.type_path))
	) con.generated_types;
	output_string ch "\n\n$(OUT): $(OBJECTS)";
	output_string ch "\n\t$(CC) $(CFLAGS) $(INCLUDES) $(OBJECTS) -o $(OUT) $(LDFLAGS)\n";
	output_string ch "\n\nclean:\n\t$(RM) $(OUT) $(OBJECTS)";
	close_out ch


(* Init & main *)

let initialize_class con c =
	let add_init e = match c.cl_init with
		| None -> c.cl_init <- Some e
		| Some e2 -> c.cl_init <- Some (Type.concat e2 e)
	in
	let add_member_init e = match c.cl_constructor with
		| Some ({cf_expr = Some ({eexpr = TFunction tf} as ef)} as cf) ->
			cf.cf_expr <- Some ({ef with eexpr = TFunction {tf with tf_expr = Type.concat tf.tf_expr e}})
		| _ ->
			failwith "uhm..."
	in
	let check_dynamic cf stat = match cf.cf_kind with
		| Method MethDynamic ->
			(* create implementation field *)
			let p = cf.cf_pos in
			let cf2 = {cf with cf_name = mk_runtime_prefix cf.cf_name; cf_kind = Method MethNormal } in
			if stat then begin
				c.cl_ordered_statics <- cf2 :: c.cl_ordered_statics;
				c.cl_statics <- PMap.add cf2.cf_name cf2 c.cl_statics;
				let ef1 = Expr.mk_static_field c cf p in
				let ef2 = Expr.mk_static_field c cf2 p in
				let ef2 = Wrap.wrap_static_function con.hxc ef2 in
				add_init (Codegen.binop OpAssign ef1 ef2 ef1.etype p);
			end else begin
				let ethis = mk (TConst TThis) (monofy_class c) p in
				let ef1 = mk (TField(ethis,FInstance(c,List.map snd c.cl_params,cf))) cf.cf_type p in
				let ef2 = mk (TField(ethis,FStatic(c,cf2))) cf2.cf_type p in
				let ef2 = Wrap.wrap_function con.hxc ethis ef2 in
				add_member_init (Codegen.binop OpAssign ef1 ef2 ef1.etype p);
				c.cl_ordered_fields <- cf2 :: c.cl_ordered_fields;
				c.cl_fields <- PMap.add cf2.cf_name cf2 c.cl_fields
			end;
			cf.cf_expr <- None;
			cf.cf_kind <- Var {v_read = AccNormal; v_write = AccNormal};
			cf.cf_type <- con.hxc.t_closure cf.cf_type;
		| _ ->
			()
	in

	let check_closure cf = match cf.cf_type with
		| TFun _ -> cf.cf_type <- con.hxc.t_closure cf.cf_type;
		| _ -> ()
	in

	let infer_null_argument cf =
		try
		match cf.cf_expr,follow cf.cf_type with
			| Some ({eexpr = TFunction tf} as e),TFun(args,tr) ->
				let args = List.map2 (fun (v,co) (n,o,t) ->
					let t = if not o && co = None then
						t
					else if is_null v.v_type then
						v.v_type
					else begin
						v.v_type <- con.com.basic.tnull v.v_type;
						v.v_type
					end in
					n,o,t
				) tf.tf_args args in
				cf.cf_type <- TFun(args,tr);
				cf.cf_expr <- Some ({e with etype = cf.cf_type})
			| _ ->
				()
		with _ -> Printf.printf "null args: %s FAILED\n" cf.cf_name;
	in

	List.iter (fun cf ->
		match cf.cf_kind with
		| Var _ -> check_closure cf
		| Method m -> match follow cf.cf_type with
			| TFun(_) ->
				infer_null_argument cf;
				check_dynamic cf false;
			| _ ->
				Printf.printf "Invalid type %s While handling %s.%s\n" (Type.s_type (print_context()) cf.cf_type) (s_type_path c.cl_path) cf.cf_name;
				assert false;
	) c.cl_ordered_fields;

	List.iter (fun cf ->
		match cf.cf_kind with
		| Var _ ->
			check_closure cf;
			begin match cf.cf_expr with
				| None -> ()
				| Some e ->
					(* add static var initialization to cl_init *)
					let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
					let ethis = mk (TTypeExpr (TClassDecl c)) ta cf.cf_pos in
					let efield = Codegen.field ethis cf.cf_name cf.cf_type cf.cf_pos in
					let eassign = mk (TBinop(OpAssign,efield,e)) efield.etype cf.cf_pos in
					cf.cf_expr <- Some eassign;
					add_init eassign;
			end
		| Method _ ->
			infer_null_argument cf;
			check_dynamic cf true;
	) c.cl_ordered_statics;

	begin match c.cl_constructor with
		| Some cf -> infer_null_argument cf
		| _ -> ()
	end;

	if not (Meta.has (Meta.Custom ":noVTable") c.cl_meta) then begin
		let v = Var {v_read=AccNormal;v_write=AccNormal} in
		let cf_vt = Expr.mk_class_field (mk_runtime_prefix "vtable") (TInst(con.hxc.c_vtable,[])) false null_pos v [] in
		let cf_hd = Expr.mk_class_field (mk_runtime_prefix "header") (con.hxc.t_int64) false null_pos v [] in
		c.cl_ordered_fields <- cf_vt :: cf_hd :: c.cl_ordered_fields;
		c.cl_fields <- PMap.add cf_vt.cf_name cf_vt (PMap.add cf_hd.cf_name cf_hd c.cl_fields);
	end;

	let e_typeref = Expr.mk_ccode con ("&" ^ (get_typeref_name (path_to_name c.cl_path))) c.cl_pos in
	let e_init = Expr.mk_static_call_2 con.hxc.c_boot "registerType" [e_typeref] c.cl_pos in
	add_init e_init

let initialize_constructor con c cf =
	match cf.cf_expr with
	| Some ({eexpr = TFunction tf} as e) ->
		let p = e.epos in
		let t_class = monofy_class c in
		let e_alloc = if is_value_type t_class then
			Expr.mk_ccode con ("{0}; //semicolon") p
		else
			let e_size = Expr.mk_ccode con (Printf.sprintf "sizeof(%s)" (path_to_name c.cl_path)) p in
			Expr.mk_static_call_2 con.hxc.c_cstdlib "calloc" [Expr.mk_int con.com 1 p;e_size] p
		in
		let v_this = alloc_var "this" t_class in
		let e_this = Expr.mk_local v_this p in
		let el_vt = try
			let cf_vt = PMap.find (mk_runtime_prefix "vtable") c.cl_fields in
			let e_vt = mk (TField(e_this,FInstance(c,List.map snd c.cl_params,cf_vt))) cf_vt.cf_type null_pos in
			let easgn = Expr.mk_binop OpAssign e_vt (Expr.mk_static_field_2 c (mk_runtime_prefix "_vtable") null_pos ) cf_vt.cf_type null_pos in
			[easgn]
		with Not_found ->
			[]
		in
		let args = List.map (fun (v,_) -> v.v_name,false,v.v_type) tf.tf_args in
		let mk_ctor_init () =
			let cf_init = Expr.mk_class_field (mk_runtime_prefix "initInstance") (TFun((v_this.v_name,false,v_this.v_type) :: args,con.com.basic.tvoid)) false p (Method MethNormal) [] in
			let rec map_this e = match e.eexpr with
				| TConst TThis -> e_this
				| _ -> Type.map_expr map_this e
			in
			let tf_ctor = {
				tf_args = (v_this,None) :: List.map (fun (v,_) -> v,None) tf.tf_args;
				tf_type = con.com.basic.tvoid;
				tf_expr = map_this tf.tf_expr;
			} in
			cf_init.cf_expr <- Some (mk (TFunction tf_ctor) cf_init.cf_type p);
			c.cl_ordered_statics <- cf_init :: c.cl_ordered_statics;
			c.cl_statics <- PMap.add cf_init.cf_name cf_init c.cl_statics;
			let ctor_args = List.map (fun (v,_) -> Expr.mk_local v p) tf.tf_args in
			Expr.mk_static_call c cf_init (e_this :: ctor_args) p
		in
		let e_vars = mk (TVar (v_this,Some e_alloc)) con.com.basic.tvoid p in
		let e_return = mk (TReturn (Some e_this)) t_dynamic p in
		let e_init = if is_value_type t_class then
			tf.tf_expr
		else
			mk_ctor_init ()
		in
		let tf_alloc = {
			tf_args = [];
			tf_type = t_class;
			tf_expr = Expr.mk_block con.com p (e_vars :: el_vt @ [e_return]);
		} in
		let cf_alloc = Expr.mk_class_field (mk_runtime_prefix "alloc") (tfun [] t_class) false p (Method MethNormal) [] in
		cf_alloc.cf_expr <- Some (mk (TFunction tf_alloc) cf_alloc.cf_type cf_alloc.cf_pos);
		c.cl_ordered_statics <- cf_alloc :: c.cl_ordered_statics;
		c.cl_statics <- PMap.add cf_alloc.cf_name cf_alloc c.cl_statics;
		let tf = {
			tf_args = tf.tf_args;
			tf_type = t_class;
			tf_expr = mk (TBlock [
				mk (TVar (v_this,Some (Expr.mk_static_call c cf_alloc [] p))) con.com.basic.tvoid p;
				e_init;
				e_return
			]) t_class p;
		} in
		cf.cf_expr <- Some {e with eexpr = TFunction tf};
		cf.cf_type <- TFun(args, t_class)
	| _ ->
		()

let generate_types con types =

	List.iter (fun mt -> match mt with
		| TClassDecl c -> initialize_class con c
		| _ -> ()
	) types;
	VTableHandler.get_chains con types;
	List.iter (fun mt -> match mt with
		| TClassDecl ({cl_constructor = Some cf} as c) -> initialize_constructor con c cf
		| _ -> ()
	) types;
	(* ascending priority *)
	let filters = [
		DefaultValues.filter;
		ExprTransformation2.filter
	] in

	let gen = Filters.mk_gen_context con in
	List.iter (Filters.add_filter gen) filters;
	Filters.run_filters_types gen types;
	let filters = [
		VarDeclarations.filter;
		ExprTransformation.filter;
		ArrayHandler.filter;
		TypeChecker.filter;
		StringHandler.filter;
		SwitchHandler.filter;
		ClosureHandler.filter;
		DefaultValues.handle_call_site;
	] in

	let gen = Filters.mk_gen_context con in
	List.iter (Filters.add_filter gen) filters;
	Filters.run_filters_types gen types


let generate com =
	let rec find_class path mtl = match mtl with
		| TClassDecl c :: _ when c.cl_path = path -> c
		| _ :: mtl -> find_class path mtl
		| [] -> assert false
	in
	let c_lib = find_class (["c"],"Lib") com.types in
	let null_func _ = assert false in

	let arch64 = {
		ta_refsize = 8;
		ta_tpsize  = 8;
		ta_fpsize  = 8;
	} in

	let arch32 = {
		ta_refsize = 4;
		ta_tpsize  = 8;
		ta_fpsize  = 4;
	} in
	let _ = arch32 in

	let hxc = List.fold_left (fun acc mt -> match mt with
		| TClassDecl c ->
			begin match c.cl_path with
				| [],"jmp_buf" -> {acc with t_jmp_buf = TInst(c,[])}
				| [],"hxc" -> {acc with c_boot = c}
				| [],"String" -> {acc with c_string = c}
				| [],"Array" -> {acc with c_array = c}
				| ["c"],"TypeReference" -> {acc with t_typeref = fun t -> TInst(c,[t])}
				| ["c"],"FixedArray" -> {acc with c_fixed_array = c}
				| ["c"],"Exception" -> {acc with c_exception = c}
				| ["c"],"Closure" -> {acc with t_closure = fun t -> TInst(c,[t])}
				| ["c"],"CString" -> {acc with c_cstring = c}
				| ["c"],"CStdlib" -> {acc with c_cstdlib = c}
				| ["c"],"CSetjmp" -> {acc with c_csetjmp = c}
				| ["c"],"CStdio" -> {acc with c_cstdio = c}
				| ["c"],"VTable" -> {acc with c_vtable = c}
				| ["c";"gc"],"GC" -> {acc with c_gc = c}
				| ["c";"gc"],"Alloc" -> {acc with c_gc_alloc = c}
				| ["c";"gc"],"Pool" -> {acc with c_gc_pool = c}
				| ["c";"gc"],"Templates" -> {acc with c_gc_templates = c}
				| ["c";"gc"],"ClassHeader" -> {acc with c_gc_classheader = c}
				| ["c";"gc"],"EnumHeader" -> {acc with c_gc_enumheader = c}
				| ["c";"gc"],"AnonHeader" -> {acc with c_gc_anonheader = c}
				| p ->
					acc
			end
		| TAbstractDecl a ->
			begin match a.a_path with
			| ["c"],"ConstSizeArray" ->
				acc
			| ["c"],"Pointer" ->
				{acc with t_pointer = fun t -> TAbstract(a,[t])}
			| ["c"],"ConstPointer" ->
				{acc with t_const_pointer = fun t -> TAbstract(a,[t])}
			| ["c"],"FunctionPointer" ->
				{acc with t_func_pointer = fun t -> TAbstract(a,[t])}
			| ["c"],"Int64" ->
				{acc with t_int64 = TAbstract(a,[])}
			| ["c"],"UInt64" ->
				{acc with t_uint64 = TAbstract(a,[])}
			| ["c"],"UInt32" ->
				{acc with t_uint32 = TAbstract(a,[])}
			| [],"hx_uint32" ->
				{acc with t_uint32 = TAbstract(a,[])}
			| [],"hx_uint16" ->
				{acc with t_uint16 = TAbstract(a,[])}
			| [],"hx_uint8" ->
				{acc with t_uint8 = TAbstract(a,[])}
			| ["c"],"VarArg" ->
				{acc with t_vararg = TAbstract(a,[])}
			| p ->
				acc
			end
		| _ ->
			acc
	) {
		c_lib = c_lib;
		cf_deref = PMap.find "dereference" c_lib.cl_statics;
		cf_addressof = PMap.find "getAddress" c_lib.cl_statics;
		cf_sizeof = PMap.find "sizeof" c_lib.cl_statics;
		t_typeref = null_func;
		t_pointer = null_func;
		t_closure = null_func;
		t_const_pointer = null_func;
		t_func_pointer = null_func;
		t_int64 = t_dynamic;
		t_uint64 = t_dynamic;
		t_uint32 = t_dynamic;
		t_uint16 = t_dynamic;
		t_uint8 = t_dynamic;
		t_jmp_buf = t_dynamic;
		t_vararg = t_dynamic;
		c_boot = null_class;
		c_exception = null_class;
		c_string = null_class;
		c_array = null_class;
		c_fixed_array = null_class;
		c_cstring = null_class;
		c_csetjmp = null_class;
		c_cstdlib = null_class;
		c_cstdio = null_class;
		c_vtable = null_class;
		c_gc     = null_class;
		c_gc_alloc = null_class;
		c_gc_pool  = null_class;
		c_gc_classheader = null_class;
		c_gc_enumheader = null_class;
		c_gc_anonheader = null_class;
		c_gc_templates = null_class;

		gc_types = GC.init_gc_types_ctx ();
		arch = arch64;

	} com.types in
	let anons = ref PMap.empty in
	let added_anons = ref PMap.empty in
	let get_anon =
		let num_anons = ref 0 in
		fun fields ->
			let fields = pmap_to_list fields in
			let fields = sort_anon_fields fields in
			let id = String.concat "," (List.map (fun cf -> cf.cf_name ^ (Type.s_type (print_context()) (follow cf.cf_type))) fields) in
			let s = try
				fst (PMap.find id !anons)
			with Not_found ->
				incr num_anons;
				let s = mk_runtime_prefix  ("anon_" ^ (string_of_int !num_anons)) in
				anons := PMap.add id (s,fields) !anons;
				added_anons := PMap.add id (s,fields) !added_anons;
				s
			in
			s
	in
	let con = {
		com = com;
		hxc = hxc;
		num_temp_funcs = 0;
		num_labels = 0;
		(* this has to start at 0 so the first type id is 1 *)
		num_identified_types = 0;
		type_ids = PMap.empty;
		type_parameters = PMap.empty;
		init_modules = [];
		generated_types = [];
		get_anon_signature = get_anon;
		generated_closure_macros = PMap.empty;
	} in

	let types1,types2 = List.partition (fun mt -> match mt with
		| TClassDecl {cl_path = [],"hxc"} -> false
		| _ -> true
	) com.types in

	generate_types con types1;

	generate_types con types2;
	GC.collect_info con;
	List.iter (generate_type con) types1;
	List.iter (generate_type con) types2;


	let rec loop () =
		let anons = !added_anons in
		added_anons := PMap.empty;
		PMap.iter (fun _ (s,cfl) -> generate_anon con s cfl) anons;
		if not (PMap.is_empty !added_anons) then loop()
	in
	loop();
	generate_init_file con;
	generate_make_file con
