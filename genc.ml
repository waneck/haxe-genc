open Ast
open Common
open Type

type context = {
	com : Common.context;
	cvar : tvar;
	mutable num_temp_funcs : int;
	mutable num_labels : int;
	mutable num_anon_types : int;
	mutable num_identified_types : int;
	mutable anon_types : (string,string * tclass_field list) PMap.t;
	mutable type_ids : (string,int) PMap.t;
	mutable type_parameters : (path * texpr) list;
	mutable ttype : t -> t;
}

type function_context = {
	field : tclass_field;
	expr : texpr option;
	mutable local_vars : tvar list;
	mutable loop_stack : string option list;
}

type type_context = {
	con : context;
	file_path_no_ext : string;
	buf_c : Buffer.t;
	buf_h : Buffer.t;
	mutable buf : Buffer.t;
	mutable tabs : string;
	mutable curpath : path;
	mutable fctx : function_context;
	mutable dependencies : (path,bool) PMap.t;
}

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

(** helpers **)
let rec is_value_type ctx t = match follow t with
	| TAbstract({ a_impl = None }, _) -> true
	(* | TInst(c,_) -> has_meta Meta.Struct c.cl_meta *)
	| TEnum(_,_) -> false (* TODO: define when a TEnum will be stack-allocated and when it won't *)
	| TAbstract(a,tl) ->
		if has_meta Meta.NotNull a.a_meta then
			true
		else
			is_value_type ctx (Codegen.Abstract.get_underlying_type a tl)
	| _ -> false

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
		curpath = path;
		fctx = {
			local_vars = [];
			field = null_field;
			expr = None;
			loop_stack = [];
		};
		dependencies = PMap.empty;
	}

let path_to_name (pack,name) = match pack with [] -> name | _ -> String.concat "_" pack ^ "_" ^ name
let path_to_header_path (pack,name) = match pack with [] -> name ^ ".h" | _ -> String.concat "/" pack ^ "/" ^ name ^ ".h"

let close_type_context ctx =
	let n = "_h" ^ path_to_name ctx.curpath in
	let ch_h = open_out_bin (ctx.file_path_no_ext ^ ".h") in
	print_endline ("Writing to " ^ (ctx.file_path_no_ext ^ ".h"));
	output_string ch_h ("#ifndef " ^ n ^ "\n");
	output_string ch_h ("#define " ^ n ^ "\n");
	output_string ch_h "#define GC_NOT_DLL\n";
	output_string ch_h "#include \"gc.h\"\n";
	(* TODO: get rid of these *)
	output_string ch_h "#include <glib.h>\n";
	output_string ch_h "#include <setjmp.h>\n";
	output_string ch_h "#include <stdio.h>\n";
	output_string ch_h "#include <string.h>\n";
	let pabs = get_full_path ctx.con.com.file in
	PMap.iter (fun path b ->
		let name = path_to_name path in
		if b then begin
			if path = (["hxc"],"AnonTypes") || path = (["c";"hxc"],"Exception") then output_string ch_h ("#include \"" ^ pabs ^ "/" ^ (path_to_header_path path) ^ "\"\n")
			else output_string ch_h (Printf.sprintf "typedef struct %s %s;\n"  name name)
		end else output_string ch_h (Printf.sprintf "#include <%s>\n" (path_to_header_path path))
	) ctx.dependencies;
	output_string ch_h (Buffer.contents ctx.buf_h);
	output_string ch_h "\n#endif";
	close_out ch_h;

	let sc = Buffer.contents ctx.buf_c in
	if String.length sc > 0 then begin
		let ch_c = open_out_bin (ctx.file_path_no_ext ^ ".c") in
		output_string ch_c ("#include \"" ^ (snd ctx.curpath) ^ ".h\"\n");
		PMap.iter (fun path b ->
			if b then output_string ch_c ("#include \"" ^ pabs ^ "/" ^ (path_to_header_path path) ^ "\"\n")
		) ctx.dependencies;
		output_string ch_c sc;
		close_out ch_c
	end

let expr_debug ctx e =
	Printf.sprintf "%s: %s" ctx.fctx.field.cf_name (s_expr (s_type (print_context())) e)

let block e = match e.eexpr with
	| TBlock _ -> e
	| _ -> mk (TBlock [e]) e.etype e.epos

let begin_loop ctx =
	ctx.fctx.loop_stack <- None :: ctx.fctx.loop_stack;
	fun () ->
		match ctx.fctx.loop_stack with
		| ls :: l ->
			(match ls with None -> () | Some s -> print ctx "%s: {}" s);
			ctx.fctx.loop_stack <- l;
		| _ ->
			assert false

let mk_ccode ctx s =
	mk (TCall ((mk (TLocal ctx.con.cvar) t_dynamic Ast.null_pos), [mk (TConst (TString s)) t_dynamic Ast.null_pos])) t_dynamic Ast.null_pos

let mk_int ctx i p =
	mk (TConst (TInt (Int32.of_int i))) ctx.con.com.basic.tint p

let full_field_name c cf = (path_to_name c.cl_path) ^ "_" ^ cf.cf_name
let full_enum_field_name en ef = (path_to_name en.e_path) ^ "_" ^ ef.ef_name

let add_dependency ctx path =
	if path <> ctx.curpath then ctx.dependencies <- PMap.add path true ctx.dependencies

let add_class_dependency ctx c =
	if not c.cl_extern then add_dependency ctx c.cl_path

let add_type_dependency ctx t = match follow t with
	| TInst(c,_) ->
		add_class_dependency ctx c
	| TEnum(en,_) ->
		add_dependency ctx en.e_path
	| TAnon _ ->
		add_dependency ctx (["hxc"],"AnonTypes");
	| _ ->
		()

let anon_signature ctx fields =
	let fields = PMap.fold (fun cf acc -> cf :: acc) fields [] in
	let fields = List.sort (fun cf1 cf2 -> compare cf1.cf_name cf2.cf_name) fields in
	let id = String.concat "," (List.map (fun cf -> cf.cf_name ^ (s_type (print_context()) (follow cf.cf_type))) fields) in
	try fst (PMap.find id ctx.con.anon_types)
	with Not_found ->
		ctx.con.num_anon_types <- ctx.con.num_anon_types + 1;
		let s = "_hx_anon_" ^ (string_of_int ctx.con.num_anon_types) in
		ctx.con.anon_types <- PMap.add id (s,fields) ctx.con.anon_types;
		s

let t_path t = match follow t with
	| TInst(c,_) -> c.cl_path
	| TEnum(e,_) -> e.e_path
	| TAbstract(a,_) -> a.a_path
	| _ -> [],"Dynamic"

let rec s_type ctx t =
	match follow t with
	| TAbstract({a_path = [],"Int"},[]) -> "int"
	| TAbstract({a_path = [],"Float"},[]) -> "double"
	| TAbstract({a_path = [],"Void"},[]) -> "void"
	| TAbstract({a_path = ["c"],"Pointer"},[t]) -> s_type ctx t ^ "*"
	| TInst(({cl_path = [],"typeref"} as c),_) ->
		add_class_dependency ctx c;
		"const " ^ (path_to_name c.cl_path) ^ "*"
	| TAbstract({a_path = [],"Bool"},[]) -> "int"
	| TInst({cl_path = [],"String"},[]) -> "char*"
	| TInst({cl_kind = KTypeParameter _},_) -> "void*"
	| TInst(c,_) ->
		let ptr = if is_value_type ctx t then "" else "*" in
		add_class_dependency ctx c;
		(path_to_name c.cl_path) ^ ptr
	| TEnum(en,_) ->
		let ptr = if is_value_type ctx t then "" else "*" in
		if not en.e_extern then add_dependency ctx en.e_path;
		(path_to_name en.e_path) ^ ptr
	| TAnon a ->
		begin match !(a.a_status) with
		| Statics c -> "Class_" ^ (path_to_name c.cl_path) ^ "*"
		| EnumStatics en -> "Enum_" ^ (path_to_name en.e_path) ^ "*"
		| AbstractStatics a -> "Anon_" ^ (path_to_name a.a_path) ^ "*"
		| _ ->
			add_dependency ctx (["hxc"],"AnonTypes");
			(anon_signature ctx a.a_fields) ^ "*"
		end
	| _ -> "void*"

let get_type_id ctx t =
	let id = Type.s_type (print_context()) (follow t) in
	try
		PMap.find id ctx.con.type_ids
	with Not_found ->
		ctx.con.num_identified_types <- ctx.con.num_identified_types + 1;
		ctx.con.type_ids <- PMap.add id ctx.con.num_identified_types ctx.con.type_ids;
		ctx.con.num_identified_types

let monofy_class c = TInst(c,List.map (fun _ -> mk_mono()) c.cl_types)

let declare_var ctx v = if not (List.mem v ctx.local_vars) then ctx.local_vars <- v :: ctx.local_vars

let get_fun t =
  match follow t with | TFun(r1,r2) -> (r1,r2) | _ -> assert false

let infer_params ctx pos (original_args:((string * bool * t) list * t)) (applied_args:((string * bool * t) list * t)) (params:(string * t) list) calls_parameters_explicitly : tparams =
	match params with
	| [] -> []
	| _ ->
		let args_list args = (if not calls_parameters_explicitly then t_dynamic else snd args) :: (List.map (fun (n,o,t) -> t) (fst args)) in

		let monos = List.map (fun _ -> mk_mono()) params in
		let original = args_list (get_fun (apply_params params monos (TFun(fst original_args,snd original_args)))) in
		let applied = args_list applied_args in

		(try
			List.iter2 (fun a o ->
				unify a o
				(* type_eq EqStrict a o *)
			) applied original
			(* unify applied original *)
		with | Unify_error el ->
				(* List.iter (fun el -> gen.gcon.warning (Typecore.unify_error_msg (print_context()) el) pos) el; *)
				ctx.con.com.warning ("This expression may be invalid") pos
		| Invalid_argument("List.map2") ->
				ctx.con.com.warning ("This expression may be invalid") pos
		);

		List.map (fun t ->
			match follow t with
				| TMono _ -> t_dynamic
				| t -> t
		) monos

(** separate special behaviour from raw code handling *)
let rec handle_special_call ctx e = match e.eexpr with
	| TCall({eexpr = TLocal({v_name = "__trace"})},[e1]) ->
		spr ctx "printf(\"%s\\n\",";
		generate_expr ctx e1;
		spr ctx ")";
		true
	| TCall({eexpr = TLocal({v_name = "__c"})},[{eexpr = TConst(TString code)}]) ->
		spr ctx code;
		true
	| TCall({eexpr = TLocal({v_name = "__call"})},{eexpr = TConst(TString name)} :: p) ->
		print ctx "%s(" name;
		concat ctx "," (generate_expr ctx) p;
		spr ctx ")";
		true
	(* sizeof *)
	| TCall({eexpr = TLocal({v_name = "__sizeof__"})},[e1]) ->
		(* get TypeReference's type *)
		let t = match follow e1.etype with
			| TInst({cl_path = [],"typeref"},[t]) -> t
			| _ -> ctx.con.com.error "This expression cannot be generated. Expected a TypeReference type" e1.epos; assert false
		in
		(match follow t with
		| TInst({cl_kind = KTypeParameter _},_) ->
			(* indirection *)
			spr ctx "(";
			generate_expr ctx e1;
			spr ctx ")->refSize"
		| _ ->
			print ctx "sizeof(%s)" (s_type ctx t));
		true
	(* pointer functions *)
	| TCall({eexpr = TField(_,FStatic({cl_path = ["c";"_Pointer"],"Pointer_Impl_"}, ({ cf_name = ("__get"|"__set") } as cf)))}, ethis :: p) ->
		spr ctx "(";
		generate_expr ctx ethis;
		spr ctx "[";
		(match cf.cf_name, p with
		| "__get", [i] ->
			generate_expr ctx i;
			spr ctx "])"
		| "__set", [i;v] ->
			generate_expr ctx i;
			spr ctx "] = ";
			generate_expr ctx v;
			spr ctx ")"
		| _ -> assert false);
		true
	| TCall({eexpr = TField(_,FStatic({cl_path = ["c";"_Pointer"],"Pointer_Impl_"}, ({ cf_name = ("add"|"increment") } as cf)))}, p) ->
		spr ctx "(";
		(match cf.cf_name, p with
		| "add", [a;o] ->
			generate_expr ctx a;
			spr ctx " + ";
			generate_expr ctx o
		| "increment", [a] ->
			generate_expr ctx a;
			spr ctx "++"
		| _ -> assert false);
		spr ctx ")";
		true
	| TCall(({eexpr = TField(e1,FInstance(c,({cf_params = _::_} as cf)))} as ef),el) ->
		generate_tparam_call ctx e ef e1 c cf false el;
		true
	| TCall(({eexpr = TField(e1,FStatic(c,({cf_params = _::_} as cf)))} as ef),el) ->
		generate_tparam_call ctx e ef e1 c cf true el;
		true
	| _ -> false

(** this is here to separate array implementation from all code, so it can be easily swapped *)
and handle_array ctx e = match e.eexpr with
  | TArray(e1, e2) -> (try
    match follow e1.etype with
    | TInst(c,p) ->
      let f = PMap.find "__get" c.cl_fields in
      generate_expr ctx { e with eexpr = TCall({ e1 with eexpr = TField(e1,FInstance(c,f)); etype = apply_params c.cl_types p f.cf_type}, [e2]) }
    | _ -> raise Not_found
  with | Not_found ->
    generate_expr ctx e1;
    spr ctx "[";
    generate_expr ctx e2;
    spr ctx "]");
    true
  | TBinop( (Ast.OpAssign | Ast.OpAssignOp _ as op), {eexpr = TArray(e1,e2)}, v) -> (try
    match follow e1.etype with
    | TInst(c,p) ->
      let f = PMap.find "__set" c.cl_fields in
      if op <> Ast.OpAssign then assert false; (* FIXME: this should be handled in an earlier stage (gencommon, anyone?) *)
      generate_expr ctx { e with eexpr = TCall({ e1 with eexpr = TField(e1,FInstance(c,f)); etype = apply_params c.cl_types p f.cf_type}, [e2;v]) }
    | _ -> raise Not_found
  with | Not_found ->
    generate_expr ctx e1;
    spr ctx "[";
    generate_expr ctx e2;
    spr ctx "]";
		print ctx " %s " (s_binop op);
    generate_expr ctx v);
    true
  | TArrayDecl [] ->
    let c,p = match follow e.etype with
      | TInst(c,[p]) ->
        c,p
      | _ -> assert false
    in
    generate_expr ctx { e with eexpr = TNew(c,[p],[]) };
    true
  | TArrayDecl el ->
    (* Array.ofNative(FixedArray.ofPointerCopy(len,{})) *)
    let t, param = match follow e.etype with
      | TInst({ cl_path = [],"Array"},[p]) ->
        p, mk_type_param ctx e.epos p
      | _ -> assert false
    in
    spr ctx "Array_ofPointerCopy(";
    generate_expr ctx param;
    print ctx ", %d, (void *) (%s[]) { " (List.length el) (s_type ctx t);
    concat ctx "," (generate_expr ctx) el;
    spr ctx " })";
    true
  | _ -> false


and mk_type_param ctx pos t =
	let t = ctx.con.ttype t in
	let c,p = match follow t with
		| TInst(c,p) -> c,p
		| _ -> assert false
	in
	{ eexpr = TNew(c,p,[]); etype = t; epos = pos }

and generate_tparam_call ctx e ef e1 c cf static el =
	(* FIXME: cf.cf_type may inherit the type parameters of a superclass *)
	let params = infer_params ctx e.epos (get_fun cf.cf_type) (get_fun ef.etype) cf.cf_params true in
	let el = (if static then [] else [e1]) @ List.map (mk_type_param ctx e.epos) params @ el in
	add_class_dependency ctx c;
	spr ctx (full_field_name c cf);
	spr ctx "(";
	concat ctx "," (generate_expr ctx) el;
	spr ctx ")"

and generate_expr ctx e = match e.eexpr with
  | TArray _ | TBinop _ | TArrayDecl _ when handle_array ctx e -> ()
	| TBlock([]) ->
		spr ctx "{ }"
	| TBlock(el) ->
		spr ctx "{";
		let b = open_block ctx in
		List.iter (fun e ->
			newline ctx;
			generate_expr ctx e;
		) el;
		b();
		newline ctx;
		spr ctx "}";
		newline ctx;
	| TConst(TString s) ->
		print ctx "\"%s\"" s
	| TConst(TInt i) ->
		print ctx "%ld" i
	| TConst(TFloat s) ->
		print ctx "%s" s
	| TConst(TNull) ->
		spr ctx "NULL"
	| TConst(TSuper) ->
		(* TODO: uhm... *)
		()
	| TConst(TBool true) ->
		spr ctx "TRUE"
	| TConst(TBool false) ->
		spr ctx "FALSE"
	| TConst(TThis) ->
		spr ctx "this"
	| TCall _ when handle_special_call ctx e -> ()
	| TCall({eexpr = TField(e1,FInstance(c,cf))},el) ->
		add_class_dependency ctx c;
		spr ctx (full_field_name c cf);
		spr ctx "(";
		generate_expr ctx e1;
		List.iter (fun e ->
			spr ctx ",";
			generate_expr ctx e
		) el;
		spr ctx ")"
	| TCall({eexpr = TField(_,FEnum(en,ef))},el) ->
		print ctx "new_%s(" (full_enum_field_name en ef);
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")"
	| TCall(e1, el) ->
		generate_expr ctx e1;
		spr ctx "(";
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")"
	| TTypeExpr (TClassDecl c) ->
		spr ctx (path_to_name c.cl_path);
	| TTypeExpr (TEnumDecl e) ->
		add_dependency ctx e.e_path;
		spr ctx (path_to_name e.e_path);
	| TTypeExpr (TTypeDecl _ | TAbstractDecl _) ->
		(* shouldn't happen? *)
		assert false
	| TField(_,FStatic(c,cf)) ->
		add_class_dependency ctx c;
		spr ctx (full_field_name c cf)
	| TField(_,FEnum(en,ef)) ->
		add_dependency ctx en.e_path;
		print ctx "new_%s()" (full_enum_field_name en ef)
	| TField(e1,fa) ->
		let n = field_name fa in
		spr ctx "(";
		generate_expr ctx e1;
		if is_value_type ctx e1.etype then
			print ctx ").%s" n
		else
			print ctx ")->%s" n
	| TLocal v ->
		spr ctx v.v_name;
	| TObjectDecl fl ->
		let s = match follow e.etype with TAnon a -> anon_signature ctx a.a_fields | _ -> assert false in
		let fl = List.sort (fun (n1,_) (n2,_) -> compare n1 n2) fl in
		print ctx "new_%s(" s;
		concat ctx "," (generate_expr ctx) (List.map (fun (_,e) -> add_type_dependency ctx e.etype; e) fl);
		spr ctx ")";
	| TNew({cl_path = [],"typeref"},[p],[]) -> (match follow p with
		| TInst(({cl_kind = KTypeParameter _} as c),_) -> (try
			let expr = List.assoc c.cl_path ctx.con.type_parameters in
			generate_expr ctx expr
		with | Not_found ->
			ctx.con.com.error ("Cannot find type parameter called " ^ s_type_path c.cl_path) e.epos)
		| _ ->
			let path = t_path p in
			add_dependency ctx path;
			spr ctx ("&" ^ (path_to_name path ) ^ "__typeref"))
	| TNew(c,tl,el) ->
		let el = List.map (mk_type_param ctx e.epos) tl @ el in
		add_class_dependency ctx c;
		spr ctx (full_field_name c (match c.cl_constructor with None -> assert false | Some cf -> cf));
		spr ctx "(";
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")";
	| TReturn None ->
		spr ctx "return"
	| TReturn (Some e1) ->
		spr ctx "return (";
		generate_expr ctx e1;
		spr ctx ")"
	| TBinop(OpAssign, e1, e2) ->
		generate_expr ctx e1;
		spr ctx " = ";
		generate_expr ctx e2;
	| TVars(vl) ->
		let f (v,eo) =
			print ctx "%s %s" (s_type ctx v.v_type) v.v_name;
			begin match eo with
				| None -> ()
				| Some e ->
					spr ctx " = ";
					generate_expr ctx e;
			end
		in
		concat ctx ";" f vl
	| TWhile(e1,e2,NormalWhile) ->
		spr ctx "while";
		generate_expr ctx e1;
		let l = begin_loop ctx in
		generate_expr ctx (block e2);
		l()
	| TWhile(e1,e2,DoWhile) ->
		spr ctx "do";
		let l = begin_loop ctx in
		generate_expr ctx (block e2);
		spr ctx " while";
		generate_expr ctx e1;
		l()
	| TContinue ->
		spr ctx "continue";
	| TBreak _ ->
		let label = match ctx.fctx.loop_stack with
			| (Some s) :: _ -> s
			| None :: l ->
				let s = Printf.sprintf "_hx_label%i" ctx.con.num_labels in
				ctx.con.num_labels <- ctx.con.num_labels + 1;
				ctx.fctx.loop_stack <- (Some s) :: l;
				s
			| [] ->
				assert false
		in
		print ctx "goto %s" label;
	| TIf(e1,e2,e3) ->
		spr ctx "if";
		generate_expr ctx e1;
		generate_expr ctx (block e2);
		(match e3 with None -> () | Some e3 ->
			spr ctx " else ";
			generate_expr ctx (block e3))
	| TSwitch(e1,cases,edef) ->
		spr ctx "switch";
		generate_expr ctx e1;
		spr ctx "{";
		let generate_case_expr e =
			let b = open_block ctx in
			List.iter (fun e ->
				newline ctx;
				generate_expr ctx e;
			) (match e.eexpr with TBlock el -> el | _ -> [e]);
			newline ctx;
			spr ctx "break";
			b();
		in
		let b = open_block ctx in
		newline ctx;
		List.iter (fun (el,e) ->
			spr ctx "case ";
			concat ctx "," (generate_expr ctx) el;
			spr ctx ":";
			generate_case_expr e;
			newline ctx;
		) cases;
		begin match edef with
			| None -> ()
			| Some e ->
				spr ctx "default:";
				generate_case_expr e;
		end;
		b();
		newline ctx;
		spr ctx "}";
	| TBinop(op,e1,e2) ->
		generate_expr ctx e1;
		print ctx " %s " (s_binop op);
		generate_expr ctx e2;
	| TUnop(op,Prefix,e1) ->
		spr ctx (s_unop op);
		generate_expr ctx e1;
	| TUnop(op,Postfix,e1) ->
		generate_expr ctx e1;
		spr ctx (s_unop op);
	| TParenthesis e1 ->
		spr ctx "(";
		generate_expr ctx e1;
		spr ctx ")";
	| TArrayDecl _ | TTry _ | TFor _ ->
		(* handled in function context pass *)
		assert false
	| TMeta(_,e) ->
		generate_expr ctx e
	| TCast(e,_) ->
		(* TODO: make this do something *)
		generate_expr ctx e
	| TEnumParameter (e1,ef,i) ->
		generate_expr ctx e1;
		begin match follow e1.etype with
			| TEnum(en,_) ->
				add_dependency ctx en.e_path;
				let s,_,_ = match ef.ef_type with TFun(args,_) -> List.nth args i | _ -> assert false in
				print ctx "->args.%s.%s" ef.ef_name s;
			| _ ->
				assert false
		end
	| TThrow e1 ->
		ctx.dependencies <- PMap.add ([],"setjmp") false ctx.dependencies;
		add_dependency ctx (["c";"hxc"],"Exception");
		spr ctx "c_hxc_Exception_thrownObject = ";
		generate_expr ctx e1;
		newline ctx;
		print ctx "(longjmp(*c_hxc_Exception_peek(),%i))" (get_type_id ctx e1.etype);
	| TPatMatch dt ->
		let fl = ctx.con.num_labels in
		ctx.con.num_labels <- ctx.con.num_labels + (Array.length dt.dt_dt_lookup) + 1;
		let mk_label i = Printf.sprintf "_hx_label%i" (i + fl) in
		let rec loop d =
			match d with
			| DTGoto i ->
				print ctx "goto %s" (mk_label i);
				newline ctx;
			| DTBind(bl,dt) ->
				List.iter (fun ((v,p),e) ->
					print ctx "%s = " v.v_name;
					generate_expr ctx e;
					newline ctx;
				) bl;
				loop dt
			| DTExpr e -> generate_expr ctx (block e)
			| DTGuard(e, dt1, dt2) ->
				spr ctx "if(";
				generate_expr ctx e;
				spr ctx ")";
				loop dt1;
				(match dt2 with None -> () | Some dt ->
					spr ctx " else ";
					loop dt)
			| DTSwitch(e,cl) ->
				let def = ref None in
				let cl = List.filter (fun (e,dt) ->
					match e.eexpr with
	 				| TMeta((Meta.MatchAny,_,_),_) ->
						def := Some dt;
						false
					| _ ->
						true
				) cl in
				spr ctx "switch(";
				generate_expr ctx e;
				spr ctx ") {";
				let b = open_block ctx in
				List.iter (fun (e,dt) ->
					newline ctx;
					spr ctx "case ";
					generate_expr ctx e;
					spr ctx ":";
					loop dt;
					newline ctx;
					spr ctx "break";
				) cl;
				begin match !def with
					| None -> ()
					| Some dt ->
						newline ctx;
						spr ctx "default:";
						loop dt;
						newline ctx;
						spr ctx "break";
				end;
				b();
				newline ctx;
				spr ctx "}";
				newline ctx;
		in
		print ctx "goto %s" (mk_label dt.dt_first);
		Array.iteri (fun i d ->
			newline ctx;
			print ctx "%s: {}" (mk_label i);
			newline ctx;
			loop d;
			print ctx "goto %s" (mk_label (Array.length dt.dt_dt_lookup));
			newline ctx;
		) dt.dt_dt_lookup;
		print ctx "%s: {}" (mk_label (Array.length dt.dt_dt_lookup));
		newline ctx;
	| TFunction _ ->
		print_endline ("Not implemented yet: " ^ (expr_debug ctx e))
  | TArray _ -> assert false

let mk_function_context ctx cf =
	let locals = ref [] in
	let rec loop e = match e.eexpr with
		| TVars vl ->
			let el = ExtList.List.filter_map (fun (v,eo) ->
				locals := v :: !locals;
				match eo with
				| None -> None
				| Some e -> Some (mk (TBinop(OpAssign, mk (TLocal v) v.v_type e.epos,loop e)) e.etype e.epos)
			) vl in
			begin match el with
			| [e] -> e
			| _ -> mk (TBlock el) ctx.con.com.basic.tvoid e.epos
			end
		| TTry (e1,cl) ->
			ctx.dependencies <- PMap.add ([],"setjmp") false ctx.dependencies;
			add_dependency ctx (["c";"hxc"],"Exception");
			let esubj = mk_ccode ctx "(setjmp(*c_hxc_Exception_push()))" in
			let epop = mk_ccode ctx "c_hxc_Exception_pop()" in
			let epopassign = mk_ccode ctx "jmp_buf* _hx_jmp_buf = c_hxc_Exception_pop()" in
			let c1 = [mk_int ctx 0 e.epos],(Codegen.concat (loop e1) epop) in
			let def = ref None in
			let cl = c1 :: (ExtList.List.filter_map (fun (v,e) ->
				let eassign = mk_ccode ctx ((s_type ctx v.v_type) ^ " " ^ v.v_name ^ " = c_hxc_Exception_thrownObject") in
				let e = Codegen.concat eassign (Codegen.concat epopassign e) in
				let e = mk (TBlock [e]) e.etype e.epos in
				if v.v_type == t_dynamic then begin
					def := Some e;
					None;
				end else
					Some ([mk_int ctx (get_type_id ctx v.v_type) e.epos],e)
			) cl) in
			mk (TSwitch(esubj,cl,!def)) e.etype e.epos
		| TPatMatch dt ->
 			let rec dtl d = match d with
				| DTGoto _ | DTExpr _ ->
					()
				| DTGuard(_,dt1,dt2) ->
					dtl dt1;
					(match dt2 with None -> () | Some dt -> dtl dt)
				| DTSwitch(_,cl) ->
					List.iter (fun (_,dt) -> dtl dt) cl
				| DTBind(bl,dt) ->
					List.iter (fun ((v,_),_) ->
						if v.v_name.[0] = '`' then v.v_name <- "_" ^ (String.sub v.v_name 1 (String.length v.v_name - 1));
						locals := v :: !locals
					) bl;
					dtl dt
			in
			Array.iter dtl dt.dt_dt_lookup;
			List.iter (fun (v,_) ->
				if v.v_name.[0] = '`' then v.v_name <- "_" ^ (String.sub v.v_name 1 (String.length v.v_name - 1));
				locals := v :: !locals
			) dt.dt_var_init;
			Type.map_expr loop e
		| TFor(v,e1,e2) ->
			let ehasnext = mk (TField(e1,quick_field e1.etype "hasNext")) ctx.con.com.basic.tbool e1.epos in
			let enext = mk (TField(e1,quick_field e1.etype "next")) v.v_type e1.epos in
			let ebody = Codegen.concat enext e2 in
			mk (TBlock [
				mk (TVars [v,None]) ctx.con.com.basic.tvoid e1.epos;
				mk (TWhile(ehasnext,ebody,NormalWhile)) ctx.con.com.basic.tvoid e1.epos;
			]) ctx.con.com.basic.tvoid e.epos
		| _ -> Type.map_expr loop e
	in
	let e = match cf.cf_expr with
		| None -> None
		| Some e -> Some (loop e)
	in
	{
		field = cf;
		local_vars = !locals;
		expr = e;
		loop_stack = [];
	}

let generate_function_header ctx c cf =
	let args,ret = match follow cf.cf_type with
		| TFun(args,ret) -> args,ret
		| _ -> assert false
	in
	print ctx "%s %s(%s)" (s_type ctx ret) (full_field_name c cf) (String.concat "," (List.map (fun (n,_,t) -> Printf.sprintf "%s %s" (s_type ctx t) n) args))

let get_typeref_forward ctx path =
	Printf.sprintf "extern const typeref %s__typeref" (path_to_name path)

let get_typeref_declaration ctx t =
	let path = t_path t in
	Printf.sprintf "const typeref %s__typeref = { \"%s\", sizeof(%s) }; //typeref declaration" (path_to_name path) (s_type_path path) (s_type ctx t)

let generate_method ctx c cf =
	ctx.fctx <- mk_function_context ctx cf;
	generate_function_header ctx c cf;
	match ctx.fctx.expr with
	| None -> newline ctx
	| Some {eexpr = TFunction ({tf_expr = {eexpr = TBlock el}; tf_type = t})} ->
		let el = match ctx.fctx.local_vars with
			| [] -> el
			| _ ->
				let einit = mk (TVars (List.map (fun v -> v,None) ctx.fctx.local_vars)) ctx.con.com.basic.tvoid cf.cf_pos in
				einit :: el
		in
		let e = mk (TBlock el) t cf.cf_pos in
		generate_expr ctx e
	| _ -> assert false

let change_parameter_function ctx cf vars =
	let tf_args, types = match vars, cf.cf_params with
		| _ :: _, _ -> (* vars *)
			List.map (fun (f,_) -> alloc_var f.cf_name f.cf_type, None) vars, List.map snd vars
		| _, _ :: _ ->
			List.map (fun (_,t) -> alloc_var (path_to_name (t_path t) ^ "_tp") (ctx.con.ttype t),None) cf.cf_params, List.map snd cf.cf_params
		| _ -> [],[]
	in
	match tf_args, cf.cf_type, cf.cf_expr with
	| _ :: _, TFun(args,ret), Some({ eexpr = TFunction(tf) } as e) ->
		let t = TFun(List.map (fun (v,_) -> v.v_name,false,v.v_type) tf_args @ args,ret) in
		let e = { e with eexpr = TFunction({ tf with tf_args = tf_args @ tf.tf_args }); etype = t } in
		cf.cf_expr <- Some e;
		cf.cf_type <- t;
		List.map2 (fun t (v,_) -> (t_path t), v) types tf_args
	| _ -> []

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

let cls_parameter_vars ctx c = match c.cl_types with
	| [] -> []
	| types ->
		let vars = List.map (fun (s,t) ->
			mk_class_field (path_to_name (t_path t) ^ "_tp") (ctx.con.ttype t) false c.cl_pos (Var {v_read = AccNormal; v_write = AccNormal}) []
		) types in
		c.cl_ordered_fields <- vars @ c.cl_ordered_fields;
		List.iter (fun f -> c.cl_fields <- PMap.add f.cf_name f c.cl_fields ) vars;
		List.map2 (fun v (_,t) -> v,t) vars types

let generate_class ctx c =
	let old_tparams = ctx.con.type_parameters in
	let param_vars = if c.cl_path <> ([],"typeref") then cls_parameter_vars ctx c else [] in
	let mk_this_field f =
		{ eexpr = TField({ eexpr = TConst TThis; etype = monofy_class c; epos = f.cf_pos}, FInstance(c,f)); etype = f.cf_type; epos = f.cf_pos }
	in
	ctx.con.type_parameters <- List.map (fun (f,t) ->
		t_path t, mk_this_field f) param_vars;
	(* split fields into member vars, static vars and functions *)
	let vars = DynArray.create () in
	let svars = DynArray.create () in
	let methods = DynArray.create () in
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ -> DynArray.add vars cf
		| Method _ -> match cf.cf_type with
			| TFun(args,ret) ->
				cf.cf_type <- TFun(("this",false,monofy_class c) :: args, ret);
				DynArray.add methods cf
			| _ ->
				assert false;
	) c.cl_ordered_fields;
	List.iter (fun cf -> match cf.cf_kind with
		| Var _ -> DynArray.add svars cf
		| Method _ -> DynArray.add methods cf
	) c.cl_ordered_statics;

	begin match c.cl_constructor with
		| None -> ()
		| Some cf ->
			let func_params = change_parameter_function ctx cf param_vars in
			match follow cf.cf_type, cf.cf_expr with
			| TFun(args,_), Some e ->
				let path = path_to_name c.cl_path in
				let einit =
					(if is_value_type ctx (TInst(c,List.map snd c.cl_types)) then
						mk_ccode ctx (Printf.sprintf "%s this" path)
					else
  					mk_ccode ctx (Printf.sprintf "%s* this = (%s*) GC_MALLOC(sizeof(%s))" path path path)) ::
					List.map2 (fun (f,_) (_,v) -> {
						eexpr = TBinop(
							Ast.OpAssign,
							mk_this_field f,
							{ eexpr = TLocal v; etype = v.v_type; epos = f.cf_pos });
						etype = v.v_type;
						epos = f.cf_pos
					}) param_vars func_params
				in
				let ereturn = mk_ccode ctx "return this" in
				let e = match e.eexpr with
					| TFunction({tf_expr = ({eexpr = TBlock el } as ef) } as tf) ->
						{e with eexpr = TFunction ({tf with tf_expr = {ef with eexpr = TBlock(einit @ el @ [ereturn])}})}
					| _ -> assert false
				in
				cf.cf_expr <- Some e;
				cf.cf_type <- TFun(args, monofy_class c);
				DynArray.add methods cf
			| _ -> ()
	end;

	ctx.buf <- ctx.buf_c;

	spr ctx (get_typeref_declaration ctx (TInst(c,List.map snd c.cl_types)));
	newline ctx;

	(* generate function implementations *)
	if not (DynArray.empty methods) then begin
		DynArray.iter (fun cf ->
			let old_tparams = ctx.con.type_parameters in
			let params = change_parameter_function ctx cf [] in
			let params = List.map (fun (p,v) -> p,mk (TLocal v) v.v_type cf.cf_pos) params in

			ctx.con.type_parameters <- params @ old_tparams;
			generate_method ctx c cf;
			ctx.con.type_parameters <- old_tparams
		) methods;
	end;

	(* generate static vars *)
	if not (DynArray.empty svars) then begin
		spr ctx "// static vars\n";
		DynArray.iter (fun cf ->
      let is_constant = match cf.cf_kind with
        | Var { v_read = AccInline }
        | Var { v_write = AccNever } -> true
        | _ -> false
      in
      print ctx "%s%s %s" (if is_constant then "const " else "") (s_type ctx cf.cf_type) (full_field_name c cf);
			match cf.cf_expr with
			| None -> newline ctx
			| Some e ->
				spr ctx " = ";
				generate_expr ctx e;
				newline ctx
		) svars;
	end;

	(* check if we have the main class *)
	(match ctx.con.com.main_class with
	| Some path when path = c.cl_path ->
		ctx.dependencies <- PMap.add ([],"setjmp") false ctx.dependencies;
		add_dependency ctx (["c";"hxc"],"Exception");
		print ctx "int main() {\n\tGC_INIT();\n\tswitch(setjmp(*c_hxc_Exception_push())) {\n\t\tcase 0: %s();break;\n\t\tdefault: printf(\"Something went wrong\");\n\t}\n}" (full_field_name c (PMap.find "main" c.cl_statics))
  | _ -> ());

	ctx.buf <- ctx.buf_h;

	(* generate member struct *)
	if not (DynArray.empty vars) then begin
		spr ctx "// member var structure\n";
		print ctx "typedef struct %s {" (path_to_name c.cl_path);
		let b = open_block ctx in
		DynArray.iter (fun cf ->
			newline ctx;
			print ctx "%s %s" (s_type ctx cf.cf_type) cf.cf_name;
		) vars;
		b();
		newline ctx;
		print ctx "} %s" (path_to_name c.cl_path);
		newline ctx;
		spr ctx "\n";
	end else begin
		print ctx "typedef struct %s { void* dummy; } %s" (path_to_name c.cl_path) (path_to_name c.cl_path);
		newline ctx;
	end;

	(* generate static vars *)
	if not (DynArray.empty svars) then begin
		spr ctx "// static vars\n";
		DynArray.iter (fun cf ->
      let is_constant = match cf.cf_kind with
        | Var { v_read = AccInline }
        | Var { v_write = AccNever } -> true
        | _ -> false
      in
      print ctx "extern %s%s %s" (if is_constant then "const " else "") (s_type ctx cf.cf_type) (full_field_name c cf);
      newline ctx
    ) svars
	end;

	ctx.buf <- ctx.buf_h;

	(* generate forward declarations of functions *)
	if not (DynArray.empty methods) then begin
		spr ctx "// forward declarations\n";
		DynArray.iter (fun cf ->
			generate_function_header ctx c cf;
			newline ctx;
		) methods;
	end;

	add_dependency ctx ([],"typeref");
	spr ctx (get_typeref_forward ctx c.cl_path);
	newline ctx;

	(* restore type parameters stack *)
	ctx.con.type_parameters <- old_tparams

let generate_enum ctx en =
	ctx.buf <- ctx.buf_h;
	add_dependency ctx ([],"typeref");
	spr ctx (get_typeref_forward ctx en.e_path);
	newline ctx;

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
				print ctx "%s %s" (s_type ctx t) n;
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
	spr ctx "int index";
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
			print ctx "%s new_%s(%s)" (s_type ctx ret) (full_enum_field_name en ef) (String.concat "," (List.map (fun (n,_,t) -> Printf.sprintf "%s %s" (s_type ctx t) n) args));
		| _ ->
			assert false
	) ctors;
	newline ctx;

	ctx.buf <- ctx.buf_c;
	spr ctx (get_typeref_declaration ctx (TEnum(en,List.map snd en.e_types)));
	newline ctx;

	(* generate constructor functions *)
	spr ctx "// constructor functions";
	List.iter (fun ef ->
		newline ctx;
		match ef.ef_type with
		| TFun(args,ret) ->
			print ctx "%s new_%s(%s) {" (s_type ctx ret) (full_enum_field_name en ef) (String.concat "," (List.map (fun (n,_,t) -> Printf.sprintf "%s %s" (s_type ctx t) n) args));
			let b = open_block ctx in
			newline ctx;
			print ctx "%s* this = (%s*) GC_MALLOC(sizeof(%s))" path path path;
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
	| TClassDecl c when not c.cl_extern ->
		let ctx = mk_type_context con c.cl_path in
		generate_class ctx c;
		close_type_context ctx;
	| TEnumDecl en when not en.e_extern ->
		let ctx = mk_type_context con en.e_path in
		generate_enum ctx en;
		close_type_context ctx;
	| TAbstractDecl a ->
		let ctx = mk_type_context con a.a_path in
		ctx.buf <- ctx.buf_c;
		spr ctx (get_typeref_declaration ctx (TAbstract(a,List.map snd a.a_types)));
		newline ctx;
		ctx.buf <- ctx.buf_h;
		add_dependency ctx ([],"typeref");
		spr ctx (get_typeref_forward ctx a.a_path);
		newline ctx;
		close_type_context ctx;
	| _ ->
		()

let generate_hxc_files con =
	let ctx = mk_type_context con (["hxc"],"AnonTypes") in

	spr ctx "// forward declarations";
	PMap.iter (fun _ (s,_) ->
		newline ctx;
		print ctx "typedef struct %s %s" s s;
	) con.anon_types;
	newline ctx;

	spr ctx "// structures";
	PMap.iter (fun _ (s,cfl) ->
		newline ctx;
		print ctx "typedef struct %s {" s;
		let b = open_block ctx in
		List.iter (fun cf ->
			newline ctx;
			print ctx "%s %s" (s_type ctx cf.cf_type) cf.cf_name;
		) cfl;
		b();
		newline ctx;
		print ctx "} %s" s;
	) con.anon_types;
	newline ctx;
	spr ctx "// constructor forward declarations";
	PMap.iter (fun _ (s,cfl) ->
		newline ctx;
		print ctx "%s* new_%s(%s)" s s (String.concat "," (List.map (fun cf -> Printf.sprintf "%s %s" (s_type ctx cf.cf_type) cf.cf_name) cfl));
	) con.anon_types;
	newline ctx;

	ctx.buf <- ctx.buf_c;

	spr ctx "// constructor definitions";
	PMap.iter (fun _ (s,cfl) ->
		newline ctx;
		print ctx "%s* new_%s(%s) {" s s (String.concat "," (List.map (fun cf -> Printf.sprintf "%s %s" (s_type ctx cf.cf_type) cf.cf_name) cfl));
		let b = open_block ctx in
		newline ctx;
		print ctx "%s* this = (%s*) GC_MALLOC(sizeof(%s))" s s s;
		List.iter (fun cf ->
			newline ctx;
			print ctx "this->%s = %s" cf.cf_name cf.cf_name;
		) cfl;
		newline ctx;
		spr ctx "return this";
		b();
		newline ctx;
		spr ctx "}"
	) con.anon_types;
	close_type_context ctx

let get_type com path = try
	match List.find (fun md -> Type.t_path md = path) com.types with
	| TClassDecl c -> TInst(c, List.map snd c.cl_types)
	| TEnumDecl e -> TEnum(e, List.map snd e.e_types)
	| TTypeDecl t -> TType(t, List.map snd t.t_types)
	| TAbstractDecl a -> TAbstract(a, List.map snd a.a_types)
with | Not_found ->
	com.error ("The type " ^ Ast.s_type_path path ^ " is required and was not found") Ast.null_pos; assert false

let generate com =
	let ttype = get_type com ([],"typeref") in
	let con = {
		com = com;
		cvar = alloc_var "__c" t_dynamic;
		num_temp_funcs = 0;
		num_labels = 0;
		num_anon_types = -1;
		(* this has to start at 0 so the first type id is 1 *)
		num_identified_types = 0;
		anon_types = PMap.empty;
		type_ids = PMap.empty;
		type_parameters = [];
		ttype = (fun t -> match follow ttype with
			| TInst(c,[_]) -> TInst(c,[t])
			| _ -> assert false);
	} in
	List.iter (generate_type con) com.types;
	generate_hxc_files con
