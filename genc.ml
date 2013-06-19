open Ast
open Common
open Type

type context = {
	com : Common.context;
	cvar : tvar;
	mutable num_temp_funcs : int;
}

type function_context = {
	field : tclass_field;
	expr : texpr option;
	mutable local_vars : tvar list;
}

type type_context = {
	con : context;
	file_path_no_ext : string;
	buf_c : Buffer.t;
	buf_h : Buffer.t;
	mutable buf : Buffer.t;
	mutable tabs : string;
	mutable curclass : tclass;
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
		curclass = null_class;
		fctx = {
			local_vars = [];
			field = null_field;
			expr = None;
		};
		dependencies = PMap.empty;
	}

let path_to_name (pack,name) = match pack with [] -> name | _ -> String.concat "_" pack ^ "_" ^ name
let path_to_header_path (pack,name) = match pack with [] -> name ^ ".h" | _ -> String.concat "/" pack ^ "/" ^ name ^ ".h"

let close_type_context ctx =
	let n = "_h" ^ path_to_name ctx.curclass.cl_path in
	let ch_h = open_out_bin (ctx.file_path_no_ext ^ ".h") in

	output_string ch_h ("#ifndef " ^ n ^ "\n");
	output_string ch_h ("#define " ^ n ^ "\n");
	output_string ch_h "#define GC_NOT_DLL\n";
	output_string ch_h "#include \"gc.h\"\n";
	output_string ch_h "#include \"glib/garray.h\"\n";
	let pabs = get_full_path ctx.con.com.file in
	PMap.iter (fun path _ ->
		output_string ch_h ("#include \"" ^ pabs ^ "/" ^ (path_to_header_path path) ^ "\"\n")
	) ctx.dependencies;
	output_string ch_h (Buffer.contents ctx.buf_h);
	output_string ch_h "\n#endif";
	close_out ch_h;

	let ch_c = open_out_bin (ctx.file_path_no_ext ^ ".c") in
	output_string ch_c ("#include \"" ^ (snd ctx.curclass.cl_path) ^ ".h\"\n");
	output_string ch_c (Buffer.contents ctx.buf_c);
	close_out ch_c

let expr_debug ctx e =
	Printf.sprintf "%s: %s" ctx.fctx.field.cf_name (s_expr (s_type (print_context())) e)

let mk_ccode ctx s =
	mk (TCall ((mk (TLocal ctx.con.cvar) t_dynamic Ast.null_pos), [mk (TConst (TString s)) t_dynamic Ast.null_pos])) t_dynamic Ast.null_pos

let full_field_name c cf = (path_to_name c.cl_path) ^ "_" ^ cf.cf_name

let add_dependency ctx c =
	if c != ctx.curclass then ctx.dependencies <- PMap.add c.cl_path true ctx.dependencies

let s_type ctx t = match follow t with
	| TAbstract({a_path = [],"Int"},[]) -> "int"
	| TAbstract({a_path = [],"Float"},[]) -> "double"
	| TAbstract({a_path = [],"Void"},[]) -> "void"
	| TInst({cl_path = [],"String"},[]) -> "char*"
	| TInst({cl_path = [],"Array"},[_]) -> "GArray*"
	| TInst({cl_kind = KTypeParameter _},_) -> "void*"
	| TInst(c,_) ->
		add_dependency ctx c;
		(path_to_name c.cl_path) ^ "*"
	| _ -> "void*"

let monofy_class c = TInst(c,List.map (fun _ -> mk_mono()) c.cl_types)

let declare_var ctx v = if not (List.mem v ctx.local_vars) then ctx.local_vars <- v :: ctx.local_vars

let rec generate_expr ctx e = match e.eexpr with
	| TBlock([]) -> spr ctx "{ }"
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
	| TConst(TThis) ->
		spr ctx "this"
	| TCall({eexpr = TLocal({v_name = "__trace"})},[e1]) ->
		spr ctx "printf(\"%s\\n\",";
		generate_expr ctx e1;
		spr ctx ")";
	| TCall({eexpr = TLocal({v_name = "__c"})},[{eexpr = TConst(TString code)}]) ->
		spr ctx code
	| TCall({eexpr = TField(e1,FInstance(c,cf))},el) ->
		add_dependency ctx c;
		spr ctx (full_field_name c cf);
		spr ctx "(";
		generate_expr ctx e1;
		List.iter (fun e ->
			spr ctx ",";
			generate_expr ctx e
		) el;
		spr ctx ")"
	| TCall(e1, el) ->
		spr ctx "(";
		generate_expr ctx e1;
		spr ctx ")(";
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")"
	| TTypeExpr (TClassDecl c) ->
		spr ctx (path_to_name c.cl_path);
	| TField(_,FStatic(c,cf)) ->
		add_dependency ctx c;
		spr ctx (full_field_name c cf)
	| TField(e1,fa) ->
		let n = field_name fa in
		spr ctx "(";
		generate_expr ctx e1;
		print ctx ")->%s" n
	| TLocal v ->
		spr ctx v.v_name;
	| TObjectDecl _ ->
		spr ctx "0";
	| TNew(c,_,el) ->
		add_dependency ctx c;
		spr ctx (full_field_name c (match c.cl_constructor with None -> assert false | Some cf -> cf));
		spr ctx "(";
		concat ctx "," (generate_expr ctx) el;
		spr ctx ")";
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
	| TArray(e1,e2) ->
		spr ctx "g_array_index(";
		generate_expr ctx e1;
		spr ctx ",";
		spr ctx (s_type ctx e.etype);
		spr ctx ",";
		generate_expr ctx e2;
		spr ctx ")";
	| TWhile(e1,e2,NormalWhile) ->
		spr ctx "while(";
		generate_expr ctx e1;
		spr ctx ")";
		generate_expr ctx e2;
	| TBinop(op,e1,e2) ->
		spr ctx "(";
		generate_expr ctx e1;
		spr ctx ")";
		spr ctx (s_binop op);
		spr ctx "(";
		generate_expr ctx e2;
		spr ctx ")";
	| TUnop(op,Prefix,e1) ->
		begin match op with
		| Increment ->
			(* TODO: sanitize *)
			generate_expr ctx e1;
			spr ctx " = ";
			generate_expr ctx e1;
			spr ctx " + 1";
		| _ ->
			assert false
		end
	| TParenthesis e1 ->
		spr ctx "(";
		generate_expr ctx e1;
		spr ctx ")";
	| t ->
		print_endline (expr_debug ctx e)

let mk_array_decl ctx el t p =
	let ts = match follow t with
		| TInst(_,[t]) -> s_type ctx t
		| _ -> assert false
	in
	let name = "_hx_func_" ^ (string_of_int ctx.con.num_temp_funcs) in
	let arity = List.length el in
	print ctx "GArray* %s(%s) {" name (String.concat "," (ExtList.List.mapi (fun i e -> Printf.sprintf "%s v%i" (s_type ctx e.etype) i) el));
	ctx.con.num_temp_funcs <- ctx.con.num_temp_funcs + 1;
	let bl = open_block ctx in
	newline ctx;
	print ctx "GArray* garray = g_array_sized_new(FALSE, FALSE, sizeof(%s), %i)" ts arity;
	newline ctx;
	ExtList.List.iteri (fun i e ->
		print ctx "g_array_append_val(garray, v%i)" i;
		newline ctx;
	) el;
	spr ctx "return garray";
	bl();
	newline ctx;
	spr ctx "}";
	newline ctx;
	let v = alloc_var name t_dynamic in
	let ev = mk (TLocal v) v.v_type p in
	mk (TCall(ev,el)) t p

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
		| TArrayDecl el ->
			mk_array_decl ctx el e.etype e.epos
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
	}

let generate_function_header ctx c cf =
	let args,ret = match follow cf.cf_type with
		| TFun(args,ret) -> args,ret
		| _ -> assert false
	in
	print ctx "%s %s(%s)" (s_type ctx ret) (full_field_name c cf) (String.concat "," (List.map (fun (n,_,t) -> Printf.sprintf "%s %s" (s_type ctx t) n) args))

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

let generate_class ctx c =
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
		| Some cf -> match follow cf.cf_type, cf.cf_expr with
			| TFun(args,_), Some e ->
				let path = path_to_name c.cl_path in
				let einit = mk_ccode ctx (Printf.sprintf "%s* this = (%s*) GC_MALLOC(sizeof(%s))" path path path) in
				let ereturn = mk_ccode ctx "return this" in
				let e = match e.eexpr with
					| TFunction({tf_expr = ({eexpr = TBlock el } as ef) } as tf) ->
						{e with eexpr = TFunction ({tf with tf_expr = {ef with eexpr = TBlock(einit :: el @ [ereturn])}})}
					| _ -> assert false
				in
				cf.cf_expr <- Some e;
				cf.cf_type <- TFun(args, monofy_class c);
				DynArray.add methods cf
			| _ -> ()
	end;

	ctx.buf <- ctx.buf_c;

	(* generate function implementations *)
	if not (DynArray.empty methods) then begin
		DynArray.iter (fun cf ->
			generate_method ctx c cf;
		) methods;
	end;

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
	end;

	(* generate static vars *)
	if not (DynArray.empty svars) then begin
		spr ctx "// static vars\n";
		DynArray.iter (fun cf ->
			print ctx "%s %s" (s_type ctx cf.cf_type) (full_field_name c cf);
			match cf.cf_expr with
			| None -> newline ctx
			| Some e ->
				spr ctx " = ";
				generate_expr ctx e;
				newline ctx
		) svars;
	end;

	(* generate forward declarations of functions *)
	if not (DynArray.empty methods) then begin
		spr ctx "// forward declarations\n";
		DynArray.iter (fun cf ->
			generate_function_header ctx c cf;
			newline ctx;
		) methods;
	end;

	(* check if we have the main class *)
	match ctx.con.com.main_class with
	| Some path when path = c.cl_path ->
		print ctx "int main() {\n\tGC_INIT();\n\t%s();\n}" (full_field_name c (PMap.find "main" c.cl_statics))
	| _ -> ()

let generate_type con mt = match mt with
	| TClassDecl c ->
		let ctx = mk_type_context con c.cl_path in
		ctx.curclass <- c;
		generate_class ctx c;
		close_type_context ctx;
	| _ ->
		()

let generate com =
	let con = {
		com = com;
		cvar = alloc_var "__c" t_dynamic;
		num_temp_funcs = 0;
	} in
	List.iter (generate_type con) com.types