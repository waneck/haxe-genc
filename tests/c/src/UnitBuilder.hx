/*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package ;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using StringTools;
using haxe.macro.TypeTools;

class UnitBuilder {
	static var isTrue = macro Main.isTrue;
	static var isFalse = macro Main.isFalse;
	static var equals = macro Main.equals;
	static var equalsNot = macro Main.equalsNot;
	static var equalsFloat = macro Main.equalsFloat;

	static public macro function build(basePath:String):Array<Field> {
		var ret = Context.getBuildFields();
		var calls = [];
		var numFiles = 0;

		function readDir(path) {
			var dir = sys.FileSystem.readDirectory(path);
			path = path.endsWith("\\") || path.endsWith("/") ? path : path + "/";
			for (file in dir) {
				var filePath = path + file;
				var pos = Context.makePosition( { min:0, max:0, file:filePath } );
				if (file.endsWith(".unit.hx")) {
					numFiles++;
					var cl = macro class A {
						static public function run() @:pos(pos) ${read(filePath)}
					}
					var name = ~/\./g.map(file, function(_) return "_");
					cl.name = name;
					cl.pack = ["unit"];
					Context.defineType(cl);
					calls.push(macro unit.$name.run());
				} else if (sys.FileSystem.isDirectory(filePath)) {
					readDir(filePath);
				}
			}
		}
		readDir(basePath);
		var m = {
			name: "new",
			kind: FFun({
				args: [],
				ret: null,
				params: [],
				expr: macro $b{calls}
			}),
			pos:Context.currentPos(),
			access: [],
			doc: null,
			meta: []
		}
		ret.push(m);
		return ret;
	}

	#if macro
	static function collapseToOrExpr(el:Array<Expr>) {
		return switch(el) {
			case []: throw "";
			case [e]: e;
		case _:
			var e = el.pop();
			{ expr: EBinop(OpBoolOr, e, collapseToOrExpr(el)), pos: e.pos }
		}
	}

	static function mkEq(e1, e2, p) {
		function isFloat(e) {
			try return switch(Context.follow(Context.typeof(e))) {
				case TAbstract(tr, _):
					tr.get().name == "Float";
				case _:
					false;
			} catch (e:Dynamic) {
				return false;
			}
		}
		var e = switch [isFloat(e1) || isFloat(e2), e2.expr] {
			case [_, EField( { expr:EConst(CIdent("Math" | "math")) }, "POSITIVE_INFINITY" | "NEGATIVE_INFINITY")] if (Context.defined("cpp") || Context.defined("php")):
				macro $isTrue($e1 == $e2);
			case [true, _]:
				macro $equalsFloat($e1, $e2);
			case _:
				macro $equals($e1, $e2);
		}
		return {
			expr: e.expr,
			pos: p
		}
	}
	static public function read(path:String) {
		var p = Context.makePosition( { min:0, max:0, file:path } );
		var file = sys.io.File.getContent(path);
		var code = Context.parseInlineString("{" + file + "}", p);
		function mkBlock(e:Expr) {
			return switch(e.expr) {
				case EBlock(b): b;
				case _: [e];
			}
		}
		function bl(block:Array<Expr>):Array<Expr> {
			var ret = [];
			for (e in block) {
				var e = switch(e.expr) {
					case EBinop(OpEq, e1, { expr: EConst(CIdent("false")) } )
					| EBinop(OpEq, { expr: EConst(CIdent("false")) }, e1):
						{
							expr: (macro $isFalse($e1)).expr,
							pos: e.pos
						}
					case EBinop(OpEq, e1, { expr: EConst(CIdent("true")) } )
					| EBinop(OpEq, { expr: EConst(CIdent("true")) }, e1):
						{
							expr: (macro $isTrue($e1)).expr,
							pos: e.pos
						}
					case EBinop(OpEq, e1, { expr: EArrayDecl(el) } )
					| EBinop(OpEq, { expr: EArrayDecl(el) }, e1 ):
						var el2 = [];
						for (i in 0...el.length) {
							var e2 = el[i];
							el2.push(mkEq((macro $e1[$v{i}]), e2, e.pos));
						}
						if (el2.length == 0)
							mkEq((macro @:pos(e1.pos) $e1.length), (macro 0), e.pos);
						else
							macro { $a{el2}; };
					case EBinop(OpEq, e1, e2):
						mkEq(e1, e2, e.pos);
					case EBinop(OpNotEq, e1, e2):
						macro $isTrue($e1 != $e2);
					case EBinop(OpGt | OpGte | OpLt | OpLte, _, _):
						{
							expr: (macro t($e)).expr,
							pos: e.pos
						}
					case EThrow(e):
						macro exc(function() $e);
					case EIn(e1, {expr:EArrayDecl(el) }):
						var el2 = [];
						for (e in el)
							el2.push(macro $e1 == $e);
						macro @:pos(e.pos) t(${ collapseToOrExpr(el2) } );
					case EVars(vl):
						for (v in vl)
							if (v.name == "t" || v.name == "f" || v.name == "eq" || v.name == "neq")
								Context.error('${v.name} is reserved for unit testing', e.pos);
							e;
					case EFor(it, {expr: EBlock(el), pos: p}):
						{ expr: EFor(it, {expr:EBlock(bl(el)), pos: p}), pos: e.pos };
					case _:
						e;
				}
				ret.push(e);
			}
			return ret;
		}
		var block = mkBlock(code);
		return macro $b{bl(block)};
	}
	#end
}