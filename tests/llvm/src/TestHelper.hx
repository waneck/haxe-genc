import haxe.macro.Expr;
import haxe.macro.Context;

using StringTools;
using haxe.macro.Tools;

class TestHelper {
	macro static function build():Array<Field> {
		var fields = Context.getBuildFields();
		var testFields = [];
		var c = Context.getLocalClass().toString();
		for (field in fields) {
			if (!field.name.startsWith("test")) {
				continue;
			}
			switch (field.kind) {
				case FFun(f) if (f.expr != null && field.name.startsWith("test")):
					testFields.push(field);
					var el = [];
					var s = "Running " + c + "." + field.name;
					el.push(macro c.CStdio.puts($v{s}));
					switch (f.expr.expr) {
						case EBlock(el2):
							for (e in el2) {
								el.push(transform(e));
							}
						case _:
							throw 'Something went wrong';
					}
					f.expr = macro $b{el};
				case _:
			}
		}
		var callExprs = testFields.map(function(field) return macro $i{field.name}());
		var ctor = (macro class X { static public function test() $b{callExprs}; }).fields[0];
		fields.push(ctor);
		return fields;
	}

	static function isFloatExpr(e:Expr) {
		return switch (e.expr) {
			case EConst(CFloat(_)): true;
			case EUnop(_, _, {expr: EConst(CFloat(_))}): true;
			case _: false;
		}
	}

	static function isBoolExpr(e:Expr) {
		return switch (e.expr) {
			case EConst(CIdent("true" | "false")): true;
			case EUnop(_, _, {expr: EConst(CIdent("true" | "false"))}): true;
			case _: false;
		}
	}

	static function isStringExpr(e:Expr) {
		return switch (e.expr) {
			case EConst(CString(_)): true;
			case _: false;
		}
	}

	static function transform(e:Expr) {
		return switch (e) {
			case macro $e1 == $e2:
				if (isFloatExpr(e1) || isFloatExpr(e2)) {
					macro @:pos(e.pos) Eq.eqFloat($e1, $e2);
				} else if (isBoolExpr(e1) || isBoolExpr(e2)){
					macro @:pos(e.pos) Eq.eqBool($e1, $e2);
				} else if (isStringExpr(e1) || isStringExpr(e2)) {
					macro @:pos(e.pos) Eq.eqString($e1, $e2);
				} else {
					macro @:pos(e.pos) Eq.eqInt($e1, $e2);
				}
			case _:
				e;
		}
	}
}