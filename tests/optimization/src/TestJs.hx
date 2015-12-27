private enum Tree<T> {
	Node(l:Tree<T>, r:Tree<T>);
	Leaf(v:T);
}

private enum Some {
    one(s1:String);
    pair(s1:String, s2:String);
    triad(s1:String, s2:String, s3:String);
}

class Inl{
	public var x:Float;
	public inline function new(f){
		x = f;
	}
}

private enum EnumFlagTest {
	EA;
	EB;
	EC;
}

@:analyzer(code_motion)
class TestJs {
	//@:js('var x = 10;"" + x;var x1 = 10;"" + x1;var x2 = 10.0;"" + x2;var x3 = "10";x3;var x4 = true;"" + x4;')
	//static function testStdString() {
	//var x = 10;
	//Std.string(x);
	//var x:UInt = 10;
	//Std.string(x);
	//var x = 10.0;
	//Std.string(x);
	//var x = "10";
	//Std.string(x);
	//var x = true;
	//Std.string(x);
	//}

	@:js("var a = new List();var _g_head = a.h;while(_g_head != null) _g_head = _g_head.next;")
	static function testListIteratorInline() {
		var a = new List();
		for (v in a) { }
	}

	@:js("var a = 1;var tmp;var v2 = a;tmp = a + v2;tmp;")
	@:analyzer(no_const_propagation)
	@:analyzer(no_copy_propagation)
	@:analyzer(no_check_has_effect)
	@:analyzer(no_local_dce)
	static function testInlineWithArgumentUsedMoreThanOnce() {
		var a = 1;
		if (_inlineWithArgumentUsedMoreThanOnce(a) > 0) { }
	}

	inline static function _inlineWithArgumentUsedMoreThanOnce(v) {
		var v2 = v;
		return v + v2;
	}

	@:js("var a = [];a;")
	@:analyzer(no_check_has_effect)
	@:analyzer(no_local_dce)
	static function testInlineWithComplexExpr() {
		var a = [];
		if (_inlineWithComplexExpr(a, 0)) {}
	}

	inline static function _inlineWithComplexExpr(a, i) {
		return try a[i] catch (e:Dynamic) null;
	}

	@:js("var a = { v : [{ b : 1}]};a;var tmp;switch(a.v.length) {case 1:switch(a.v[0].b) {case 1:tmp = true;break;default:tmp = false;}break;default:tmp = false;}tmp;")
	@:analyzer(no_const_propagation, no_local_dce, no_check_has_effect)
	static function testDeepMatchingWithoutClosures() {
		var a = {v: [{b: 1}]};
		a;
		if (switch (a) {
			case {v: [{b: 1}]}: true;
			default: false;
		}) {}
	}

	@:js("var a = [1,2,3];var _g = 0;while(_g < a.length) {var v = a[_g];++_g;console.log(v + 2);}")
	static function testInlineFunctionWithAnonymousCallback() {
		var a = [1,2,3];
		inline function forEach(f) for (v in a) f(v);
		forEach(function(x) trace(x + 2));
	}

	@:js('var a = "";var e;var __ex0 = a;var _g = __ex0.toLowerCase();switch(_g) {case "e":e = 0;break;default:throw new Error();}')
	@:analyzer(no_const_propagation, no_local_dce, no_copy_propagation)
	static function testRValueSwitchWithExtractors() {
		var a = "";
		var e = switch (a) {
			case _.toLowerCase() => "e": 0;
			default: throw new js.Error();
		}
	}

	@:js('console.log("1" + "2" + "3" + "4");')
	static function testEnumValuePropagation1() {
		var n = Node(Node(Leaf("1"), Node(Leaf("2"), Leaf("3"))), Leaf("4"));
		switch (n) {
			case Node(Node(Leaf(s1), Node(Leaf(s2), Leaf(s3))), Leaf(s4)):
				trace(s1 + s2 + s3 + s4);
			case _:
		}
	}

	@:js('')
	static function testEnumValuePropagation2() {
		var v = pair("foo", "bar");
		var x = switch (v) {
			case one(s1): verify(s1);
			case pair(s1, s2): verify(s1) && verify(s2);
			case triad(s1, s2, s3): verify(s1) && verify(s2) && verify(s3);
		}
	}

	static inline function verify(s1) return s1 == "foo";

	@:js('
		var object = { \'hello\' : "world"};
		TestJs["use"](object);
	')
	static function testQuotedStructureFields1() {
		var object = {
			"hello": "world"
		}
		use(object);
	}

	@:js('
		var object = { \'hello\' : "world", world : "hello", \'another\' : "quote"};
		TestJs["use"](object);
	')
	static function testQuotedStructureFields2() {
		var object = {
			'hello': "world",
			world: "hello",
			"another": "quote"
		}
		use(object);
	}

	@:analyzer(no_local_dce)
	@:js('var v0_x = 0;var v1_x = 1;var vRand_x = Math.random();')
	// #4558
	static function testInlineFunctionNew() {
		var v0 = new Inl(0);
		var v1 = new Inl(1);
		var vRand = new Inl(Math.random());
	}

	@:js("try {throw new js__$Boot_HaxeError(false);} catch( e ) {}")
	static function testNoHaxeErrorUnwrappingWhenNotRequired() {
		try throw false catch (e:Dynamic) {}
	}

	@:js("try {throw new js__$Boot_HaxeError(false);} catch( e ) {if (e instanceof js__$Boot_HaxeError) e = e.val;console.log(e);}")
	static function testHaxeErrorUnwrappingWhenUsed() {
		try throw false catch (e:Dynamic) trace(e);
	}

	@:js("try {throw new js__$Boot_HaxeError(false);} catch( e ) {if (e instanceof js__$Boot_HaxeError) e = e.val;if( js_Boot.__instanceof(e,Bool) ) {} else throw(e);}")
	static function testHaxeErrorUnwrappingWhenTypeChecked() {
		try throw false catch (e:Bool) {};
	}


	@:js('TestJs["use"](2);')
	static function testIssue3938() {
		var a = 1;
		if (a == 1) {
			a = 2;
		} else {
			a = 3;
		}
		use(a);
	}

	@:js('
		TestJs["use"](3);
	')
	static function testBinop() {
		var a = 1;
		var b = 2;
		use(a + b);
	}

	@:js('
		TestJs["use"](false);
		TestJs["use"](true);
		TestJs["use"](true);
		TestJs["use"](true);
		TestJs["use"](false);
		TestJs["use"](true);
		TestJs["use"](true);
		TestJs["use"](false);
		TestJs["use"](false);
		TestJs["use"](true);
		TestJs["use"](false);
	')
	static function testEnumValueFlags() {
		var flags = new haxe.EnumFlags();
		use(flags.has(EA));
		flags = new haxe.EnumFlags(1);
		use(flags.has(EA));

		// set
		flags.set(EB);
		use(flags.has(EA));
		use(flags.has(EB));
		use(flags.has(EC));

		// unset
		flags.unset(EC);
		use(flags.has(EA));
		use(flags.has(EB));
		use(flags.has(EC));
		flags.unset(EA);
		use(flags.has(EA));
		use(flags.has(EB));
		use(flags.has(EC));
	}

	@:js('
		var map = new haxe_ds_StringMap();
		var tmp;
		if(__map_reserved.some != null) map.setReserved("some",2); else map.h["some"] = 2;
		tmp = 2;
		var i = tmp;
		TestJs["use"](i);
	')
	static function testIssue4731() {
        var map = new Map();
        var i = map["some"] = 2;
		use(i);
		// This is not const-propagated because StringMap introduced unbound variables
	}

	@:js('
		var x = TestJs.getInt();
		var tmp;
		TestJs.getInt();
		tmp = TestJs.getInt();
		TestJs.call([x,"foo"],tmp);
	')
	static function testMightBeAffected1() {
		var x = getInt();
		call([x, "foo"], {
			getInt();
			getInt();
		});
	}

	@:js('
		var x = TestJs.getInt();
		var tmp = [x,"foo"];
		var tmp1;
		x = TestJs.getInt();
		tmp1 = TestJs.getInt();
		TestJs.call(tmp,tmp1);
	')
	static function testMightBeAffected2() {
		var x = getInt();
		call([x, "foo"], {
			x = getInt();
			getInt();
		});
	}

	@:js('
		var x = TestJs.getInt();
		var tmp = x;
		var tmp1;
		++x;
		tmp1 = TestJs.getInt();
		TestJs.call(tmp,tmp1);
	')
	static function testMightBeAffected3() {
		var x = getInt();
		call(x, {
			x++;
			getInt();
		});
	}

	@:js('
		var a = 0;
		if(Math.random() < 0.5) a = 2;
		var b = "";
		if(Math.random() < 0.5) b = "hello";
		TestJs["use"](a);
		TestJs["use"](b);
	')
	static function testIssue4739() {
		var a = 0;
		if (Math.random() < 0.5)
			a += 2;

		var b = "";
		if (Math.random() < 0.5)
			b = b + "hello";

		use(a);
		use(b);
	}

	@:js('
		var a = TestJs.getInt();
		var b = TestJs.getInt();
		var x;
		var tmp = a + b;
		while(a != b) {
			x = tmp;
			TestJs["use"](x);
		}
	')
	static function testCodeMotion1() {
		var a = getInt();
		var b = getInt();
		var x;
		while (a != b) {
			x = a + b;
			use(x);
		}
	}

	@:js('
		var a = TestJs.getInt();
		var b = TestJs.getInt();
		var x = 0;
		while(a != b) {
			x = a + x;
			TestJs["use"](x);
		}
	')
	static function testCodeMotion2() {
		var a = getInt();
		var b = getInt();
		var x = 0;
		while (a != b) {
			x = a + x;
			use(x);
		}
	}

	@:js('
		var a = TestJs.getInt();
		var b = TestJs.getInt();
		var x;
		while(a != b) {
			var tmp = a + b;
			while(a != b) {
				x = tmp;
				TestJs["use"](x);
			}
		}
	')
	static function testCodeMotion3() {
		var a = getInt();
		var b = getInt();
		var x;
		while (a != b) {
			while (a != b) {
				x = a + b;
				use(x);
			}
		}
	}

	@:js('
		var a = TestJs.getInt();
		var b = TestJs.getInt();
		var x;
		var tmp = a + b + b;
		while(a != b) {
			x = tmp;
			TestJs["use"](x);
		}
	')
	static function testCodeMotion4() {
		var a = getInt();
		var b = getInt();
		var x;
		while (a != b) {
			x = a + b + b;
			use(x);
		}
	}

	@:js('
		var a = TestJs.getInt();
		var b = TestJs.getInt();
		var x;
		var tmp = a + b;
		while(a != b) {
			x = tmp;
			TestJs["use"](x);
			TestJs["use"](x);
		}
	')
	static function testCodeMotion5() {
		var a = getInt();
		var b = getInt();
		var x;
		while (a != b) {
			x = a + b;
			use(x);
			use(x);
		}
	}

	@:js('
		var a = TestJs.getInt();
		TestJs["use"](a);
	')
	static function testCopyPropagation1() {
		var a = getInt();
		var b = a;
		use(b);
	}

	@:js('
		var a = TestJs.getInt();
		var b = a;
		a = TestJs.getInt();
		TestJs["use"](b);
	')
	static function testCopyPropagation2() {
		var a = getInt();
		var b = a;
		a = getInt();
		use(b);
	}

	@:js('
		var b = TestJs.getInt();
		TestJs["use"](b);
	')
	static function testCopyPropagation3() {
		var a;
		{
			var b = getInt();
			a = b;
		}
		use(a);
	}

	@:js('
		TestJs.getInt();
		if(TestJs.getInt() != 0) throw new js__$Boot_HaxeError("meh");
	')
	static function testIfInvert() {
		var tmp;
		var tmp2 = getInt();
		if (getInt() == 0) {
			tmp = tmp2;
		} else {
			throw "meh";
		}
	}

	@:js('
		var d1 = TestJs.intField;
		TestJs.call(TestJs["use"](null),d1);
	')
	static function testInlineRebuilding1() {
		inlineCall(intField, use(null));
	}

	@:js('
		TestJs.call(TestJs["use"](null),TestJs.stringField);
	')
	static function testInlineRebuilding2() {
		inlineCall(stringField, use(null));
	}

	@:js('
		var a = TestJs.getArray();
		var d1 = a[0]++;
		TestJs.call(a[1]++,d1);
	')
	static function testInlineRebuilding3() {
		var a = getArray();
		inlineCall(a[0]++, a[1]++);
	}

	@:js('
		var a = TestJs.getArray();
		var d1 = a[0] = 1;
		TestJs.call(a[1] = 1,d1);
	')
	static function testInlineRebuilding4() {
		var a = getArray();
		inlineCall(a[0] = 1, a[1] = 1);
	}

	@:js('
		var a = TestJs.getArray();
		var d1 = a[0] += 1;
		TestJs.call(a[1] += 1,d1);
	')
	static function testInlineRebuilding5() {
		var a = getArray();
		inlineCall(a[0] += 1, a[1] += 1);
	}

	@:js('
		var d1 = TestJs.call(1,2);
		TestJs.call(TestJs.call(3,4),d1);
	')
	static function testInlineRebuilding6() {
		inlineCall(call(1, 2), call(3, 4));
	}

	@:js('
		var d1;
		var d11 = TestJs.call(1,2);
		d1 = TestJs.call(TestJs.call(3,4),d11);
		var d2;
		var d12 = TestJs.call(5,6);
		d2 = TestJs.call(TestJs.call(7,8),d12);
		TestJs.call(d2,d1);
	')
	static function testInlineRebuilding7() {
		inlineCall(inlineCall(call(1, 2), call(3, 4)), inlineCall(call(5, 6), call(7, 8)));
	}

	@:js('
		var d1;
		var d11 = TestJs.call(1,2);
		d1 = TestJs.call(TestJs.intField,d11);
		var d2;
		var d12 = TestJs.intField;
		d2 = TestJs.call(TestJs.call(5,6),d12);
		TestJs.call(d2,d1);
	')
	static function testInlineRebuilding8() {
		inlineCall(inlineCall(call(1, 2), intField), inlineCall(intField, call(5, 6)));
	}

	@:js('
		var d1 = TestJs.call(TestJs.stringField,TestJs.call(1,2));
		TestJs.call(TestJs.call(TestJs.call(5,6),TestJs.stringField),d1);
	')
	static function testInlineRebuilding9() {
		inlineCall(inlineCall(call(1, 2), stringField), inlineCall(stringField, call(5, 6)));
	}

	static inline function inlineCall(d1:Dynamic, d2:Dynamic) {
		return call(d2, d1);
	}

	static function getInt(?d:Dynamic) { return 1; }
	static function getArray() { return [0, 1]; }
	static function call(d1:Dynamic, d2:Dynamic) { return d1; }
	static function use<T>(t:T) { return t; }

	static var intField = 12;
	static var stringField = "foo";
}