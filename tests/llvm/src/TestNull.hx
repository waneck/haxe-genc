private class MyThing<T> {
	var t:T;
	public function new(t:T) {
		this.t = t;
	}

	public function getT() {
		return t;
	}
}

@:build(TestHelper.build())
class TestNull {
	static function testBasic() {
		var a:Null<Int> = 0;
		true == (a == 0);
		false == (a == null);
		a = null;
		false == (a == 0);
		true == (a == null);
	}

	static function testNullArg() {
		1 == nullArg();
		1 == nullArg(null);
		12 == nullArg(12);

		var n:Null<Int> = null;
		1 == nullArg(n);
		n = 12;
		12 == nullArg(n);
	}

	static function nullArg(?a:Int) {
		if (a == null) {
			a = 1;
		}
		return a;
	}

	static function testTypeParameter() {
		var a = new MyThing(12);
		//12 == a.getT();
//
		//var b = new MyThing(true);
		//true == b.getT();
//
		//var c = new MyThing(false);
		//false == c.getT();

		var d = new MyThing<Null<Int>>(null);
		//null == d.getT(); // TODO: TestHelper problem
	}

	static function nullBoolString(b:Null<Bool>) {
		if (b == null) {
			return "null";
		} else if (b == false) {
			return "false";
		} else if (b == true) {
			return "true";
		} else {
			return "wtf";
		}
	}

	static function testBool() {
		"null" == nullBoolString(null);
		"false" == nullBoolString(false);
		"true" == nullBoolString(true);

		var n:Null<Bool> = null;
		"null" == nullBoolString(n);
		n = false;
		"false" == nullBoolString(n);
		n = true;
		"true" == nullBoolString(n);
	}

	static function nullFloatString(f:Null<Float>) {
		if (f == null) {
			return "null";
		} else if (f < 0) {
			return "<";
		} else if (f > 0) {
			return ">";
		} else if (f == 0) {
			return "==";
		} else {
			return "wtf";
		}
	}

	static function testFloat() {
		"null" == nullIntString(null);
		"<" == nullFloatString(-1.);
		">" == nullFloatString(1.);
		"==" == nullFloatString(0.);

		var n:Null<Float> = null;
		"null" == nullFloatString(n);
		n = -1.;
		"<" == nullFloatString(n);
		n = 1.;
		">" == nullFloatString(n);
		n = 0.;
		"==" == nullFloatString(n);
	}

	static function nullIntString(i:Null<Int>) {
		if (i == null) {
			return "null";
		} else if (i < 0) {
			return "<";
		} else if (i > 0) {
			return ">";
		} else if (i == 0) {
			return "==";
		} else {
			return "wtf";
		}
	}

	static function testInt() {
		"null" == nullIntString(null);
		"<" == nullIntString(-1);
		">" == nullIntString(1);
		"==" == nullIntString(0);

		var n:Null<Int> = null;
		"null" == nullIntString(n);
		n = -1;
		"<" == nullIntString(n);
		n = 1;
		">" == nullIntString(n);
		n = 0;
		"==" == nullIntString(n);

		"null" == nullFloatString(null);
		"<" == nullFloatString(-1);
		">" == nullFloatString(1);
		"==" == nullFloatString(0);

		var n:Null<Int> = null;
		"null" == nullFloatString(n);
		n = -1;
		"<" == nullFloatString(n);
		n = 1;
		">" == nullFloatString(n);
		n = 0;
		"==" == nullFloatString(n);
	}
}