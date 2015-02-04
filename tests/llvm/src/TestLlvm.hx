@:build(TestHelper.build())
class TestLlvm {
	static function testEval() {
		var z = 0;
		z++ == 0;
		z == 1;
		++z == 2;
		z++;
		z == 3;
		++z;
		z == 4;

		(z += 3) == 7;

		var x = 0;
		var arr = [3];
		arr[x++]++ == 3;
		x == 1;
		arr[0] == 4;
		x = 0;
		(arr[x++] += 3) == 7;
		arr[0] == 7;

		var a = [0];
		a[0] += 1;
		a[0] == 1;
		a[0] += a[0] + 1;
		a[0] == 3;
	}

	static function testArithmeticOps() {
		1 == 1 + 0;
		2 == 1 + 1;
		0 == 0 + 0;
		4 == 8 - 4;
		6 == 2 * 3;
		7 == 4 + 3;
		0 == 4 - 4;
		0 == 3 * 0;
		4 == 4 * 1;
		1.5 == 3 / 2;
		2.0 == 4 / 2;
		1.0 == 4 / 4;
		0 == (3 + 4) * 0;
		5 == 2 * 2 + 1;
		4 == 3 + (4 - 3);
		2.0 == 2 / (2 - 1);
		1. == (1 + 1) / (3 - 1);
		2. == (1+1) / 2 + 1;
		-3 == 0 - 3;
		-3 == -5 + 2;
		-7 == -5 + -2;
		0 == -5 + 5;
		2.0 == -4 / -2;
		0 == -1 * 0;
		1 == -1 + 2;
		1 == (0 + -1) + 2;
		-3 == -3 + 0;
		-3 == -3 * 1;
		3 == -3 * -1;
		-2.0 == -4. / 2.;
		2 == -(1 + 1) + 4;
		2.0 == -(3 - 1) / -(3 - 2);
		-1.0 == -(3 - 2) / (3 - 2);
		6 == -(1 * 3) * -(1 + 1);
		-6 == -((1 + 2) * (1 + 1));
		2 == 8 % 6;
		2 == 8 % 3;
		0 == 8 % 2;
		0 == 0 % 2;
		0 == 8 % 1;
	}

	static function testBoolOps() {
		true == (true || false);
		true == (false || true);
		true == (true || true);
		false == (false || false);

		true == (true && true);
		false == (true && false);
		false == (false && true);
		false == (false && false);
	}

	static var called = false;

	static function testBoolOpsShortCircuit() {
		true == (true || getBool());
		called == false;
		false == (false && getBool());
		called == false;
	}

	static function getBool() {
		called = true;
		return false;
	}

	static function testShiftOps() {
		4 == 2 << 1;
		8 == 2 << 2;
		16 == 1 << 4;
		16 == 16 << 0;
		0 == 0 << 2;

		2 == 8 >> 2;
	}

	static function testBreak() {
		var a = 0;
		while (true) {
			if (a == 0) {
				break;
			}
			a = 1;
		}
		a == 0;
	}

	static function doSwitch(val:Int) {
		return switch (val) {
			case 1: 100;
			case 2: 99;
			case 3: 98;
			default: 97;
		}
	}

	static function testSwitch() {
		100 == doSwitch(1);
		99 == doSwitch(2);
		98 == doSwitch(3);
		97 == doSwitch(0);
	}

	static function doIf(val:Int) {
		if (val == 0) {
			return 2;
		} else if (val == 1) {
			return 1;
		} else {
			if (val < 0) {
				return 0;
			}
		}
		return -1;
	}

	static function testIf() {
		2 == doIf(0);
		1 == doIf(1);
		0 == doIf(-1);
		-1 == doIf(2);
	}

	static function testClosure() {
		var x = 1;
		function add(y) {
			return y + x;
		}
		3 == add(2);

		var add2 = add;
		4 == add2(3);

		function write(k) {
			x = k;
		}
		1 == x;
		write(12);
		12 == x;
	}

	static function testStaticClosure() {
		var f = doSwitch;
		100 == f(1);
		99 == f(2);
		98 == f(3);
		97 == f(0);
	}

	var x:Int;

	function new(x:Int) {
		this.x = x;
	}

	function addToX(y:Int) {
		return x + y;
	}

	static function testMemberClosure() {
		var m = new TestLlvm(12);
		var f = m.addToX;
		var i = 1;
		13 == f(i);
		14 == f(++i);
		2 == i;
	}

	static function testTyperef() {
		//var m = Type.createEmptyInstance(TestLlvm);
		//m.x = 12;
		//12 == m.x;

		var tref = new c.TypeReference<TestLlvm>();
		var m = (cast tref.constructor:c.Pointer.FunctionPointer<Int->TestLlvm>)(12);
		12 == m.x;

		var ctor:c.Pointer.FunctionPointer<Int->TestLlvm> = cast tref.constructor;
		var n = ctor(12);
		12 == m.x;
		12 == ctor(12).x;
	}

	static function testVTable() {
		var d = new D(4);
		4 == d.a();
		4 == d.b();
		4 == d.c();
		4 == d.d();

		var d:A = new D(4);
		4 == d.a();
		4 == d.b();

		var c = new C(3);
		3 == c.a();
		3 == c.b();
		3 == c.c();

		var e = new E();
		5 == e.b();

		var e:B = new E();
		5 == e.b();
	}
}

@:keep
class A {
	var id:Int;
	public function new(id) this.id = id;
	public function a() return id;
	public function b() return id;
}

class B extends A {
	public function new(id) super(id);
	public override function b() return id;
	public function c() return id;
}

class C extends B {
	public function new(id) super(id);
	public override function b() return id;
}

class D extends C {
	public function new(id) super(id);
	public function d() return id;
}

class E extends D {
	public function new() super(5);
	public override function b() return id;
}