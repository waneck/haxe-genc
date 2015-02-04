@:build(TestHelper.build())
class TestClosure {
	static function fFloatToInt(i:Float):Int {
		return cast 1.0 + i;
	}

	static function testIntToFloat() {
		var nullTwoInt:Null<Int> = 2;
		var nullTwoFloat:Null<Float> = 2.0;

		3.0 == fFloatToInt(2);

		var f:Float->Int = fFloatToInt;
		3 == f(2.0);
		3 == f(nullTwoFloat);
		3 == f(nullTwoInt);

		var f:Null<Float>->Int = fFloatToInt;
		3 == f(2.0);
		3 == f(nullTwoFloat);
		3 == f(nullTwoInt);

		var f:Float->Null<Int> = fFloatToInt;
		3 == f(2.0);
		3 == f(nullTwoFloat);
		3 == f(nullTwoInt);

		var f:Null<Float>->Null<Int> = fFloatToInt;
		3 == f(2.0);
		3 == f(nullTwoFloat);
		3 == f(nullTwoInt);

		var f:Int->Float = fFloatToInt;
		3.0 == f(2);
		3.0 == f(nullTwoInt);

		var f:Null<Int>->Float = fFloatToInt;
		3.0 == f(2);
		3.0 == f(nullTwoInt);

		var f:Int->Null<Float> = fFloatToInt;
		3.0 == f(2);
		3.0 == f(nullTwoInt);

		var f:Null<Int>->Null<Float> = fFloatToInt;
		3.0 == f(2);
		3.0 == f(nullTwoInt);
	}

	static function testIntToFloatNullRet() {
		var nullTwoInt:Null<Int> = 2;
		var nullTwoFloat:Null<Float> = 2.0;
		var nullThreeInt:Null<Int> = 3;
		var nullThreeFloat:Null<Float> = 3.0;

		Eq.eqFloat(nullThreeFloat, fFloatToInt(2));

		var f:Float->Int = fFloatToInt;
		nullThreeInt == f(2.0);
		nullThreeInt == f(nullTwoFloat);
		nullThreeInt == f(nullTwoInt);

		var f:Null<Float>->Int = fFloatToInt;
		nullThreeInt == f(2.0);
		nullThreeInt == f(nullTwoFloat);
		nullThreeInt == f(nullTwoInt);

		var f:Float->Null<Int> = fFloatToInt;
		nullThreeInt == f(2.0);
		nullThreeInt == f(nullTwoFloat);
		nullThreeInt == f(nullTwoInt);

		var f:Null<Float>->Null<Int> = fFloatToInt;
		nullThreeInt == f(2.0);
		nullThreeInt == f(nullTwoFloat);
		nullThreeInt == f(nullTwoInt);

		var f:Int->Float = fFloatToInt;
		Eq.eqFloat(nullThreeFloat, f(2));
		Eq.eqFloat(nullThreeFloat, f(nullTwoInt));

		var f:Null<Int>->Float = fFloatToInt;
		Eq.eqFloat(nullThreeFloat, f(2));
		Eq.eqFloat(nullThreeFloat, f(nullTwoInt));

		var f:Int->Null<Float> = fFloatToInt;
		Eq.eqFloat(nullThreeFloat, f(2));
		Eq.eqFloat(nullThreeFloat, f(nullTwoInt));

		var f:Null<Int>->Null<Float> = fFloatToInt;
		Eq.eqFloat(nullThreeFloat, f(2));
		Eq.eqFloat(nullThreeFloat, f(nullTwoInt));
	}

	static function testNested() {
		var s0 = "begin";
		function f1(s1:String) {
			var s2 = "1";
			function f2(s3:String) {
				var s4 = "2";
				function f3(s5:String) {
					return s0 + s1 + s2 + s3 + s4 + s5;
				}
				return f3;
			}
			return f2;
		}
		var s = f1("foo")("bar")("end");
		"beginfoo1bar2end" == s;
	}
}