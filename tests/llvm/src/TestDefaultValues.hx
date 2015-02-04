private typedef TestObject = {
	a:Float,
	b:Float,
	c:Float,
	d:Float,
	e:Float
}

@:build(TestHelper.build())
class TestDefaultValues {

	static function fDefault(a = 1., ?b = 2., c:Float = 3., ?d:Float = 4., ?e:Null<Float> = 5.):TestObject {
		return {
			a: a,
			b: b,
			c: c,
			d: d,
			e: e
		}
	}

	static function objEqual(obj1:TestObject, obj2:TestObject) {
		//c.CStdio.prFloatf("%i %i %i %i %i\n", [obj1.a, obj1.b, obj1.c, obj1.d, obj1.e]);
		//c.CStdio.prFloatf("%i %i %i %i %i\n", [obj2.a, obj2.b, obj2.c, obj2.d, obj2.e]);
		return obj1.a == obj2.a && obj1.b == obj2.b && obj1.c == obj2.c && obj1.d == obj2.d && obj1.e == obj2.e;
	}

	static function testBasic() {
		var nullZeroFloat:Null<Float> = 0.;
		var nullOneFloat:Null<Float> = 1.;
		var nullTwoFloat:Null<Float> = 2.;
		var nullThreeFloat:Null<Float> = 3.;
		var nullFourFloat:Null<Float> = 4.;
		var nullFiveFloat:Null<Float> = 5.;
		var nullNullFloat:Null<Float> = null;

		var nullZeroInt:Null<Int> = 0;
		var nullOneInt:Null<Int> = 1;
		var nullTwoInt:Null<Int> = 2;
		var nullThreeInt:Null<Int> = 3;
		var nullFourInt:Null<Int> = 4;
		var nullFiveInt:Null<Int> = 5;
		var nullNullInt:Null<Int> = null;

		var objRef = { a: 1., b: 2., c: 3., d: 4., e: 5. };

		// 0 args

		true == objEqual(objRef, fDefault());

		// arg 1 (not nullable)

		var objRef2 = { a: 5., b: 2., c: 3., d: 4., e: 5. };
		var objRef3 = { a: 0., b: 2., c: 3., d: 4., e: 5. };

		true == objEqual(objRef2,  fDefault(5));
		true == objEqual(objRef2,  fDefault(5.));
		true == objEqual(objRef2,  fDefault(nullFiveInt));
		true == objEqual(objRef2,  fDefault(nullFiveFloat));
		true == objEqual(objRef3,  fDefault(nullNullInt));
		true == objEqual(objRef3,  fDefault(nullNullFloat));

		// arg 2 (nullable)

		var objRef2 = { a: 1., b: 1., c: 3., d: 4., e: 5. };

		true == objEqual(objRef2,  fDefault(1, 1));
		true == objEqual(objRef2,  fDefault(1, 1.));
		true == objEqual(objRef2,  fDefault(1, nullOneInt));
		true == objEqual(objRef2,  fDefault(1, nullOneFloat));
		true == objEqual(objRef,  fDefault(1, nullNullInt));
		true == objEqual(objRef,  fDefault(1, nullNullFloat));
		true == objEqual(objRef,  fDefault(1, null));

		// arg 3 (not nullable)

		var objRef2 = { a: 1., b: 2., c: 1., d: 4., e: 5. };
		var objRef3 = { a: 1., b: 2., c: 0., d: 4., e: 5. };

		true == objEqual(objRef2,  fDefault(1, 2, 1));
		true == objEqual(objRef2,  fDefault(1, 2, 1.));
		true == objEqual(objRef2,  fDefault(1, 2, nullOneInt));
		true == objEqual(objRef2,  fDefault(1, 2, nullOneFloat));
		true == objEqual(objRef3,  fDefault(1, 2, nullNullInt));
		true == objEqual(objRef3,  fDefault(1, 2, nullNullFloat));

		// arg 4 (nullable)

		var objRef2 = { a: 1., b: 2., c: 3., d: 1., e: 5. };

		true == objEqual(objRef2,  fDefault(1, 2, 3, 1));
		true == objEqual(objRef2,  fDefault(1, 2, 3, 1.));
		true == objEqual(objRef2,  fDefault(1, 2, 3, nullOneInt));
		true == objEqual(objRef2,  fDefault(1, 2, 3, nullOneFloat));
		true == objEqual(objRef,  fDefault(1, 2, 3, nullNullInt));
		true == objEqual(objRef,  fDefault(1, 2, 3, nullNullFloat));

		// arg 5 (nullable)

		var objRef2 = { a: 1., b: 2., c: 3., d: 4., e: 1. };

		true == objEqual(objRef2,  fDefault(1, 2, 3, 4, 1));
		true == objEqual(objRef2,  fDefault(1, 2, 3, 4, 1.));
		true == objEqual(objRef2,  fDefault(1, 2, 3, 4, nullOneInt));
		true == objEqual(objRef2,  fDefault(1, 2, 3, 4, nullOneFloat));
		true == objEqual(objRef,  fDefault(1, 2, 3, nullNullInt));
		true == objEqual(objRef,  fDefault(1, 2, 3, nullNullFloat));
	}
}