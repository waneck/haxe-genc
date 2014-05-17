package ;

enum TestEnum {
	E1;
	E2;
	E3;
}

class TestAnalyzer {
	#if static_analyzer

	@:js('1;')
	static function testConstPropagation() {
		var a = 1;
		var b = a;
		b;
	}

	@:js('2;')
	static function testConstPropagation2() {
		var a = 1;
		a = 2;
		var b = a;
		b;
	}

	@:js('
		var a = 1;
		if(Math.random() > 0.5) {
			a = 2;
		}
		a;
	')
	static function testCond() {
		var a = 1;
		if (Math.random() > 0.5) {
			a = 2;
		}
		a; // a is unknown here (could be 1 or 2)
	}

	@:js('
		if(Math.random() > 0.5) {
			TestAnalyzer.sideEffect(1);
			TestAnalyzer.sideEffect(2);
			1;
		}
		1;
	')
	static function testCond2() {
		var a = 1;
		if (Math.random() > 0.5) {
			sideEffect(a);
			a = 2;
			sideEffect(a);
			a = 1;
		}
		a;
	}

	@:js('
		if(Math.random() > 0.5) {
			TestAnalyzer.sideEffect(1);
			1;
		} else if(Math.random() > 0.5) {
			TestAnalyzer.sideEffect(1);
			1;
		} else {
			TestAnalyzer.sideEffect(1);
		}
		1;
	')
	static function testCond3() {
		var a = 1;
		if (Math.random() > 0.5) {
			sideEffect(a);
			a = 1;
		} else if (Math.random() > 0.5) {
			sideEffect(a);
			a = 1;
		} else {
			sideEffect(a);
		}
		a;
	}

	@:js('
		var a = 1;
		if(Math.random() > 0.5) {
			TestAnalyzer.sideEffect(1);
			a = 2;
		} else if(Math.random() > 0.5) {
			a = 2;
			TestAnalyzer.sideEffect(2);
		} else {
			TestAnalyzer.sideEffect(1);
		}
		a;
	')
	static function testCond4() {
		var a = 1;
		if (Math.random() > 0.5) {
			sideEffect(a);
			a = 2;
		} else if (Math.random() > 0.5) {
			a = 2;
			sideEffect(2);
		} else {
			sideEffect(a);
		}
		a;
	}

	@:js('
		var b = null;
		if(Math.random() > 0.5) {

		} else if(Math.random() > 0.5) {
			b = "foo";
		} else {
			b = "foo";
		}
		b;
	')
	static function testCond5() {
		var b = null;
		if (Math.random() > 0.5) {

		} else if (Math.random() > 0.5) {
			b = "foo";
		} else {
			b = "foo";
		}
		b;
	}

	@:js('
		9 + "" + 6;
	')
	static function testCond6() {
		var a = 9;
		var b = a - 3;
		if (b < 4) {
			sideEffect(b);
		} else {
			b = 6;
		}
		a + "" + b;
	}

	@:js('
		3 + "" + 9 + "" + 9;
	')
	static function testAssignOp() {
		var a = 1;
		var b = a += 2;
		var c = b *= 3;
		a + "" + b + "" + c;
	}

	@:js('
		var a = TestAnalyzer.sideEffect(1);
		a += 2;
	')
	static function testAssignOp2() {
		var a = sideEffect(1);
		a += 2;
	}

	@:js('
		var a = 1;
		a = 3;
		a = 3 + TestAnalyzer.sideEffect(2);
		a + "" + 2;
	')
	static function testAssignOp3() {
		var a = 1;
		var b = 2;
		a += b;
		a += sideEffect(b);
		a + "" + b;
	}

	@:js('
		var a = 1;
		var b = a;
		while(Math.random() > 0.5) {
			var c = b;
			var d = a;
		}
		a + b;
	')
	static function testLoopCancel() {
		var a = 1;
		var b = a;
		while(Math.random() > 0.5) {
			var c = b;
			var d = a;
		}
		a + b;
	}

	@:js('44;')
	static function testUnop() {
		var a = 12;
		var b = a--; // 12, a = 11
		var c = --a; // 10, a = 10
		var d = a++; // 10, a = 11
		var e = ++a; // 12, a = 12
		var f = -a; // -12, a = 12
		a + b + c + d + e + f;
	}

	@:js('
		if(1 == Math.random()) {
			1;
		} else {
			1;
		}
		2;
	')
	static function testIfAssignment() {
		var a;
		var b;
		if ((a = 1) == Math.random()) {
			b = a;
		} else {
			b = 1;
		}
		a + b;
	}

	@:js('
		var _g = TestAnalyzer.sideEffect(1);
		switch(_g) {
			case 0:
				0;
				break;
			default:
				0;
		}
		0;
	')
	static function testSwitch() {
		var a = 1;
		switch(sideEffect(1)) {
			case 0: a = 0;
			default: a = 0;
		}
		a;
	}

	@:js('
		var a = 1;
		var _g = TestAnalyzer.sideEffect(1);
		switch(_g) {
			case 0:
				a = 0;
				break;
		}
		a;
	')
	static function testSwitch2() {
		var a = 1;
		switch(sideEffect(1)) {
			case 0: a = 0;
		}
		a;
	}

	@:js('
		var _g = TestAnalyzer.sideEffect(1);
		switch(_g) {
			case 0:
				1;
				break;
		}
		1;
	')
	static function testSwitch3() {
		var a = 1;
		switch(sideEffect(1)) {
			case 0: a = 1;
		}
		a;
	}

	@:js('
		var _g = TestEnum.E1;
		switch(_g[1]) {
			case 0:
				2;
				break;
			case 1:
				2;
				break;
			case 2:
				2;
				break;
			}
		2;
	')
	static function testSwitch4() {
		var a = 1;
		switch(E1) {
			case E1: a = 2;
			case E2: a = 2;
			case E3: a = 2;
		}
		a;
	}
	static function sideEffect(x:Dynamic) { return x; }

	#end
}