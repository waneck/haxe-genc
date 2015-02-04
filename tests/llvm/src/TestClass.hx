@:build(TestHelper.build())
class TestClass {

	static var a = [1, 2, 3];
	static var f = function(i) {
		return a[i];
	}

	static function testStaticInit() {
		1 == a[0];
		2 == a[1];
		3 == a[2];

		1 == f(0);
		2 == f(1);
		3 == f(2);
	}

	static dynamic function staticDynamic(i:Int) {
		return 12 + i;
	}

	static function returnsInt(i:Int) {
		return i * 3;
	}

	static function testStaticDynamic() {
		var cur = staticDynamic;
		13 == staticDynamic(1);
		staticDynamic = function(i) {
			return 99 - i;
		}
		90 == staticDynamic(9);
		staticDynamic = returnsInt;
		27 == returnsInt(9);
		staticDynamic = cur;
		14 == staticDynamic(2);
	}

	var s:String;

	dynamic function memberDynamic(s2:String) {
		return s + " " + s2;
	}

	function new(s:String) {
		this.s = s;
	}

	function returnsString(s2:String) {
		return s + " " + s2 + " :D";
	}

	static function testMemberDynamic() {
		var m = new TestClass("O_O");
		var cur = m.memberDynamic;
		"O_O ^_^" == m.memberDynamic("^_^");
		m.memberDynamic = function(s2) {
			return s2 + " " +m.s;
		}
		"^_^ O_O" == m.memberDynamic("^_^");
		m.memberDynamic = m.returnsString;
		"O_O ^_^ :D" == m.memberDynamic("^_^");
		m.memberDynamic = cur;
		"O_O ^_^" == m.memberDynamic("^_^");
	}
}