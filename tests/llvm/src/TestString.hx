@:build(TestHelper.build())
class TestString {
	static function testBasic() {
		var s = "foo";
		"foo" == s;
		"foo" == "foo";
	}

	static function testConcat() {
		var s = "foo";
		"foobar" == s + "bar";
		"foobar" == "foo" + "bar";
	}
}