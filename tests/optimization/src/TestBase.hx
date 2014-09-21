package ;

class TestBase {

	var numTests:Int;
	var numFailures:Int;

	function new() {
		numTests = 0;
		numFailures = 0;
	}

	@:generic
	function assertEquals<T>(expected:T, actual:T, ?p:haxe.PosInfos) {
		++numTests;
		if (expected != actual) {
			++numFailures;
			trace(p.lineNumber);
			haxe.Log.trace(actual);
			haxe.Log.trace("should be");
			haxe.Log.trace(expected);
		}
	}

	function setup() { }
}