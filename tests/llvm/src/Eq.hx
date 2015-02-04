import c.CStdio.*;

class Eq {
	static public var numTests = 0;
	static public var numFailures = 0;

	static public function eqBool(expected:Bool, actual:Bool, ?p:haxe.PosInfos) {
		++numTests;
		if (expected != actual) {
			++numFailures;
			printf("%s:%i %i should be %i\n", [p.fileName, p.lineNumber, actual, expected]);
		}
	}

	static public function eqInt(expected:Int, actual:Int, ?p:haxe.PosInfos) {
		++numTests;
		if (expected != actual) {
			++numFailures;
			printf("%s:%i %i should be %i\n", [p.fileName, p.lineNumber, actual, expected]);
		}
	}

	static public function eqFloat(expected:Float, actual:Float, ?p:haxe.PosInfos) {
		++numTests;
		if (expected != actual) {
			++numFailures;
			printf("%s:%i %f should be %f\n", [p.fileName, p.lineNumber, actual, expected]);
		}
	}

	static public function eqString(expected:String, actual:String, ?p:haxe.PosInfos) {
		++numTests;
		if (expected != actual) {
			++numFailures;
			printf("%s:%i %s should be %s\n", [p.fileName, p.lineNumber, actual, expected]);
		}
	}
}