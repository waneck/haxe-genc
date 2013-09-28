import c.Types;
import c.Pointer;

typedef MyTypedef = {
	var given:String;
	@:optional var maybeGiven:String;
}

@:build(UnitBuilder.build("src/ctest"))
class Main {
	static var count = 0;
	static var failures = 0;
	
	static public function main() {
		trace("Beginning unit tests");
		new Main();
		c.CStdio.printf("Done %i tests (%i failed)\\n", [count, failures]);
	}
	
	function optArg(s1:String, ?s2:String = "baz", ?i:Int = 12) {
		var buf:Pointer<Char> = cast c.CStdlib.malloc(s1.length + s2.length + 20);
		c.CStdio.sprintf(buf, "%s%s%i", [s1, s2, i]);
		return untyped String.ofPointerCopyNT(buf);
	}

	@:generic static function eq<T>(t1:T, t2:T, ?p:haxe.PosInfos) {
		count++;
		if (t1 != t2) {
			failures++;
			haxe.Log.trace("Failure", p);
		}
	}
	
	@:generic static function neq<T>(t1:T, t2:T, ?p:haxe.PosInfos) {
		count++;
		if (t1 == t2) {
			failures++;
			haxe.Log.trace("Failure", p);
		}
	}
	
	static function feq(f1:Float,f2:Float) {
		var f1 = f1 < 0 ? -f1 : f1;
		var f2 = f2 < 0 ? -f2 : f2;
		if (f1 > f2 && f1 - f2 > 0.00001 || f2 > f1 && f2 - f1 > 0.00001) {
			failures++;
			haxe.Log.trace("Failure");
		}
	}
	
	static function t(t:Bool, ?p:haxe.PosInfos) {
		count++;
		if (!t) {
			failures++;
			haxe.Log.trace("Failure", p);
		}
	}
	
	static function f(t:Bool, ?p:haxe.PosInfos) {
		count++;
		if (t) {
			failures++;
			haxe.Log.trace("Failure", p);
		}
	}
}