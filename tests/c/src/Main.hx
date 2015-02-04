import c.Types;
import c.Pointer;

@:build(UnitBuilder.build("src/ctest"))
class Main {
	static var count = 0;
	static var failures = 0;

	static public function main() {
		trace("Beginning unit tests");
		new Main();
		TestVTable.run();
		c.CStdio.printf("Done %i tests (%i failed)\\n", [count, failures]);
	}

	static public function optArg(s1:String, ?s2:String = "baz", ?i:Int = 12) {
		var buf:Pointer<Char> = cast c.CStdlib.malloc(s1.length + s2.length + 20);
		c.CStdio.sprintf(buf, "%s%s%i", [s1, s2, i]);
		return untyped String.ofPointerCopyNT(buf);
	}

	@:generic static public function equals<T>(t1:T, t2:T, ?p:haxe.PosInfos) {
		count++;
		if (t1 != t2) {
			failures++;
			haxe.Log.trace("Failure", p);
		}
	}

	@:generic static public function equalsNot<T>(t1:T, t2:T, ?p:haxe.PosInfos) {
		count++;
		if (t1 == t2) {
			failures++;
			haxe.Log.trace("Failure", p);
		}
	}

	static public function equals_Float(f1:Float,f2:Float, ?p:haxe.PosInfos) {
		var f1 = f1 < 0 ? -f1 : f1;
		var f2 = f2 < 0 ? -f2 : f2;
		if (f1 > f2 && f1 - f2 > 0.00001 || f2 > f1 && f2 - f1 > 0.00001) {
			failures++;
			haxe.Log.trace("Failure", p);
		}
	}

	static public function isTrue(t:Bool, ?p:haxe.PosInfos) {
		count++;
		if (!t) {
			failures++;
			haxe.Log.trace("Failure", p);
		}
	}

	static public function isFalse(t:Bool, ?p:haxe.PosInfos) {
		count++;
		if (t) {
			failures++;
			haxe.Log.trace("Failure", p);
		}
	}
}
