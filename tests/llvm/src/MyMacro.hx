import haxe.macro.Context;

class MyMacro {
	static function makeSizedVersion() {
		var c = Context.getLocalClass();
		trace(c);
		var args = Context.getConstructorArguments();
		trace(args);
		return null;
	}
}