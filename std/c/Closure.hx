package c;
import c.Pointer;

@:keep
class Closure<F> {
	var _func:FunctionPointer<Void->Void>;
	var _this:Dynamic;
}