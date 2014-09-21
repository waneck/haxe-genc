package c;
import c.Pointer;

@:keep
class Closure<F> {
	var _func:FunctionPointer<Void->Pointer<Void>>;
	var _has_tp:Bool;
    var _this:Dynamic;
	
	public function new(func, _this) {
		this._func = func;
		this._has_tp = false;
        this._this = _this;
	}
}