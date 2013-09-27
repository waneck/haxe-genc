package c;

private typedef ClosureStruct<S, T> = {
	_func: S,
	_this: T
}

abstract Closure<S, T>(ClosureStruct<S, T>) { }