package c;
import haxe.ds.StringMap;
import c.Pointer;

@:keep
@:noVTable
class Access<T> {
	public var value:T;
	public var fields:StringMap<Dynamic>;
	public var get:FunctionPointer<Dynamic->String->Dynamic>;
	public var set:FunctionPointer<Dynamic->String->Dynamic->Dynamic>;
}