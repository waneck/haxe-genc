package c;
import c.Lib;

@:coreType
abstract Struct<T> {
	@:to public inline function toPointer():T{
		return cast Lib.getAddress(this);
	}
	public inline function p():T{
		return cast Lib.getAddress(this);
	}
}