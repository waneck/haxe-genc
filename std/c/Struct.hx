package c;
import c.Lib;

@:analyzer(no_simplification)
@:coreType
abstract Struct<T> {
	@:to public inline function toPointer():T{
		return cast Lib.getAddress(this);
	}
	public inline function p():T{
		return cast Lib.getAddress(this);
	}
	public var v(get,never):T;
    inline function get_v():T return cast Lib.getAddress(this);
    
}