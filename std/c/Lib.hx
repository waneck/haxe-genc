class Lib
{
	@:extern public static inline function pointercopy<T>(src:Pointer<T>, srcPos:Int, dest:Pointer<T>, destPos:Int, length:Int):Void
	{
		untyped __memcpy__( (dest + destPos), (src + srcPos), length * sizeof(T) );
	}

	@:extern public static inline function sizeof(d:Dynamic):Int
	{
		return untyped __sizeof__(d);
	}

	@:extern public static inline function valueOf<T>(p:Pointer<T>, idx:Int):T
	{
		return untyped p[idx];
	}
}
