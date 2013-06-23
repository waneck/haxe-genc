package c;

class Lib
{
	@:extern public static inline function memcpy<A>(src:Pointer<A>, srcPos:Int, dest:Pointer<A>, destPos:Int, length:Int):Void
	{
		untyped __call("memcpy",(dest + destPos), (src + srcPos), length * sizeof(new TypeReference<A>()) );
	}

	@:extern public static inline function sizeof<T>(c:TypeReference<T>):Int
	{
		return untyped __sizeof__(c);
	}

	@:extern public static inline function alloc(byteSize:Int):Pointer<Void>
	{
		return untyped malloc(byteSize);
	}
}
