package c;

class Lib
{
	@:extern public static inline function memcpy<A,B>(dest:Pointer<A>, src:Pointer<B>, length:Int):Void
	{
		untyped __call("memcpy",dest, src, length);
	}
	
	@:extern public static inline function sizeof<T>(c:TypeReference<T>):Int
	{
		return untyped __sizeof(c);
	}

	@:extern public static inline function dereference<T>(ptr:Pointer<T>):T
	{
		return untyped __deref(ptr);
	}
	
	@:extern public static inline function getAddress<T>(t:T):Pointer<T>
	{
		return untyped __addressof(t);
	}
	
	@:extern public static inline function alloc(byteSize:Int):Pointer<Void>
	{
		return untyped __call("malloc", byteSize);
	}
		
	@:extern public static inline function calloc(num:Int, sizeEach:Int):Pointer<Void>
	{
		return untyped __call("calloc", num, sizeEach);
	}
}