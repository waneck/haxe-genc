package c;

@:native("typeref")
@:keep class TypeReference<T>
{
	/** represents the size, in bytes, of a reference to that type; on reference types, it should be equal to word size **/
	public var refSize:Int;
	/** the fully qualified name of the type **/
	public var name:String;


	@:extern public function new() {}
}

// @:delegate abstract
