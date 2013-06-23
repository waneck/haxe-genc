package c;

@:native("typeref")
@:keep class TypeReference<T>
{
	/** reference to the type's null value **/
	private var nullval(default, never):T;
	/** represents the size, in bytes, of a reference to that type; on reference types, it should be equal to word size **/
	public var refSize(default, never):Int;
	/** the fully qualified name of the type **/
	public var name(default, never):String;

	@:extern public function new() {}
}
