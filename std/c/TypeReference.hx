package c;

import c.Pointer;
import c.Types;

@:keep
@:noVTable
class TypeReference<T> {
	/**
		The fully qualified dot-path of the type.
	**/
	public var name(default, never):ConstPointer<Char>;

	/**
		The type's default value.
	**/
	public var defaultValue(default, never):T;

	/**
		The size of references to this type.
	**/
	public var refSize(default, never):Int;

	/**
		Pointer to the constructor of this type.
	**/
	public var constructor:FunctionPointer<Void -> T>;

	/**
		Pointer to the allocator of this type.
	**/
	public var allocator:FunctionPointer<Void -> T>;

	/**
		Reference to the `TypeReference` of the parent class if exists.
	**/
	public var parentClass:Null<TypeReference<Dynamic>>;

	@:extern public function new() {}
}
