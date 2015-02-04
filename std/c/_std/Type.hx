/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

import c.Pointer.FunctionPointer;

enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass( c : Class<Dynamic> );
	TEnum( e : Enum<Dynamic> );
	TUnknown;
}

@:coreApi class Type {
	public static function getClass<T>( o : T ) : Class<T> untyped {
		return null;
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped {
		return null;
	}


	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> {
		var t:c.TypeReference<Dynamic> = cast c;
		return cast t.parentClass;
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		var t:c.TypeReference<Dynamic> = cast c;
		return untyped String.ofPointerCopyNT(t.name);
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		return null;
	}

	public static function resolveClass( name : String ) : Class<Dynamic> {
		var raw = untyped String.raw(name);
		for (typeref in c.Boot.typeReferences) {
			if (c.CString.strcmp(raw, typeref.name) == 0) return cast typeref;
		}
		return null;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped {
		return null;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T {
		var t:c.TypeReference<T> = cast cl;
		var ctor:Dynamic = t.constructor;
		if (ctor == null) {
			return null;
		}
		return switch (args) {
			case []: (ctor : FunctionPointer<Void -> T>)();
			case [a]: (ctor : FunctionPointer<Dynamic -> T>)(a);
			case [a, b]: (ctor : FunctionPointer<Dynamic -> Dynamic -> T>)(a, b);
			case [a, b, c]: (ctor : FunctionPointer<Dynamic -> Dynamic -> Dynamic -> T>)(a, b, c);
			case _: throw 'Something went wrong';
		}
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		var t:c.TypeReference<T> = cast cl;
		if (t.allocator == null) {
			return null;
		}
		return t.allocator();
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		return null;
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		return null;
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> untyped {
		return null;
	}

	public static function typeof( v : Dynamic ) : ValueType untyped {
		return null;
	}

	public static function enumEq<T>( a : T, b : T ) : Bool untyped {
		return false;
	}

	public static function enumConstructor( e : EnumValue ) : String {
		return null;
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> {
		return null;
	}

	public inline static function enumIndex( e : EnumValue ) : Int {
		return untyped e.index;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> {
		return null;
	}
}