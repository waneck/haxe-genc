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

@:coreApi
class Reflect {

	public static function hasField( o : Dynamic, field : String ) : Bool {
		return false;
	}

	public static function field( o : Dynamic, field : String ) : Dynamic {
		return null;
	}

	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void {

	}

	public static function getProperty( o : Dynamic, field : String ) : Dynamic {
		return null;
	}

	public static function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void {

	}

	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic {
		return null;
	}

	public static function fields( o : Dynamic ) : Array<String> {
		return [];
	}

	public static function isFunction( f : Dynamic ) : Bool {
		return false;
	}

	public static function compare<T>( a : T, b : T ) : Int {
		return 0;
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		return false;
	}

	public static function isObject( v : Dynamic ) : Bool {
		return false;
	}

	public static function isEnumValue( v : Dynamic ) : Bool {
		return false;
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool {
		return false;
	}

	public static function copy<T>( o : T ) : T {
		return o;
	}

	@:overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return null;
	}
}
