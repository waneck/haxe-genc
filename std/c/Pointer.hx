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
package c;

import c.Types;
import c.NInt;

@:runtimeValue
@:coreType
abstract Pointer<T> to ConstPointer<T> {
	public inline function new(i:Int) {
		this = untyped i;
	}

	@:extern @:op(A+B) public static function add<T>(lhs:Pointer<T>, offset:Int):Pointer<T>;
	@:extern @:op(A+B) public static function add64<T>(lhs:Pointer<T>, offset:Int64):Pointer<T>;
	@:extern @:op(A+B) public static function addP<T>(lhs:Pointer<T>, rhs:Pointer<T>):Pointer<T>;
	@:extern @:op(A-B) public static function sub<T>(lhs:Pointer<T>, offset:Int):Pointer<T>;
	@:extern @:op(A-B) public static function sub64<T>(lhs:Pointer<T>, offset:Int64):Pointer<T>;
	@:extern @:op(A-B) public static function subP<T>(lhs:Pointer<T>, rhs:Pointer<T>):Pointer<T>;
	@:extern @:op(++A) public function increment():Pointer<T>;

	@:arrayAccess public function __get(index:Int):T;
	@:arrayAccess public function __set(index:Int, value:T):T;

	@:extern @:to inline public function const():ConstPointer<T>
	{
		return untyped this;
	}

	public inline function value() {
		return this;
	}

	public inline function pcast<U>(p:Pointer<U>):Pointer<U> return cast this;
	public inline function pcast2<U>():Pointer<U> return cast this;
	public inline function int64():Int64 return cast this;
}

@:coreType
abstract ConstPointer<T> {
	public inline function new(ptr) this = ptr;

	@:op(A+B) public static function add<T>(lhs:ConstPointer<T>, offset:Int):ConstPointer<T>;
	@:op(A+B) public static function addP<T>(lhs:ConstPointer<T>, rhs:ConstPointer<T>):ConstPointer<T>;
	@:op(A-B) public static function sub<T>(lhs:ConstPointer<T>, offset:Int):ConstPointer<T>;
	@:op(A-B) public static function subP<T>(lhs:ConstPointer<T>, rhs:ConstPointer<T>):ConstPointer<T>;

	@:op(++A) public function increment<T>():ConstPointer<T>;

	@:noExpr // HACK HACK HACK HACK
	@:arrayAccess public inline function __get(index:Int):T {
		return untyped this[index];
	}

	@:from static public inline function fromString(s:String):ConstPointer<Char> {
		return @:privateAccess String.raw(s);
	}

	public inline function value() {
		return this;
	}
}

@:coreType
@:callable
abstract FunctionPointer<T> {}

@:coreType
abstract VarArg from Dynamic to Dynamic { }