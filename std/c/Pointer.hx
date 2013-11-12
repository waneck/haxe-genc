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
abstract Pointer<T>(Int) {
	public inline function new(i:Int) {
		this = untyped i;
	}

	@:extern @:op(A+B) public static inline function add<T>(lhs:Pointer<T>, offset:Int):Pointer<T> {
		return new Pointer(lhs.value() + offset);
	}
	@:extern @:op(A+B) public static inline function add64<T>(lhs:Pointer<T>, offset:Int64):Pointer<T> {
		return new Pointer(lhs.value() + offset.value());
	}
	@:extern @:op(A+B) public static inline function addP<T>(lhs:Pointer<T>, rhs:Pointer<T>):Pointer<T> {
		return new Pointer(lhs.value() + rhs.value());
	}

	@:extern @:op(A-B) public static inline function sub<T>(lhs:Pointer<T>, offset:Int):Pointer<T> {
		return new Pointer(lhs.value() - offset);
	}

	@:extern @:op(A-B) public static inline function sub64<T>(lhs:Pointer<T>, offset:Int64):Pointer<T> {
		return new Pointer(lhs.value() - offset.value());
	}

	@:extern @:op(A-B) public static inline function subP<T>(lhs:Pointer<T>, rhs:Pointer<T>):Pointer<T> {
		return new Pointer(lhs.value() - rhs.value());
	}

	@:extern @:op(++A) public inline function increment<T>():Pointer<T> {
		return new Pointer(++this);
	}

	@:extern @:arrayAccess public inline function __get(index:Int):T {
		return untyped this[index];
	}

	@:extern @:arrayAccess public inline function __set(index:Int, value:T):T {
		return untyped this[index] = value;
	}

	@:extern @:to inline public function const():ConstPointer<T>
	{
		return untyped this;
	}

	@:extern public inline function value() {
		return this;
	}

	@:extern public inline function pcast<T>(p:Pointer<T>):Pointer<T> return cast this;
}

@:coreType
abstract ConstPointer<T>(Pointer<T>) {
	public inline function new(ptr) this = ptr;

	@:extern @:op(A+B) public static inline function add<T>(lhs:ConstPointer<T>, offset:Int):ConstPointer<T> {
		return new ConstPointer(lhs.value() + offset);
	}

	@:extern @:op(A+B) public static inline function addP<T>(lhs:ConstPointer<T>, rhs:ConstPointer<T>):ConstPointer<T> {
		return new ConstPointer(lhs.value() + rhs.value());
	}

	@:extern @:op(A-B) public static inline function sub<T>(lhs:ConstPointer<T>, offset:Int):ConstPointer<T> {
		return new ConstPointer(lhs.value() - offset);
	}

	@:extern @:op(A-B) public static inline function subP<T>(lhs:ConstPointer<T>, rhs:ConstPointer<T>):ConstPointer<T> {
		return new ConstPointer(lhs.value() - rhs.value());
	}

	@:extern @:op(++A) public inline function increment<T>():ConstPointer<T> {
		return new ConstPointer(++this);
	}

	@:extern @:arrayAccess public inline function __get(index:Int):T {
		return untyped this[index];
	}

	@:from static public inline function fromString(s:String):ConstPointer<Char> {
		return @:privateAccess String.raw(s);
	}

	@:extern public inline function value() {
		return this;
	}
}

@:coreType
abstract FunctionPointer<T>(T) {
	public inline function new(t) this = t;
}

@:coreType
abstract VarArg from Dynamic to Dynamic { }