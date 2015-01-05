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
import c.Pointer;
import c.Lib;

@:struct
class FixedArray<T> implements ArrayAccess<T>
{
	public var array(default, null):Pointer<T>;
	public var length(default, null):Int;

	public function new(len:Int, ?array:Null<Pointer<T>>)
	{
		this.length = len;
		if (array == null)
			array = cast c.CStdlib.calloc(len, Lib.sizeof(new TypeReference<T>()));
		this.array = array;
	}

	@:keep private function __get(idx:Int)
	{
		return array[idx];
	}

	@:keep private function __set(idx:Int, v:T)
	{
		return array[idx] = v;
	}

	@:extern public static inline function copy<A>(src:ConstPointer<A>, srcPos:Int, dest:Pointer<A>, destPos:Int, length:Int):Void
	{
		c.CString.memcpy((dest + destPos), (src + srcPos), length * Lib.sizeof(new TypeReference<A>()) );

	}
}
