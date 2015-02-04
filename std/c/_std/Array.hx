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

import c.Pointer;
import c.TypeReference;
import c.FixedArray;
import c.Lib;
import c.Types.Char;
import c.Types.Short;
import c.CStdio;
import c.CString;
import c.NInt.Int64;

@:coreApi
@:final class Array<T> implements ArrayAccess<T>
{
	public var length(default,null) : Int;

	private var __a:Pointer<T>;
	private var __byte_length:Int;

	public function new() : Void
	{
		this.length = 0;
		var byteLength = Lib.sizeof(new c.TypeReference<T>());
		this.__a = cast __alloc_mem(byteLength); // TODO: fix wasting the allocation
		this.__byte_length = byteLength;
	}

	@:specialize
	private static function __new<T>(len:Int):Array<T> {
		var ret = new Array();
		ret.__a = __alloc_mem(len);
		ret.__byte_length = len;
		return cast ret;
	}

	@:keep
	@:specialize
	private static function __alloc_mem<T>(len:Int):Pointer<T>{
		var size = Lib.sizeof(new c.TypeReference<T>());
		var p:Pointer<T> = cast c.CStdlib.calloc(len, size);
		return p;
	}

	@:extern
	@:specialize
	private static inline function memcpy<T>(src:Pointer<T>, srcPos:Int, dest:Pointer<T>, destPos:Int, length:Int):Void
	{
		var size = Lib.sizeof(new c.TypeReference<T>());
		c.CString.memcpy((dest + destPos * size), (src + srcPos * size), length * size);
	}

	@:keep
	@:specialize
	private static function ofNative<X>(native:Pointer<X>, length:Int):Array<X>
	{
		var ret = new Array();
		ret.__a = cast native;
		ret.length = length;
		return ret;
	}

	@:specialize
	public function concat( a : Array<T> ) : Array<T>
	{
		var length = length;
		var len = length + a.length;
		var retarr = __alloc_mem(len);
		memcpy(cast __a, 0, cast retarr, 0, length);
		memcpy(cast a.__a, 0, cast retarr, length, a.length);

		return cast ofNative(cast retarr, len);
	}

	@:specialize
	public function copy() : Array<T>
	{
		var len = length;
		var newarr = __alloc_mem(len);
		memcpy(__a, 0, newarr, 0, len);
		var ret = ofNative(newarr,len);
		ret.__byte_length = len;
		return cast ret;
	}

	@:specialize
	public function insert( pos : Int, x : T ) : Void
	{
		var l = this.length;
		if( pos < 0 ) {
			pos = l + pos;
			if( pos < 0 ) pos = 0;
		}
		if ( pos >= l ) {
			this.push(x);
			return;
		} else if (pos == 0) {
			this.unshift(x);
			return;
		}

		var elementSize = Lib.sizeof(new c.TypeReference<T>());
		var byteOffset = pos * elementSize;
		if (l * elementSize >= __byte_length)
		{
			var newLen = (length << 1) + 1;
			var newarr = __alloc_mem(newLen);
			__byte_length = newLen;
			memcpy(__a, 0, newarr, 0, pos);
			newarr[pos] = cast x;
			memcpy(__a, pos, newarr, pos + 1, l - pos);

			this.__a = cast newarr;
			++this.length;
		} else {
			CString.memmove(__a + byteOffset, __a + byteOffset + elementSize, (l-pos) << 2);
			__a[pos] = cast x;
			++this.length;
		}
	}

	@:specialize
	public function iterator() : Iterator<T>
	{
		var i = 0;
		var len = length;
		return {
			hasNext:function() return i < len,
			next:function() {
				i = i + 1;
				return cast __a[i - 1];
			}
		};
	}

	@:specialize
	public function indexOf(x:T, ?fromIndex:Int = 0):Int {
		var i:Int = fromIndex; // coerce, defaults to 0
		while (i < length) {
			if (__a[i] == x) {
				return i;
			}
			++i;
		}
		return -1;
	}

	@:specialize
	public function lastIndexOf(x:T, ?fromIndex:Int):Int {
		var i:Int = fromIndex == null ? length - 1 : fromIndex;
		while (i > 0) {
			if (__a[i] == x) {
				return i;
			}
			--i;
		}
		return -1;
	}

	public function join( sep : String ) : String
	{
		// var buf = new StringBuf();
		// var i = -1;

		// var first = true;
		// var length = length;
		// while (++i < length)
		// {
		// 	if (first)
		// 		first = false;
		// 	else
		// 		buf.add(sep);
		// 	buf.add(__a.array[i]);
		// }

		// return buf.toString();
		return null; //TODO
	}

	@:specialize
	public function pop() : Null<T>
	{
		var length = length;
		if (length > 0)
		{
			length-=1;
			var val = cast __a[length];
			__a[length] = cast 0;
			this.length = length;

			return cast val;
		} else {
			return null;
		}
	}

	@:specialize
	public function push(x : T) : Int
	{
		var length = length;
		if (length >= __byte_length)
		{
			var newLen = (length << 1) + 1;
			var newarr = __alloc_mem(newLen);
			memcpy(__a, 0, newarr, 0, length);
			__byte_length = newLen;
			__a = newarr;
			__a[length] = cast x;

			this.__a = cast newarr;
		} else {
	        __a[length] = cast x;
		}
		return ++this.length;
	}

	@:specialize
	public function remove( x : T ) : Bool
	{
		var i = -1;
		var length = length;
		while (++i < length)
		{
			if (__a[i] == cast x)
			{
				memcpy(__a, i + 1, __a, i, length - i - 1);
				this.length-=1;
				__a[this.length] = cast 0;

				return true;
			}
		}

		return false;
	}

	@:specialize
	public function reverse() : Void
	{
		var i = 0;
		var l = this.length;
		var a = this.__a;
		var half = l >> 1;
		l -= 1;
		while ( i < half )
		{
			var tmp = __a[i];
			__a[i] = __a[l-i];
			__a[l-i] = tmp;
			i += 1;
		}
	}

	@:specialize
	public function shift() : Null<T>
	{
		var l = this.length;
		if( l == 0 )
			return null;

		var x = __a[0];
		l -= 1;
		var elementSize = Lib.sizeof(new c.TypeReference<T>());
		CString.memmove(__a, __a + elementSize, l << 2);

		__a[l] = cast 0;
		this.length = l;
		return cast x;
	}

	@:specialize
	public function slice( pos : Int, ?end : Int ) : Array<T>
	{
		if( pos < 0 ){
			pos = this.length + pos;
			if( pos < 0 )
				pos = 0;
		}
		if( end == null )
			end = this.length;
		else if( end < 0 )
			end = this.length + end;
		if( end > this.length )
			end = this.length;
		var len = end - pos;
		if ( len < 0 ) return new Array();

		var newarr    = __alloc_mem(len);
		memcpy(__a, pos, newarr, 0, len);

		return cast ofNative(newarr,len);
	}

	public function sort( f : T -> T -> Int ) : Void
	{
		//TODO
	// 	if (length == 0)
	// 		return;
	// 	quicksort(0, length - 1, f);
	}
//
// 	private function quicksort( lo : Int, hi : Int, f : T -> T -> Int ) : Void
// 	{
//         var buf = __a;
// 		var i = lo, j = hi;
//         var p = buf[(i + j) >> 1];
// 		while ( i <= j )
// 		{
// 			while ( f(buf[i], p) < 0 ) i++;
//             while ( f(buf[j], p) > 0 ) j--;
// 			if ( i <= j )
// 			{
//                 var t = buf[i];
//                 buf[i++] = buf[j];
//                 buf[j--] = t;
//             }
// 		}
//
// 		if( lo < j ) quicksort( lo, j, f );
//         if( i < hi ) quicksort( i, hi, f );
// 	}

	@:specialize
	public function splice( pos : Int, len : Int ) : Array<T>
	{
		if( len < 0 ) return __new(1);
		if( pos < 0 ) {
			pos = this.length + pos;
			if( pos < 0 ) pos = 0;
		}
		if( pos > this.length ) {
			pos = 0;
			len = 0;
		} else if( pos + len > this.length ) {
			len = this.length - pos;
			if( len < 0 ) len = 0;
		}

		var ret = __alloc_mem(len);

		memcpy(__a, pos, ret, 0, len);

		var ret = ofNative(ret,len);
		ret.__byte_length = len;

		var end = pos + len;
		memcpy(__a, end, __a, pos, this.length - end);
		this.length -= len;
		while( --len >= 0 )
			__a[this.length + len] = cast 0;
		return cast ret;
	}

	public function toString() : String
	{
		var ret = new StringBuf();
		var a = __a;
		ret.add("[");
		var first = true;
		for (i in 0...length)
		{
			if (first)
				first = false;
			else
				ret.add(",");
			ret.add(Std.string(a[i]));
		}

		ret.add("]");
		return ret.toString();
	}

	@:specialize
	public function unshift( x : T ) : Void
	{
		var length = length;
		var elementSize = Lib.sizeof(new c.TypeReference<T>());
		if (length * elementSize >= __byte_length)
		{
			var newLen = (length << 1) + 1;
			var newarr = __alloc_mem(newLen);
			memcpy(__a, 0, newarr, 1, length);
			__byte_length = newLen;
			__a = newarr;

			this.__a = cast newarr;
		} else {
			CString.memmove(__a, __a + elementSize, length << 2);
		}

		this.__a[0] = cast x;
		++this.length;
	}

	public function map<S>( f : T -> S ) : Array<S> {
		return null; //TODO
		// var ret = [];
		// for (elt in this)
		// 	ret.push(f(elt));
		// return ret;
	}

	public function filter( f : T -> Bool ) : Array<T> {
		return null; //TODO
		// var ret = [];
		// for (elt in this)
		// 	if (f(elt))
		// 		ret.push(elt);
		// return ret;
	}

	@:keep
	@:specialize
	private function __get(idx:Int):T
	{
		if (idx >= length || idx < 0)
			return cast 0;

		return __a[idx];
	}

	@:specialize
	@:keep
	private function __set(idx:Int, v:T):T
	{
		if (idx >= __byte_length)
		{
			var newl = idx + 1;
			if (idx == __byte_length)
				newl = (idx << 1) + 1;
			var newArr = __alloc_mem(newl);
			__byte_length = newl;
			if (length > 0)
				memcpy(__a, 0, newArr, 0, length);
			this.__a = cast (__a = newArr);
		}

		if (idx >= length)
			this.length = idx + 1;

		return __a[idx] = cast v;
	}
}
