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

#if CSTRING

import c.Types;
import c.Pointer;
import c.TypeReference;
import c.FixedArray;
import c.Lib;
import c.CString.memcmp;

//@:include("<string.h>")
@:coreApi @:final class String {
	
	public var length(default,null) : Int;
	private var __a:Pointer<Char>;

	@:keep private static function memmem(p0:Pointer<Char>,l0:Int,p1:Pointer<Char>,l1:Int):Int{
		// nothing is everywhere
		if (l1 == 0)
			return 0;
		// can't be contained
		else if(l1 > l0 || l0 == 0)
			return -1;
		// same string
		else if (l0==l1 && memcmp(p0,p1,l0) == 0)
			return 0;
		// actual work
		else {
			var first:Int = p1[0];
			var pos:Int = 0;
			while (pos < l0){
				var pchr:Pointer<Char> = c.CString.memchr(p0+pos,first,l0-pos);
				//untyped __c('printf("mm %d \\n",pos)');
				if (pchr != new Pointer(0)){
					pos = (pchr - p0).value();
					if (memcmp(pchr,p1,l1) == 0){
						return pos;
					} else {
						pos += 1;
					}
				} else {
					return -1;
				}
			}
			return -1;
		}
	}
	
	@:keep private static function compare(s0:String,s1:String) : Int {
		if (s0.length != s1.length)
			return 1;
		return memmem(s0.__a, s0.length, s1.__a, s1.length);
	}
	
	@:keep private static function equals(s0:String,s1:String) : Bool {
		return compare(s0,s1) == 0;
	}
	
	@:keep private static function concat(s0:String,s1:String) : String {
		if (s0.length == 0)
			return s1;
		if (s1.length == 0)
			return s0;
		var ret = new String(null);
		ret.length = s0.length + s1.length;
		ret.__a = cast c.CStdlib.calloc(ret.length, Lib.sizeof(new TypeReference<Char>()));
		FixedArray.copy(s0.__a,0,ret.__a,0,s0.length);
		FixedArray.copy(s1.__a,0,ret.__a,s0.length,s1.length);
		return ret;
	}
	/* NT is null-terminated, for char* literals.
	 * How to deal with strings that aren't used after return (stack-allocation)?
	 * TODO: externs for string.h
	 */
	@:keep private static function ofPointerCopyNT(p:Pointer<Char>):String {
		var len = c.CString.strlen(cast p);
		return ofPointerCopy(len,p); // keep 0x00 for C-compat TODO: do we want that?
	}

	@:keep private static function ofPointerCopy<T>(len:Int, p:Pointer<Char>):String
	{
		var ret = new String(null);
		ret.__a = cast c.CStdlib.calloc(len, Lib.sizeof(new TypeReference<Char>()));
		FixedArray.copy(p,0,ret.__a,0,len);
		ret.length = len;
		return ret;
	}
	
	@:keep private static function stringOfSize(len:Int):String{
		var s = new String(null);
		s.__a = cast c.CStdlib.calloc(len, Lib.sizeof(new TypeReference<Char>()));
		s.length = len;
		return s;
	}
	
	@:keep static private function raw(s:String):ConstPointer<Char> {
		return cast s.__a;
	}
	
	public function new(string:String) {
		if ( string != null ){
			length = string.length;
			__a = cast c.CStdlib.calloc(string.length, Lib.sizeof(new TypeReference<Char>()));
			FixedArray.copy(string.__a,0,__a,0,string.length); // 0x00
		} else {
			__a = null;
			length = 0;
		}
	}

	public function toUpperCase():String {
		return this;
	}

	public function toLowerCase():String {
		return this;
	}
	/*
	 * we need a static string of size 0, and for this implementation all strings of size 1, too
	 */
	public function charAt(index : Int):String {
		if (index > 0 && index < length){
			var ret = stringOfSize(1);
			ret.__a[0] = __a[index];
			return ret;
		} else {
			return null;
		}
	}

	public function charCodeAt( index : Int):Null<Int> {
		if (index > 0 && index < length)
			return untyped __a[index]; // Field charCodeAt has different type than in core type
									 // index : Int -> Null<Int> should be index : Int -> Int ??
		return null; // TODO this is *definitely* wrong here, null (0) is a valid char code
	}

	
	public function indexOf( str : String, ?startIndex : Int ):Int {
		if (str.length == 0){
			return 0;
		}
		if (startIndex < 0 || startIndex >= length) {
			return -1;
		}
		//untyped __c('printf("idxOf: sidx: %d \\n",startIndex)');
		var p_tmp0 = __a + startIndex; // TODO inlining produces garbage
		var ret = memmem(p_tmp0,
						length-startIndex,
						str.__a,
						str.length );
		return ret > -1 ? startIndex+ret : -1;
	}

	public function lastIndexOf( str : String, ?startIndex : Int ):Int {
		if (str.length == 0)
			return length-1;
		else if (str.length < 0 || str.length > length)
			return -1;
		if (startIndex < 0 || startIndex >= length)
			return -1;
		var first = str.__a[0];
		startIndex = startIndex > 0 ? startIndex : length-1; // NULL == 0 issue, TODO
		var pos = (length - str.length) < startIndex ? (length - str.length) : startIndex;
		//untyped __c('printf("%d \\n",pos)');
		do{
			var p_tmp = __a + pos;      // TODO inlining produces garbage
			var char_at_pos = __a[pos]; // TODO inlining produces garbage
			if (char_at_pos == first && (memcmp(p_tmp,str.__a,str.length) == 0)){
				return pos;
			} else {
				--pos;
			}
		} while (pos > -1);
		return -1;
	}

	public function split( delimiter : String ):Array<String> {
		
		if (delimiter.length == 0){
			var ret = new Array();
			for (i in 0...length){
				ret.push(fromCharCode(charCodeAt(i)));
			}
			return ret;
		}
		var start = 0;
		var cur = indexOf(delimiter);
		if (cur == -1){
			return [this];
		} else {
			var ret = new Array();
			while (true){
				ret.push(substr(start,cur-start));
				start    = (cur + delimiter.length);
				var tmp  = indexOf(delimiter,start);
				if (-1 == tmp){
					if (cur < length){
						ret.push(substr(start,length-cur));
					}
					break;
				} else {
					cur = tmp;
				}
			}
			return ret;
		}
	}

	public function substr( pos : Int, ?len : Int ):String {
		if (pos < 0) {
			pos = (length + pos) < 0 ? 0 : (length + pos);
		}
		if (len < 0) {
			//undefined
			return null;
		} else if (len == 0) {
			len = length - pos; // omitted case, TODO
		}
		if (pos+len > length){
			len = length-pos;
		}
		var ret = stringOfSize(len);
		FixedArray.copy( __a, pos, ret.__a, 0, len);
		return ret;
	}

	public function substring( startIndex : Int, ?endIndex : Int ):String {
		//
		startIndex = startIndex < 0 ? 0 : startIndex;
		endIndex = endIndex < 0 ? 0 : endIndex;
		var len = endIndex - startIndex;
		return substr(startIndex,len);
	}

	public function toString():String {
		return this;
	}

	public static function fromCharCode( code : Int ):String {
		var ret = stringOfSize(1);
		ret.__a[0] = untyped code; //TODO int->Int8 conv
		return ret;
	}
}

#else
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
/**
	The basic String class.

	A haxe String is immutable, it is not possible to modify individual
	characters. No method of this class changes the state of `this` String.

	Strings can be constructed using the string literal syntax "string value".

	String can be concatenated by using the + operator. If an operand is not a
	String, it is passed through Std.string() first.
**/

class String {

	var length(default,null) : Int;

	public function new(string:String) {

	}

	function toUpperCase() {

	}

	function toLowerCase() {

	}

	function charAt(index : Int) {

	}

	function charCodeAt( index : Int) {

	}

	function indexOf( str : String, ?startIndex : Int ) {

	}

	function lastIndexOf( str : String, ?startIndex : Int ) {

	}

	function split( delimiter : String ) {

	}

	public function substr( pos : Int, ?len : Int ) {

	}

	function substring( startIndex : Int, ?endIndex : Int ) {

	}

	public function toString() {
		return this;
	}

	static function fromCharCode( code : Int ) {

	}
}
#end
