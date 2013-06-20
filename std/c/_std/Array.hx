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

class Array<T> {

	var length(default,null) : Int;

	public function new():Void {

	}

	function concat( a : Array<T> ) : Array<T> {
		return null;
	}

	function join( sep : String ) : String {
		return null;
	}

	function pop() : Null<T> {
		return null;
	}
	
	function push(x : T) : Int {
		return 0;
	}
	
	function reverse() : Void {

	}
	
	function shift() : Null<T> {
		return null;
	}
	
	function slice( pos : Int, ?end : Int ) : Array<T> {
		return null;
	}
	
	function sort( f : T -> T -> Int ) : Void {

	}
	
	function splice( pos : Int, len : Int ) : Array<T> {
		return null;
	}
	
	function toString() : String {
		return null;
	}
	
	function unshift( x : T ) : Void {

	}
	
	function insert( pos : Int, x : T ) : Void {

	}
	
	function remove( x : T ) : Bool {
		return false;
	}
	
	function copy() : Array<T> {
		return null;
	}
	
	function iterator() : Iterator<T> {
		return null;
	}
	
	function map<S>( f : T -> S ) : Array<S> {
		return null;
	}
	
	function filter( f : T -> Bool ) : Array<T> {
		return null;
	}
}
