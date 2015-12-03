/*
 * Copyright (C)2005-2015 Haxe Foundation
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

package haxe.ds;

class IntMap<T> implements Map.IMap<Int,T> {

	var tree:haxe.ds.BalancedTree<Int, T>;
	
	static function compare(i1:Int, i2:Int) return i1 - i2;
	
	public inline function new() {
		tree = new haxe.ds.BalancedTree<Int, T>(compare);
	}

	public inline function set( key : Int, value : T ) {
		tree.set(key, value);
	}

	public inline function get( key : Int ) {
		return tree.get(key);
	}

	public inline function exists( key : Int ) {
		return tree.exists(key);
	}
	
	public inline function remove( key : Int ) {
		return tree.remove(key);
	}

	public inline function keys() {
		return tree.keys();
	}

	public inline function iterator() {
		return tree.iterator();
	}

	public inline function toString() {
		return tree.toString();
	}

}
