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

package haxe.ds;

@:generic
class StringMap<T> implements Map.IMap<String,T> {

	var tree:haxe.ds.BalancedTree<String, T>;
	
	public inline function new() {
		tree = new haxe.ds.BalancedTree<String, T>(untyped String.compare);
	}

	/**
		See `Map.set`
	**/
	public inline function set( key : String, value : T ) {
		tree.set(key, value);
	}

	/**
		See `Map.get`
	**/
	public inline function get( key : String ) {
		return tree.get(key);
	}

	/**
		See `Map.exists`
	**/
	public inline function exists( key : String ) {
		return tree.exists(key);
	}

	/**
		See `Map.remove`
	**/
	public inline function remove( key : String ) {
		return tree.remove(key);
	}

	/**
		See `Map.keys`
	**/
	public inline function keys() {
		return tree.keys();
	}

	/**
		See `Map.iterator`
	**/
	public inline function iterator() {
		return tree.iterator();
	}

	/**
		See `Map.toString`
	**/
	public inline function toString() {
		return tree.toString();
	}

}
