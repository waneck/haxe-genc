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
	This class defines mathematical functions and constants.
**/

import c.CMath;

@:include("<math.h>")
class Math {
	static public var PI(default,null) : Float = 3.141592654;

	//static public var NEGATIVE_INFINITY(get, null):Float;
	//static function get_NEGATIVE_INFINITY() return -CMath.INFINITY;
		//
	//static public var POSITIVE_INFINITY(get,null):Float;
	//static public function get_POSITIVE_INFINITY() return CMath.INFINITY;
	//
	//static public var NaN(get, null):Float;
	//static public function get_NaN() return CMath.NAN;
	
	static public inline function abs(v:Float):Float {
		return CMath.abs(v);
	}

	static public function min(a:Float, b:Float):Float {
		return if( a < b ) a else if(isNaN(a)) a else b;
	}

	static public function max(a:Float, b:Float):Float {
		return if( a < b ) b else if(isNaN(b)) b else a;
	}

	static public inline function sin(v:Float):Float {
		return CMath.sin(v);
	}

	static public inline function cos(v:Float):Float {
		return CMath.cos(v);
	}

	static public inline function tan(v:Float):Float {
		return CMath.tan(v);
	}

	static public inline function asin(v:Float):Float {
		return CMath.asin(v);
	}

	static public inline function acos(v:Float):Float {
		return CMath.acos(v);
	}

	static public inline function atan(v:Float):Float {
		return CMath.atan(v);
	}

	static public inline function atan2(y:Float, x:Float):Float {
		return CMath.atan2(y, x);
	}

	static public inline function exp(v:Float):Float {
		return CMath.exp(v);
	}

	static public inline function log(v:Float):Float {
		return CMath.log(v);
	}

	static public inline function pow(v:Float, exp:Float):Float {
		return CMath.pow(v, exp);
	}

	static public inline function sqrt(v:Float):Float {
		return CMath.sqrt(v);
	}

	static public inline function round(v:Float):Int {
		return cast CMath.floor(v + 0.5);
	}

	static public inline function floor(v:Float):Int {
		return cast CMath.floor(v);
	}

	static public inline function ceil(v:Float):Int {
		return cast CMath.ceil(v);
	}

	static public inline function random() : Float {
		return c.CStdlib.rand() / c.CStdlib.RAND_MAX;
	}

	static public inline function ffloor( v : Float ) : Float {
		return CMath.floor(v);
	}

	static public inline function fceil( v : Float ) : Float {
		return CMath.ceil(v);
	}

	static public inline function fround( v : Float ) : Float {
		return CMath.floor(v + 0.5);
	}

	static public inline function isFinite( f : Float ) : Bool {
		//return cast CMath.isfinite(f);
		return false;
	}

	static public inline function isNaN( f : Float ) : Bool {
		return f != f;
	}
}


