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
class Math
{
	static var PI(default,null) : Float;

	static var NEGATIVE_INFINITY(default, null) : Float;

	static var POSITIVE_INFINITY(default,null) : Float;

	static var NaN(default, null) : Float;

	static public function abs(v:Float):Float {
		return untyped __call(fabs,v);
	}

	static public function min(a:Float, b:Float):Float {
		return untyped __call(fmin, a, b);
	}

	static public function max(a:Float, b:Float):Float {
		return untyped __call(fmax, a, b);
	}

	static public function sin(v:Float):Float {
		return untyped __call(sin, v);
	}

	static public function cos(v:Float):Float {
		return untyped __call(cos, v);
	}

	static public function tan(v:Float):Float {
		return untyped __call(tan, v);
	}

	static public function asin(v:Float):Float {
		return untyped __call(asin, v);
	}

	static public function acos(v:Float):Float {
		return untyped __call(acos, v);
	}

	static public function atan(v:Float):Float {
		return untyped __call(atan, v);
	}

	static public function atan2(y:Float, x:Float):Float {
		return untyped __call(atan2, x, y);
	}

	static public function exp(v:Float):Float {
		return untyped __call(exp, v);
	}

	static public function log(v:Float):Float {
		return untyped __call(log, v);
	}

	static public function pow(v:Float, exp:Float):Float {
		return untyped __call(pow, v, exp);
	}

	static public function sqrt(v:Float):Float {
		return untyped __call(sqrt, v);
	}

	static public function round(v:Float):Int {
		return untyped __call(round, v);
	}

	static public function floor(v:Float):Int {
		return untyped __call(floor, v);
	}

	static public function ceil(v:Float):Int {
		return untyped __call(ceil, v);
	}

	static public function random() : Float {
		return untyped __call(rand);
	}

	static inline function ffloor( v : Float ) : Float {
		return floor(v);
	}

	static inline function fceil( v : Float ) : Float {
		return ceil(v);
	}

	static inline function fround( v : Float ) : Float {
		return round(v);
	}

	static public function isFinite( f : Float ) : Bool {
		return untyped __call(isFinite, f);
	}

	static public function isNaN( f : Float ) : Bool {
		return untyped __call(isnan, f);
	}
}


