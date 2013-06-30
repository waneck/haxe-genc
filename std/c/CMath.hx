package c;

@:include("<math.h>")
extern class CMath {
	@:require(C99) @:plain static public var NAN:Float;
	@:require(C99) @:plain static public var INFINITY:Float;
	@:require(C99) @:plain static public var HUGE_VAL:Float;
	@:require(C99) @:plain static public var HUGE_VALF:Float;
	@:require(C99) @:plain static public var HUGE_VALL:Float;

	@:require(C99) @:plain static public var MATH_ERRNO:Int;
	@:require(C99) @:plain static public var MATH_ERREXCEPT:Int;

	@:require(C99) @:plain static public var FP_LAST_FMA:Int;
	@:require(C99) @:plain static public var FP_LAST_FMAF:Int;
	@:require(C99) @:plain static public var FP_LAST_FMAL:Int;

	@:require(C99) @:plain static public var FP_INFINITE:Int;
	@:require(C99) @:plain static public var FP_NAN:Int;
	@:require(C99) @:plain static public var FP_NORMAL:Int;
	@:require(C99) @:plain static public var FP_SUBNORMAL:Int;
	@:require(C99) @:plain static public var FP_ZERO:Int;

	@:require(C99) @:plain static public var FP_ILOGB0:Int;
	@:require(C99) @:plain static public var FP_ILOGBNAN:Int;

	@:plain static public function cos(x:Float):Float;
	@:plain static public function sin(x:Float):Float;
	@:plain static public function tan(x:Float):Float;
	@:plain static public function acos(x:Float):Float;
	@:plain static public function asin(x:Float):Float;
	@:plain static public function atan(x:Float):Float;
	@:plain static public function atan2(y:Float, x:Float):Float;

	@:plain static public function cosh(x:Float):Float;
	@:plain static public function sinh(x:Float):Float;
	@:plain static public function tanh(x:Float):Float;

	@:plain static public function exp(x:Float):Float;
	@:plain static public function frexp(x:Float, exp:Pointer<Int>):Float;
	@:plain static public function ldexp(x:Float, exp:Int):Float;
	@:plain static public function log(x:Float):Float;
	@:plain static public function log10(x:Float):Float;
	@:plain static public function modf(x:Float, intpart:Pointer<Float>):Float;

	@:plain static public function pow(base:Float, exponent:Float):Float;
	@:plain static public function sqrt(x:Float):Float;

	@:plain static public function ceil(x:Float):Float;
	@:plain static public function floor(x:Float):Float;
	@:plain static public function fmod(numer:Float, denom:Float):Float;

	@:plain static public function fabs(x:Float):Float;
	@:plain static public function abs(x:Float):Float;
	
	@:require(C99) @:plain static public function copysign(x:Float, y:Float):Float;
	@:require(C99) @:plain static public function nextafter(x:Float, y:Float):Float;
	@:require(C99) @:plain static public function nexttoward(x:Float, y:Float):Float;

	@:require(C99) @:plain static public function fdim(x:Float, y:Float):Float;
	@:require(C99) @:plain static public function fmax(x:Float, y:Float):Float;
	@:require(C99) @:plain static public function fmin(x:Float, y:Float):Float;

	@:require(C99) @:plain static public function fpclassify(x:Float):Int;
	@:require(C99) @:plain static public function isfinite(x:Float):Int;
	@:require(C99) @:plain static public function isinf(x:Float):Int;
	@:require(C99) @:plain static public function isnan(x:Float):Int;
	@:require(C99) @:plain static public function isnormal(x:Float):Int;
	@:require(C99) @:plain static public function signbit(x:Float):Int;

	@:require(C99) @:plain static public function isgreater(x:Float, y:Float):Int;
	@:require(C99) @:plain static public function isgreaterequal(x:Float, y:Float):Int;
	@:require(C99) @:plain static public function isless(x:Float, y:Float):Int;
	@:require(C99) @:plain static public function islessequal(x:Float, y:Float):Int;
	@:require(C99) @:plain static public function islessgreater(x:Float, y:Float):Int;
	@:require(C99) @:plain static public function isunordered(x:Float, y:Float):Int;
}