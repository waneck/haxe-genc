package c;

@:include("<math.h>")
extern class CMath {
	@:plain static public var NAN:Float;
	@:plain static public var INFINITY:Float;
	@:plain static public var HUGE_VAL:Float;
	@:plain static public var HUGE_VALF:Float;
	@:plain static public var HUGE_VALL:Float;

	@:plain static public var MATH_ERRNO:Int;
	@:plain static public var MATH_ERREXCEPT:Int;

	@:plain static public var FP_LAST_FMA:Int;
	@:plain static public var FP_LAST_FMAF:Int;
	@:plain static public var FP_LAST_FMAL:Int;

	@:plain static public var FP_INFINITE:Int;
	@:plain static public var FP_NAN:Int;
	@:plain static public var FP_NORMAL:Int;
	@:plain static public var FP_SUBNORMAL:Int;
	@:plain static public var FP_ZERO:Int;

	@:plain static public var FP_ILOGB0:Int;
	@:plain static public var FP_ILOGBNAN:Int;

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

	@:plain static public function copysign(x:Float, y:Float):Float;
	@:plain static public function nextafter(x:Float, y:Float):Float;
	@:plain static public function nexttoward(x:Float, y:Float):Float;

	@:plain static public function fdim(x:Float, y:Float):Float;
	@:plain static public function fmax(x:Float, y:Float):Float;
	@:plain static public function fmin(x:Float, y:Float):Float;

	@:plain static public function fabs(x:Float):Float;
	@:plain static public function abs(x:Float):Float;

	@:plain static public function fpclassify(x:Float):Int;
	@:plain static public function isfinite(x:Float):Int;
	@:plain static public function isinf(x:Float):Int;
	@:plain static public function isnan(x:Float):Int;
	@:plain static public function isnormal(x:Float):Int;
	@:plain static public function signbit(x:Float):Int;

	@:plain static public function isgreater(x:Float, y:Float):Int;
	@:plain static public function isgreaterequal(x:Float, y:Float):Int;
	@:plain static public function isless(x:Float, y:Float):Int;
	@:plain static public function islessequal(x:Float, y:Float):Int;
	@:plain static public function islessgreater(x:Float, y:Float):Int;
	@:plain static public function isunordered(x:Float, y:Float):Int;
}