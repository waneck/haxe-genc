
package c;

@:coreType
@:int("int64")
abstract Int64 from Int {
	public function new(v:Int){
		this = v;
	}

	@:extern @:op(A+B) public static function addI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A+B) public static function add(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A-B) public static function subI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A-B) public static function sub(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A*B) public static function mulI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A*B) public static function mul(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A/B) public static function divI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A/B) public static function div(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A<<B) public static function shlI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A<<B) public static function shl(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A>>B) public static function shrI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A>>B) public static function shr(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A^B) public static function xorI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A^B) public static function xor(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A&B) public static function andI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A&B) public static function and(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A|B) public static function orI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A|B) public static function or(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A>B) public static function gtI(lhs:Int64, rhs:Int):Bool;
	@:extern @:op(A>B) public static function gt(lhs:Int64, rhs:Int64):Bool;
	@:extern @:op(A<B) public static function ltI(lhs:Int64, rhs:Int):Bool;
	@:extern @:op(A<B) public static function lt(lhs:Int64, rhs:Int64):Bool;
	@:extern @:op(A>=B) public static function gteI(lhs:Int64, rhs:Int):Bool;
	@:extern @:op(A>=B) public static function gte(lhs:Int64, rhs:Int64):Bool;
	@:extern @:op(A<=B) public static function lteI(lhs:Int64, rhs:Int):Bool;
	@:extern @:op(A<=B) public static function lte(lhs:Int64, rhs:Int64):Bool;
	@:extern public inline function value() {
		return cast this;
	}
	@:extern public inline function int32():Int {
		return cast this;
	}
	@:extern public inline function pointer<T>():Pointer<T> {
		return cast this;
	}
}
