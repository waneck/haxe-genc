
package c;

@:coreType
@:int("int64")
abstract Int64 from Int {
	public function new(v:Int){
		this = v;
	}

	@:extern @:op(A+B) public static inline function addI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A+B) public static inline function add(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A-B) public static inline function subI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A-B) public static inline function sub(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A*B) public static inline function mulI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A*B) public static inline function mul(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A/B) public static inline function divI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A/B) public static inline function div(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A<<B) public static inline function shlI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A<<B) public static inline function shl(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A>>B) public static inline function shrI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A>>B) public static inline function shr(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A^B) public static inline function xorI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A^B) public static inline function xor(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A&B) public static inline function andI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A&B) public static inline function and(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A|B) public static inline function orI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A|B) public static inline function or(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A>B) public static inline function gtI(lhs:Int64, rhs:Int):Bool;
	@:extern @:op(A>B) public static inline function gt(lhs:Int64, rhs:Int64):Bool;
	@:extern @:op(A<B) public static inline function ltI(lhs:Int64, rhs:Int):Bool;
	@:extern @:op(A<B) public static inline function lt(lhs:Int64, rhs:Int64):Bool;
	@:extern @:op(A>=B) public static inline function gteI(lhs:Int64, rhs:Int):Bool;
	@:extern @:op(A>=B) public static inline function gte(lhs:Int64, rhs:Int64):Bool;
	@:extern @:op(A<=B) public static inline function lteI(lhs:Int64, rhs:Int):Bool;
	@:extern @:op(A<=B) public static inline function lte(lhs:Int64, rhs:Int64):Bool;
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
