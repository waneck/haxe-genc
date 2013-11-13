
package c;

@:coreType
@:int("int64")
abstract Int64 from Int {
	@:extern public inline function new(v:Int){
		this = v;
	}
	@:extern @:op(A+B) public static inline function addI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A+B) public static inline function add(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A-B) public static inline function subI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A-B) public static inline function sub(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A/B) public static inline function divI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A/B) public static inline function div(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A<<B) public static inline function shlI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A<<B) public static inline function shl(lhs:Int64, rhs:Int64):Int64;
	@:extern @:op(A>>B) public static inline function shrI(lhs:Int64, rhs:Int):Int64;
	@:extern @:op(A>>B) public static inline function shr(lhs:Int64, rhs:Int64):Int64;
	@:extern public inline function value() {
		return this;
	}
}