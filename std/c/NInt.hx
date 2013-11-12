
package c;
import c.Pointer;
import c.COps.*;
import c.COps;

@:coreType
@:int("int64")
@:runtimeValue
abstract Int64(Int) from Int {

	@:extern public inline function new(v:Int){
		this = v;
	}
	@:extern @:op(A+B) public static inline function addI(lhs:Int64, rhs:Int):Int64 {
		return hxc_add(lhs,rhs);
	}
	@:extern @:op(A+B) public static inline function add(lhs:Int64, rhs:Int64):Int64 {
		return hxc_add(lhs,rhs);
	}
	@:extern @:op(A-B) public static inline function subI(lhs:Int64, rhs:Int):Int64 {
		return hxc_sub(lhs,rhs);
	}
	@:extern @:op(A-B) public static inline function sub(lhs:Int64, rhs:Int64):Int64 {
		return hxc_sub(lhs,rhs);
	}

	@:extern @:op(A*B) public static inline function mul(lhs:Int64, rhs:Int64):Int64 {
		return hxc_mul(lhs,rhs);
	}
	@:extern @:op(A*B) public static inline function mulI(lhs:Int64, rhs:Int):Int64 {
		return hxc_mul(lhs,rhs);
	}
	@:extern @:op(A/B) public static inline function divI(lhs:Int64, rhs:Int):Int64 {
		return hxc_div(lhs,rhs);
	}
	@:extern @:op(A/B) public static inline function div(lhs:Int64, rhs:Int64):Int64 {
		return hxc_div(lhs,rhs);
	}
	@:extern @:op(A<<B) public static inline function shlI(lhs:Int64, rhs:Int):Int64 {
		return hxc_shl(lhs,rhs);
	}
	@:extern @:op(A<<B) public static inline function shl(lhs:Int64, rhs:Int64):Int64 {
		return hxc_shl(lhs,rhs);
	}
	@:extern @:op(A>>B) public static inline function shrI(lhs:Int64, rhs:Int):Int64 {
		return hxc_shr(lhs,rhs);
	}
	@:extern @:op(A>>B) public static inline function shr(lhs:Int64, rhs:Int64):Int64 {
		return hxc_shr(lhs,rhs);
	}
	@:extern @:op(A&B) public static inline function bitand(lhs:Int64, rhs:Int64):Int64 {
		return hxc_and(lhs,rhs);
	}
	@:extern @:op(A^B) public static inline function xor(lhs:Int64, rhs:Int64):Int64 {
		return hxc_xor(lhs,rhs);
	}
	@:extern @:op(A|B) public static inline function bitor(lhs:Int64, rhs:Int64):Int64 {
		return hxc_or(lhs,rhs);
	}
	@:extern @:op(A<B) public static inline function lt(lhs:Int64, rhs:Int64):Bool {
		return cast hxc_lt(lhs,rhs);
	}
	@:extern @:op(A>B) public static inline function gt(lhs:Int64, rhs:Int64):Bool {
		return cast hxc_gt(lhs,rhs);
	}
	@:from public static inline function fromPointer<T>(p:Pointer<T>):Int64{
		return cast p;
	}
	@:to public inline function toPointer<T>():Pointer<T>{
		return untyped this;
	}
	@:extern public inline function value() {
		return this;
	}
}