
package c;

@:coreType
@:int("int64")
abstract Int64 from Int {
	public function new(v:Int){
		this = v;
	}
	@:op(A+B) public static function addI(lhs:Int64, rhs:Int):Int64;
	@:op(A+B) public static function add(lhs:Int64, rhs:Int64):Int64;
	@:op(A-B) public static function subI(lhs:Int64, rhs:Int):Int64;
	@:op(A-B) public static function sub(lhs:Int64, rhs:Int64):Int64;
	@:op(A/B) public static function divI(lhs:Int64, rhs:Int):Int64;
	@:op(A/B) public static function div(lhs:Int64, rhs:Int64):Int64;
	@:op(A<<B) public static function shlI(lhs:Int64, rhs:Int):Int64;
	@:op(A<<B) public static function shl(lhs:Int64, rhs:Int64):Int64;
	@:op(A>>B) public static function shrI(lhs:Int64, rhs:Int):Int64;
	@:op(A>>B) public static function shr(lhs:Int64, rhs:Int64):Int64;
	public inline function value() {
		return this;
	}
}