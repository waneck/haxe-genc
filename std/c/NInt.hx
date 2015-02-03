
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
    @:extern @:op(A%B) public static function mod(lhs:Int64, rhs:Int64):Int64;
    @:extern @:op(A%B) public static function modI(lhs:Int64, rhs:Int):Int64;
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
	@:extern public inline function float():Float {
        return cast this;
    }
}
@:coreType
@:int("uint64")
abstract UInt64 from Int {
    public function new(v:Int){
        this = v;
    }

    @:extern @:op(A+B) public static function addI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A+B) public static function add(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A-B) public static function subI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A-B) public static function sub(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A*B) public static function mulI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A*B) public static function mul(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A/B) public static function divI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A/B) public static function div(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A<<B) public static function shlI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A<<B) public static function shl(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A>>B) public static function shrI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A>>B) public static function shr(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A^B) public static function xorI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A^B) public static function xor(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A&B) public static function andI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A&B) public static function and(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A|B) public static function orI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A|B) public static function or(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A%B) public static function mod(lhs:UInt64, rhs:UInt64):UInt64;
    @:extern @:op(A%B) public static function modI(lhs:UInt64, rhs:Int):UInt64;
    @:extern @:op(A>B) public static function gtI(lhs:UInt64, rhs:Int):Bool;
    @:extern @:op(A>B) public static function gt(lhs:UInt64, rhs:UInt64):Bool;
    @:extern @:op(A<B) public static function ltI(lhs:UInt64, rhs:Int):Bool;
    @:extern @:op(A<B) public static function lt(lhs:UInt64, rhs:UInt64):Bool;
    @:extern @:op(A>=B) public static function gteI(lhs:UInt64, rhs:Int):Bool;
    @:extern @:op(A>=B) public static function gte(lhs:UInt64, rhs:UInt64):Bool;
    @:extern @:op(A<=B) public static function lteI(lhs:UInt64, rhs:Int):Bool;
    @:extern @:op(A<=B) public static function lte(lhs:UInt64, rhs:UInt64):Bool;
    @:extern public inline function value() {
        return cast this;
    }
    @:extern public inline function int32():Int {
        return cast this;
    }
    @:extern public inline function pointer<T>():Pointer<T> {
        return cast this;
    }
    @:extern public inline function float():Float {
        return cast this;
    }
}

/*
@:coreType
@:int("uint32")
abstract UInt32 from Int to Int {}

@:coreType
@:int("uint16")
abstract UInt16 from Int to Int {}

@:coreType
@:int("uint8")
abstract UInt8 from Int to Int {}
*/
