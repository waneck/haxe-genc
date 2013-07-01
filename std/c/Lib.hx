package c;

import c.Pointer;
import c.Types;

extern class Lib {
	@:plain public static function sizeof<T>(c:TypeReference<T>):Int;

	@:plain public static function dereference<T>(ptr:Pointer<T>):T;
	
	@:plain public static function getAddress<T>(t:T):Pointer<T>;
	
	@:plain public static function alloca(i:Int):Pointer<Void>;
	
	@:plain public static function cCode(s:ConstPointer<Char>):Dynamic;
}