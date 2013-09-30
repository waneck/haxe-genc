package c;

import c.Pointer;
import c.Types;

extern class Lib {
	public static function sizeof<T>(c:TypeReference<T>):Int;

	public static function dereference<T>(ptr:Pointer<T>):T;
	
	public static function getAddress<T>(t:T):Pointer<T>;
	
	public static function alloca(i:Int):Pointer<Void>;
	
	public static function cCode(s:ConstPointer<Char>):Dynamic;
	
	public static function callMain():Void;
}