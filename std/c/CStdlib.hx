package c;

import c.Types;
import c.Pointer;

typedef DivT = {
	quot:Int,
	rem:Int
}

typedef LDivT = {
	quot:Int32,
	rem:Int32
}

@:include("<stdlib.h>")
extern class CStdlib {

	@:plain static public var EXIT_FAILURE:Int;
	@:plain static public var EXIT_SUCCESS:Int;
	@:plain static public var MB_CUR_MAX:Int;
	@:plain static public var NULL:Int;
	@:plain static public var RAND_MAX:Int;

	@:plain static public function atof(str:ConstPointer<Int8>):Float;
	@:plain static public function atoi(str:ConstPointer<Int8>):Int;
	@:plain static public function atol(str:ConstPointer<Int8>):Int32;
	@:plain static public function strtod(str:ConstPointer<Int8>, endptr:Pointer<Pointer<UInt8>>):Float;
	@:plain static public function strtol(str:ConstPointer<Int8>, endptr:Pointer<Pointer<UInt8>>, base:Int):Int32;
	@:plain static public function strtoul(str:ConstPointer<Int8>, endptr:Pointer<Pointer<UInt8>>, base:Int):UInt32;

	@:plain static public function rand():Int;
	@:plain static public function srand(seed:UInt):Void;

	@:plain static public function calloc(num:SizeT, size:SizeT):Pointer<Void>;
	@:plain static public function free(ptr:Pointer<Void>):Void;
	@:plain static public function malloc(size:SizeT):Pointer<Void>;
	@:plain static public function realloc(ptr:Pointer<Void>, size:SizeT):Pointer<Void>;

	@:plain static public function abort():Void;
	@:plain static public function atexit(func:Void -> Void):Int;
	@:plain static public function exit(status:Int):Void;
	@:plain static public function getenv(name:ConstPointer<Int8>):Pointer<Int8>;
	@:plain static public function system(command:ConstPointer<Int8>):Int;

	@:plain static public function bsearch(key:ConstPointer<Void>, base:ConstPointer<Void>, num:SizeT, size:SizeT, compar:Pointer<Void> -> Pointer<Void> -> Int):Pointer<Void>;
	@:plain static public function qsort(base:Pointer<Void>, num:SizeT, size:SizeT, compar:Pointer<Void> -> Pointer<Void> -> Int):Void;

	@:plain static public function abs(n:Int):Int;
	@:plain static public function div(numer:Int, denom:Int):DivT;
	@:plain static public function labs(n:Int32):Int32;
	@:plain static public function ldiv(numer:Int32, denom:Int32):LDivT;

	// TODO: Multibyte
}