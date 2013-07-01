package c;

import c.Exception;
import c.TypeReference;
import c.Types;

@:headerCode('
#ifndef _MSC_VER
	#define ALLOCA(n) alloca(n)
#else
	#define ALLOCA(n) _alloca(n)
#endif

typedef unsigned char hx_uchar;
typedef char hx_char;
typedef unsigned int hx_uint;
typedef unsigned char hx_uint8;
typedef char hx_int8;
typedef unsigned long hx_uint32;
typedef long hx_int32;
typedef unsigned long long hx_uint64;
typedef long long hx_int64;
#define hx_strcmp(a,b) ((a == 0 && b == 0) ? 0 : (a == 0 || b == 0) ? 1 : String_compare(a,b))

')
@:keep @:native('hxc') class Boot {
	
	static public var mainFunc:Void->Void;
	static public var argc:Int;
	static public var argv:Pointer<Pointer<Char>>;
	
	@:plain static public function main(argc:Int, argv:Pointer<Pointer<Char>>):Int {
		Boot.argc = argc;
		Boot.argv = argv;
		untyped __c("_hx_init()");
		try {
			mainFunc();
			return 0;
		} catch(e:Dynamic) {
			trace("Something went wrong");
			return 1;
		}
		
	}
}