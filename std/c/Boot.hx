package c;

import c.Exception;
import c.TypeReference;
import c.Types;
import c.NInt;
import String;
import c.Closure;
import c.VTable;
#if !llvm
import c.gc.GC;
import c.gc.Alloc;
#end

@:headerCode('

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _MSC_VER
	#define ALLOCA(n) alloca(n)
#else
	#define ALLOCA(n) _alloca(n)
#endif

typedef unsigned char hx_uchar;
typedef signed char hx_char;
typedef unsigned short hx_ushort;
typedef signed short hx_short;
typedef unsigned int hx_uint;
typedef unsigned char hx_uint8;
typedef signed char hx_int8;
typedef unsigned short int hx_uint16;
typedef signed short int hx_int16;
typedef unsigned long hx_uint32;
typedef signed long hx_int32;
typedef unsigned long long hx_uint64;
typedef signed long long hx_int64;
typedef int Date;
')
@:keep
@:native('hxc')
class Boot {
	static public var argc:Int;
	static public var argv:Pointer<Pointer<Char>>;
	static public var typeReferences:Array<c.TypeReference<Dynamic>>;

	static public function registerType(typeref:TypeReference<Dynamic>) {
		typeReferences.push(typeref);
	}

	@:plain static public function main(argc:Int, argv:Pointer<Pointer<Char>>):Int {
		Boot.argc = argc;
		Boot.argv = argv;
		typeReferences = [];
		#if llvm
		c.Lib.initializeStatics();
		#else
		c.Init._hx_init();
		#end
		try {
			c.Lib.callMain();
			return 0;
		} catch(e:Dynamic) {
			trace(Std.string(e));
			return 1;
		}
	}

	//@:plain static function personality(data:Exc) {
		//return 0;
	//}
}

//typedef Exc = {
	//i1: Int,
	//i2: Int,
	//i3: c.NInt.Int64,
	//p1: Pointer<Char>,
	//p2: Pointer<Char>
//}
