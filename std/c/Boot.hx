package c;

import c.Exception;
import c.TypeReference;

@:headerCode('
#ifndef _MSC_VER
	#define ALLOCA(n) alloca(n)
#else
	#define ALLOCA(n) _alloca(n)
#endif

typedef unsigned char uint8;
')
@:keep @:native('hxc') class Boot
{
}

@:native("uint8")
@:coreType
abstract UInt8 from Int to Int {
	
}