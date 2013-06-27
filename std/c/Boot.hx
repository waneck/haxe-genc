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

typedef unsigned char hx_uint8;
typedef char hx_int8;
')
@:keep @:native('hxc') class Boot
{
}