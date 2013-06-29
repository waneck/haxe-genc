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
')
@:keep @:native('hxc') class Boot
{
}