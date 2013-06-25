package c;

import c.Exception;
import c.TypeReference;

@:headerCode('
#ifndef _MSC_VER
	#define ALLOCA(n) alloca(n)
#else
	#define ALLOCA(n) _alloca(n)
#endif
')
@:keep @:native('hxc') class Boot
{
}
