package c.gc;
import c.NInt;
import c.Types;

@:keep
@:hxcNoHeader
class GenericHeader {
	public var header:UInt64;
	public var tparms:UInt64;
}

@:keep
@:hxcNoHeader
class EnumHeader {
	var gc_id:UInt16;
	var tp_id:UInt16;
	var enum_idx:UInt32;
	// ----  64
	var tp_info:UInt64;
	// ----  128
}

@:keep
@:hxcNoHeader
class ClassHeader {
	var gc_id:UInt16;
	var tp_id:UInt16;
	var st_group:UInt16;
	var st_id:UInt16;
	// ----  64
	var tp_info:UInt64;
	// ---- 128
	var i_group:UInt16;
	var i_id:UInt16;
	// ---- 160
	var vtable:UInt32;
	// ---- 192
}

/* Use this in case there are no interfaces and vtables */
@:keep
@:hxcNoHeader
class ClassSmallHeader {

	var gc_id:UInt16;
	var tp_id:UInt16;
	
	var st_group:UInt16;
	var st_id:UInt16;
	// ----  64
	var tp_info:UInt64;
	// ---- 128
}

@:keep
@:hxcNoHeader
class AnonHeader {
	var gc_id:UInt16;
	var tp_id:UInt16;

	var st_group:UInt16;
	var st_id:UInt16;
	var tp_info:UInt64;
	// ---- 128
	var i_group:UInt16;
	var i_id:UInt16;
	// ---- 160
}

@:keep
@:hxcNoHeader
class Templates {

	function foo( X : UInt64 )  ( 1 << X );
	function bar( X : UInt64, Y : UInt64)  ( 1 << X );
}

