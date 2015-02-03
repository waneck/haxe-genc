package c;

@:native("hx_char")
@:struct
@:coreType
abstract Char from Int to Int { }

@:native("hx_uchar")
@:struct
@:coreType
abstract UChar from Int to Int { }

@:native("hx_long")
@:struct
@:coreType
abstract Long from Int to Int { }

@:native("hx_ulong")
@:struct
@:coreType
abstract ULong from Int to Int { }

@:native("hx_short")
@:struct
@:coreType
abstract Short from Int to Int { }

@:native("hx_ushort")
@:struct
@:coreType
abstract UShort from Int to Int { }

@:native("hx_float32")
@:struct
@:coreType
abstract Float32 from Float to Float { }

@:native("hx_uint8")
@:keep
@:struct
@:coreType
abstract UInt8 from Int to Int { }

@:native("hx_int8")
@:struct
@:coreType
abstract Int8 from Int to Int { }


@:native("hx_uint16")
@:keep
@:struct
@:coreType
abstract UInt16 from Int to Int { }

@:native("hx_int16")
@:struct
@:coreType
abstract Int16 from Int to Int { }

@:native("hx_uint32")
@:keep
@:runtimeValue
@:struct
@:coreType
abstract UInt32 from Int to Int { }

@:native("hx_int32")
@:struct
@:coreType
abstract Int32 from Int to Int { }

@:native("hx_uint")
@:struct
@:coreType
abstract UInt from Int to Int { }

@:native("size_t")
@:struct
@:coreType
abstract SizeT from Int to Int { }
