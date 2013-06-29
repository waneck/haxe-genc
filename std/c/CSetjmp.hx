package c;

import c.Types;
import c.Pointer;

@:include("<setjmp.h>")
@:native("jmp_buf")
@:struct
extern class JmpBuf {
	public function new():Void;
}

@:include("<setjmp.h>")
extern class CSetjmp {
	@:plain static public function setjmp(buf:JmpBuf):Int;
	@:plain static public function longjmp(env:JmpBuf, val:Int):Void;
}