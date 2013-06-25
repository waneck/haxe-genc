package c;

import c.Pointer;

@:include("<setjmp.h>")
@:native("jmp_buf")
@:struct
extern private class JmpBuf {
	public function new():Void;
}

@:keep
class Exception {
	static var stack:Array<Pointer<JmpBuf>> = new Array();
	static var thrownObject:Dynamic;

	static public function push():Pointer<JmpBuf> untyped {
		var buf:Pointer<JmpBuf> = untyped __c("(jmp_buf*) malloc(sizeof(jmp_buf))");
		stack.push(untyped buf);
		return buf;
	}

	static public function pop():Pointer<JmpBuf> {
		return untyped stack.pop();
	}

	static public function peek():Pointer<JmpBuf> {
		return untyped stack[stack.length - 1];
	}
}
