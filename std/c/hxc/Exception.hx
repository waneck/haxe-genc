package c.hxc;

import c.Pointer;

@:native("jmp_buf")
extern private class JmpBuf {
	public function new():Void;
}

@:keep
class Exception {
	static var stack:Array<Pointer<JmpBuf>> = new Array();
	static var thrownObject:Dynamic;

	static public function push():JmpBuf untyped {
		__c("jmp_buf* buf = (jmp_buf*) malloc(sizeof(jmp_buf))");
		stack.push(buf);
		return buf;
	}

	static public function pop():JmpBuf {
		return untyped stack.pop();
	}

	static public function peek():JmpBuf {
		return untyped stack[stack.length - 1];
	}
}
