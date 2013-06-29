package c;

import c.CSetjmp;
import c.Pointer;

@:keep
class Exception {
	static var stack:Array<Pointer<JmpBuf>> = new Array();
	static var thrownObject:Dynamic;

	static public function push():Pointer<JmpBuf> {
		var buf:Pointer<JmpBuf> = cast c.CStdlib.malloc(Lib.sizeof(new TypeReference<JmpBuf>()));
		stack.push(buf);
		return buf;
	}

	static public function pop():Pointer<JmpBuf> {
		return stack.pop();
	}

	static public function peek():Pointer<JmpBuf> {
		return stack[stack.length - 1];
	}
}
