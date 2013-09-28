package c;

import c.CSetjmp;
import c.Pointer;
import haxe.ds.GenericStack;

@:keep
class Exception {
	static var stack:GenericStack<Pointer<JmpBuf>> = new GenericStack<Pointer<JmpBuf>>();
	static var thrownObject:Dynamic;

	static public function push():Pointer<JmpBuf> {
		var buf:Pointer<JmpBuf> = cast c.CStdlib.malloc(Lib.sizeof(new TypeReference<JmpBuf>()));
		stack.add(buf);
		return buf;
	}

	static public function pop():Pointer<JmpBuf> {
		return stack.pop();
	}

	static public function peek():Pointer<JmpBuf> {
		return stack.first();
	}
}
