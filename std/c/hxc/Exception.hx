package c.hxc;

@:native("jmp_buf")
extern private class JmpBuf {
	public function new():Void;
}

@:keep
class Exception {
	static var stack:JmpBuf;
	static var thrownObject:Dynamic;
	
	static public function push():JmpBuf untyped {
		__c("jmp_buf* buf = (jmp_buf*) malloc(sizeof(jmp_buf))");
		if (stack == NULL) stack = g_queue_new();
		g_queue_push_head(stack, buf);
		return buf;
	}
	
	static public function pop():JmpBuf {
		return untyped g_queue_pop_head(stack);
	}
	
	static public function peek():JmpBuf {
		return untyped g_queue_peek_head(stack);
	}
}