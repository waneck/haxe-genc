package c;

@:include("<signal.h>")
extern class CSignal {
	@:plain static public var SIGABRT:Int;
	@:plain static public var SIGFPE:Int;
	@:plain static public var SIGILL:Int;
	@:plain static public var SIGINT:Int;
	@:plain static public var SIGSEGV:Int;
	@:plain static public var SIGTERM:Int;
	
	@:plain static public function signal(sig:Int, handler:Int->Void):Void;
	@:plain static public function raise(sig:Int):Int;
}