package c;

import c.Types;
import c.Pointer;

@:include("<ctype.h>")
extern class CCtype {
	@:plain static public function isalnum(ch:Int):Int;
	@:plain static public function isalpha(ch:Int):Int;
	@:plain static public function iscntrl(ch:Int):Int;
	@:plain static public function isdigit(ch:Int):Int;
	@:plain static public function isgraph(ch:Int):Int;
	@:plain static public function islower(ch:Int):Int;
	@:plain static public function isprint(ch:Int):Int;
	@:plain static public function ispunct(ch:Int):Int;
	@:plain static public function isspace(ch:Int):Int;
	@:plain static public function isupper(ch:Int):Int;
	@:plain static public function isxdigit(ch:Int):Int;
	@:plain static public function tolower(ch:Int):Int;
	@:plain static public function toupper(ch:Int):Int;
}