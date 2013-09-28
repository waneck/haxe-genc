class SomeClass {
	public var state:String;
	
	public function new(s:String) {
		state = s;
	}
	
	public function getState() {
		return state;
	}
	
	public function setState(s) {
		return this.state = s;
	}
	
	var x = "123";
	public function getClosure(y:String) {
		var f = function(z) {return x+y+z;}
		return f;
	}

	public function invoke(f) {
		return f("678");
	}
}