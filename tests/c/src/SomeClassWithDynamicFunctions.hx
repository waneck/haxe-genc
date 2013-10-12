class SomeClassWithDynamicFunctions {

	static var staticState = "Static State";
	
	dynamic public static function staticDyn(passedVar:String) {
		return 'staticDyn($staticState, $passedVar)';
	}
	
	var memberState = "Member State";
	
	dynamic public function memberDyn(passedVar:String) {
		return 'memberDyn($memberState, $passedVar)';
	}
	
	public function new() {
		
	}
	
}