SomeClassWithDynamicFunctions.staticDyn("foo") == "staticDyn(Static State, foo)";
SomeClassWithDynamicFunctions.staticDyn = function(v) {
	return 'Custom Func($v)';
}
SomeClassWithDynamicFunctions.staticDyn("foo") == "Custom Func(foo)";

var inst = new SomeClassWithDynamicFunctions();
inst.memberDyn("bar") == "memberDyn(Member State, bar)";
inst.memberDyn = SomeClassWithDynamicFunctions.staticDyn;
inst.memberDyn("bar") == "Custom Func(bar)";