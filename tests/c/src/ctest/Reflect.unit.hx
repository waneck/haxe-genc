var c = new SomeClass("bar");
var d:Dynamic = c;
Reflect.field(d, "state") == "bar";
Reflect.setField(d, "bar", "foo");
Reflect.field(d, "bar") == "foo";