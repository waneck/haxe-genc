var c = new SomeClass("bar");
c.state == "bar";
var d:Dynamic = c;
d.state = "foo";
d.state == "foo";
c.state == "foo";
d.state2 = "bar";
d.state2 == "bar";

var a =  { bar: 12 };
var d:Dynamic = a;
d.bar == 12;
d.bar = 13;
d.bar == 13;
d.bar2 = 99;
d.bar2 == 99;