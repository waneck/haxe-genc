var s = new String("my string");
s.toString() == s;

var s = "foo";
s = s + "bar";
s == "foobar";
s += "baz";
s == "foobarbaz";