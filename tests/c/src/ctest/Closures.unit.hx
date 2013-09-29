var x = 12;
var y = 99;
var z = 9;
function makeBig(i:Int) {
	var z = 20;
	return {f1:i * 2, f2:y, f3:z};
}
var k = makeBig(x);
x == 12;
y == 99;
z == 9;
k.f1 == 24;
k.f2 == 99;
k.f3 == 20;

var f1 = makeBig;
f1(5).f1 == 10;
	
function filter(a:Array<Int>, f:Int->Bool) {
	var l = [];
	for( x in a )
		if( f(x) )
			l.push(x);
	return l;
}

var a = [4, 88, 9, 12, 3];
var a2 = filter(a, function(i) return i > 5);
a2[0] == 88;
a2[1] == 9;
a2[2] == 12;

var c = new SomeClass("foo");
var func = c.getClosure("456");
func("789") == "123456789";
var gc = c.getClosure;
var gc1 = gc("456");
var gc2 = gc1("789");
gc2 == "123456789";
gc("456")("789") == "123456789";

var old = haxe.Log.trace;
var buf = new StringBuf();
function newTrace(s,?p) {
	buf.add(s);
}
haxe.Log.trace = newTrace;
trace("foo");
trace("bar");
haxe.Log.trace = old;
buf.toString() == "foobar";

var s0 = "begin";
function f1(s1:String) {
	var s2 = "1";
	function f2(s3:String) {
		var s4 = "2";
		function f3(s5:String) {
			return s0 + s1 + s2 + s3 + s4 + s5;
		}
		return f3;
	}
	return f2;
}
var s = f1("foo")("bar")("end");
s == "beginfoo1bar2end";

var s0 = "foo";
function fAssign(s1:String) {
	s0 = s1;
}
s0 == "foo";
fAssign("bar");
s0 == "bar";