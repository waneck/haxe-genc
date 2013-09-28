function takeTypedef(t:MyTypedef) {
		return t.maybeGiven;
	}
	
function giveTypedef():MyTypedef {
	return { given: "foo" };
}


var s:Int = cast takeTypedef({given: "foo" });
s == 0;

takeTypedef({given: "foo", maybeGiven:"bar"}) == "bar";

var td:MyTypedef = { given: "foo" };
td.given == "foo";
var s:Int = cast td.maybeGiven;
s == 0;

td = { given: "foo" };
td.given == "foo";
var s:Int = cast td.maybeGiven;
s == 0;

var a:Array<MyTypedef> = [ {given: "foo" }];
a[0].given == "foo";
var s:Int = cast a[0].maybeGiven;
s == 0;

var o:{f:MyTypedef} = { f: { given: "foo" }};
o.f.given == "foo";
var s:Int = cast o.f.maybeGiven;
s == 0;

var td = giveTypedef();
td.given == "foo";
var s:Int = cast td.maybeGiven;
s == 0;