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
td.maybeGiven == null;

td = { given: "foo" };
td.given == "foo";
td.maybeGiven == null;

var a:Array<MyTypedef> = [ {given: "foo" }];
a[0].given == "foo";
a[0].maybeGiven == null;

var o:{f:MyTypedef} = { f: { given: "foo" }};
o.f.given == "foo";
o.f.maybeGiven == null;

var td = giveTypedef();
td.given == "foo";
td.maybeGiven == null;