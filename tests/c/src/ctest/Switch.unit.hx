function switchOnInt(i) {
		return switch(i) {
			case 1: 1;
			case 2: 2;
			case _: -1;
		}
	}
	
function switchOnString(s) {
	return switch(s) {
		case "foo": 1;
		case "bar": 2;
		case _: -1;
	}
}

switchOnInt(1) == 1;
switchOnInt(2) == 2;
switchOnInt(3) == -1;
	
var i = 0;
while(true) {
	switch(i) {
		case 10: break;
		case _: ++i;
	}
}
i == 10;

switchOnString("foo") == 1;
switchOnString("bar") == 2;
switchOnString("baz") == -1;

var e = macro "foo" + "bar";
var r1 = "";
var r2 = "";
switch(e.expr) {
	case _ if (2 > 20):
		trace("fail guard");
	case EBinop(OpAdd,{ expr: EConst(CString(s1))}, {expr: EConst(CString(s2)) }):
		r1 = s1;
		r2 = s2;
	case _:
		f(true);
}
r1 == "foo";
r2 == "bar";