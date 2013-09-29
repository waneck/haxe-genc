var s = "";
try {
	try {
		
	} catch(err:Dynamic) {
		trace("fail");
	}
	throw "I was thrown today";
} catch(err:String) {
	s = err;
}
s == "I was thrown today";

try {
	throw 1;
} catch(e:String) {
	trace("Should not be entered...");
	throw false;
} catch(e:Dynamic) {
	s = "Caught dynamic";
}
s == "Caught dynamic";

var s2 = "";
try {
	try {
		throw "exc1";
	} catch(e:Dynamic) {
		throw "exc2";
	}
} catch(s:String) {
	s2 = s;
}
s2 == "exc2";