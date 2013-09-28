var i = 0;
var r1 = null;
var r2 = null;
for (e in ["foo", "bar"]) {
	if (i == 0) r1 = e;
	else r2 = e;
	++i;
}
r1 == "foo";
r2 == "bar";