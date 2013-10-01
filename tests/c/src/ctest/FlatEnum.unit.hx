var a = MyFlatEnum.A;
a == MyFlatEnum.A;
var r = "";
switch(a) {
	case A: r = "foo";
	case _: r = "bar";
}
r == "foo";