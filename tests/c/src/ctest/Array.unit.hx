var a = [1, 2, 3];
a[0] == 1;
a[1] == 2;
a[2] == 3;

a[0] = 4;
a[0] == 4;

a.push(12);
a[3] == 12;
a.pop() == 12;
a.pop() == 3;
a.pop() == 2;
a.pop() == 4;

var a = ["foo", "bar"];
a.push("baz");
a.pop() == "baz";
a.pop() == "bar";
a.pop() == "foo";

var a = ArrayStruct.createWithSize(10);
a._count == 10;
for(i in -10...0)
{
	a.constArray[-i-1] = i;
	a.constArray[-i-1] == i;
}
a._count == 10;

var a1 = ["foo", "bar", "baz"];
var a2 = [];
for (e in a1) {
	a2.push(e);
}
a1 == ["foo", "bar", "baz"];
a2 == ["foo", "bar", "baz"];