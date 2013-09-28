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