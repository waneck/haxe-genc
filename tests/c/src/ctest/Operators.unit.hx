1 < 2 == true;
2 > 1 == true;
2 == 2 == true;
2 != 1 == true;

function test(i1:Int, i2:Int) {
	return i1 + i2;
}
var a = [99, 3, 7];
var i = 0;
test(a[i++], a[i++]) == 102;
i == 2;

test(a[i = 1], a[i = 2]) == 10;
i == 2;