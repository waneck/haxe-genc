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

var z = 0;
z++ == 0;
z == 1;
++z == 2;
z++;
z == 3;
++z;
z == 4;

(z += 3) == 7;

var x = 0;
var arr = [3];
arr[x++]++ == 3;
x == 1;
arr[0] == 4;
x = 0;
(arr[x++] += 3) == 7;
arr[0] == 7;

var a = [0];
a[0] += 1;
a[0] == 1;
a[0] += a[0] + 1;
a[0] == 3;