var x;
var two = 2;
var one = 1;
if (two > one) {
	x = 1;
} else {
	x = 2;
}
x == 1;

if (one > two) {
	x = 1;
} else {
	x = 2;
}
x == 2;

if (one > two) {
	x = 1;
} else if (two > one) {
	x = 2;
} else {
	x = 3;
}
x == 2;