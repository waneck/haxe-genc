var x;
if (2 > 1) {
	x = 1;
} else {
	x = 2;
}
x == 1;

if (1 > 2) {
	x = 1;
} else {
	x = 2;
}
x == 2;

if (1 > 2) {
	x = 1;
} else if (2 > 1) {
	x = 2;
} else {
	x = 3;
}
x == 2;