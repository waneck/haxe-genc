var i = 0;
while (i < 10) {
	i++;
}
i == 10;

i = 0;
do {
	i++;
} while(i < 10);
i == 10;

var k = 0;
i = 0;
while (i < 10) {
	i++;
	if (i & 1 == 0) continue;
	k++;
}
i == 10;
k == 5;


k = 0;
i = 0;
while (i < 10) {
	i++;
	if (i == 5) break;
	k++;
}
i == 5;
k == 4;

k = 0;
i = 0;
do {
	i++;
	if (i & 1 == 0) continue;
	k++;
} while (i < 10);
i == 10;
k == 5;


k = 0;
i = 0;
do {
	i++;
	if (i == 5) break;
	k++;
} while (i < 10);
i == 5;
k == 4;

k = 0;
i = 0;
while (i < 10) {
	while (k < 10) {
		k++;
		if (k == 5) break;
	}
	i++;
}
k == 10;
i == 10;