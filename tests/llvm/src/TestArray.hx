@:build(TestHelper.build())
class TestArray {
	static function testBasic() {
		var a = [3, 4, 5];
		3 == a.length;
		3 == a[0];
		4 == a[1];
		5 == a[2];

		a.push(6);
		4 == a.length;
		3 == a[0];
		4 == a[1];
		5 == a[2];
		6 == a[3];

		a.unshift(2);
		5 == a.length;
		2 == a[0];
		3 == a[1];
		4 == a[2];
		5 == a[3];
		6 == a[4];

		a.insert(0, 1);
		6 == a.length;
		1 == a[0];
		2 == a[1];
		3 == a[2];
		4 == a[3];
		5 == a[4];
		6 == a[5];

		var a2 = a.concat([8, 7]);
		6 == a.length;
		1 == a[0];
		2 == a[1];
		3 == a[2];
		4 == a[3];
		5 == a[4];
		6 == a[5];

		8 == a2.length;
		1 == a2[0];
		2 == a2[1];
		3 == a2[2];
		4 == a2[3];
		5 == a2[4];
		6 == a2[5];
		8 == a2[6];
		7 == a2[7];

		6 == a2.indexOf(8);
		6 == a2.indexOf(8, 2);
		6 == a2.indexOf(8, 6);
		-1 == a2.indexOf(8, 7);

		6 == a2.lastIndexOf(8);
		-1 == a2.lastIndexOf(8, 2);
		6 == a2.lastIndexOf(8, 6);
		6 == a2.lastIndexOf(8, 7);

		true == a2.remove(8);
		7 == a2.length;
		1 == a2[0];
		2 == a2[1];
		3 == a2[2];
		4 == a2[3];
		5 == a2[4];
		6 == a2[5];
		7 == a2[6];

		7 == a2.pop();
		6 == a2.length;
		1 == a2[0];
		2 == a2[1];
		3 == a2[2];
		4 == a2[3];
		5 == a2[4];
		6 == a2[5];

		1 == a2.shift();
		5 == a2.length;
		2 == a2[0];
		3 == a2[1];
		4 == a2[2];
		5 == a2[3];
		6 == a2[4];

		a2.reverse();
		5 == a2.length;
		6 == a2[0];
		5 == a2[1];
		4 == a2[2];
		3 == a2[3];
		2 == a2[4];
	}

	static function testString() {
		var a = ["3", "4", "5"];
		3 == a.length;
		"3" == a[0];
		"4" == a[1];
		"5" == a[2];

		a.push("6");
		4 == a.length;
		"3" == a[0];
		"4" == a[1];
		"5" == a[2];
		"6" == a[3];

		a.unshift("2");
		5 == a.length;
		"2" == a[0];
		"3" == a[1];
		"4" == a[2];
		"5" == a[3];
		"6" == a[4];

		a.insert(0, "1");
		6 == a.length;
		"1" == a[0];
		"2" == a[1];
		"3" == a[2];
		"4" == a[3];
		"5" == a[4];
		"6" == a[5];

		var a2 = a.concat(["8", "7"]);
		6 == a.length;
		"1" == a[0];
		"2" == a[1];
		"3" == a[2];
		"4" == a[3];
		"5" == a[4];
		"6" == a[5];

		8 == a2.length;
		"1" == a2[0];
		"2" == a2[1];
		"3" == a2[2];
		"4" == a2[3];
		"5" == a2[4];
		"6" == a2[5];
		"8" == a2[6];
		"7" == a2[7];

		6 == a2.indexOf("8");
		6 == a2.indexOf("8", 2);
		6 == a2.indexOf("8", 6);
		-1 == a2.indexOf("8", 7);

		6 == a2.lastIndexOf("8");
		-1 == a2.lastIndexOf("8", 2);
		6 == a2.lastIndexOf("8", 6);
		6 == a2.lastIndexOf("8", 7);

		true == a2.remove("8");
		7 == a2.length;
		"1" == a2[0];
		"2" == a2[1];
		"3" == a2[2];
		"4" == a2[3];
		"5" == a2[4];
		"6" == a2[5];
		"7" == a2[6];

		"7" == a2.pop();
		6 == a2.length;
		"1" == a2[0];
		"2" == a2[1];
		"3" == a2[2];
		"4" == a2[3];
		"5" == a2[4];
		"6" == a2[5];

		"1" == a2.shift();
		5 == a2.length;
		"2" == a2[0];
		"3" == a2[1];
		"4" == a2[2];
		"5" == a2[3];
		"6" == a2[4];

		a2.reverse();
		5 == a2.length;
		"6" == a2[0];
		"5" == a2[1];
		"4" == a2[2];
		"3" == a2[3];
		"2" == a2[4];
	}
}