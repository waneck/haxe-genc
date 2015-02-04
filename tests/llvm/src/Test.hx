class Test {
	static public function run() {
		TestLlvm.test();
		TestString.test();
		TestClass.test();
		TestNull.test();
		TestClosure.test();
		TestDefaultValues.test();
		TestArray.test();
		c.CStdio.printf("Done %i tests, %i failures", [Eq.numTests, Eq.numFailures]);
	}
}