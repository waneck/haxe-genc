Main.optArg("foo") == "foobaz12";
Main.optArg("foo", "bar") == "foobar12";
Main.optArg("foo", 1) == "foobaz1";
Main.optArg("foo", "bar", 1) == "foobar1";
Main.optArg("foo", null, null) == "foobaz12";