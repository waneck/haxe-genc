var x = null;
optArg("foo") == "foobaz12";
optArg("foo", "bar") == "foobar12";
optArg("foo", 1) == "foobaz1";
optArg("foo", "bar", 1) == "foobar1";
optArg("foo", null, null) == "foobaz12";
optArg("foo", x, null) == "foobaz12";