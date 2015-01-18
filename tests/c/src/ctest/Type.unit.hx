var cl = Type.resolveClass("Main");
cl != null;
Type.getClassName(cl) == "Main";

Type.resolveClass("Waneck12") == null;

var cl = Type.resolveClass("SomeClass");
cl != null;
var inst:SomeClass = Type.createEmptyInstance(cl);
inst != null;

var inst:SomeClass = Type.createInstance(cl, ["foo"]);
inst != null;
inst.state == "foo";

var cl = Type.resolveClass("E");
var cl2 = Type.getSuperClass(cl);
cl2 != null;
Type.getClassName(cl2) == "D";