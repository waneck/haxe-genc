var cl = Type.resolveClass("Main");
cl != null;
Type.getClassName(cl) == "Main";

Type.resolveClass("Waneck12") == null;

var cl = Type.resolveClass("SomeClass");
cl != null;
//var inst = Type.createEmptyInstance(cl);
//inst != null;

var cl = Type.resolveClass("E");
var cl2 = Type.getSuperClass(cl);
cl2 != null;
Type.getClassName(cl2) == "D";