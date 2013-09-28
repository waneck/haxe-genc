var a:UInt8 = 1;
var b:Int8 = 1;

var i = 12;
var ptr = c.Lib.getAddress(i);
var i2 = c.Lib.dereference(ptr);
i == 12;
i2 == 12;
c.Lib.dereference(c.Lib.getAddress(c.Lib.dereference(c.Lib.getAddress(i)))) == 12;