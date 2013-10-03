package c;

import c.ConstSizeArray;
import c.Pointer;

@:keep
class VTable {
	
	var slots:c.ConstSizeArray<FunctionPointer<Pointer<Void>->Void>,1>;
	
}