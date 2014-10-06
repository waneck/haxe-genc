import c.ConstSizeArray;

@:analyzer(no_simplification)
class ArrayStruct
{
	public var _count:Int; //hack so _count can be sorted first
	public var constArray:ConstSizeArray<Int,1>;

	public static function createWithSize(size:Int):ArrayStruct
	{
		if (size <= 0) throw "<=";
		var ret:ArrayStruct = cast c.CStdlib.calloc(1, 4 + 4 * size ); //sorry to hardcode that. sizeof(ArrayStruct) would give the size of the pointer
		ret._count = size;
		return ret;
	}
}
