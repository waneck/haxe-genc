package c.gc;

import c.Pointer;
import c.NInt.Int64;
import c.NInt.UInt64;
import c.gc.Alloc;

@:enum 
abstract TP_SIZE_SHIFT(Int) {
    var sz08 = 0;
    var sz16 = 1;
    var sz32 = 2;
    var sz64 = 3;
    var ____ = -1;
}


@:enum 
abstract TP_KIND<T>(Int) {
    
    public function new () this = 0;
    
    var TP_REF = 0;
    var TP_V08 = 1;
    var TP_V16 = 2;
    var TP_V32 = 3;
    var TP_V64 = 4;
    var TP_UNKNOWN = 5;
    var ____ = -1;
}

@:coreType
abstract TP_TYPE {
    @:to public function get<T>():TP_KIND<T> return TP_UNKNOWN;
}

@:keep
class GC {
    
    public static var TP:TP_TYPE;
    
    static function obj_tp_info(p:Pointer<UInt64>):UInt64 {
        return p[0] >> 16;
    }
    
    static function tp_known(value:UInt64,dest_pos:Int){
        return value << (dest_pos*3);
    }
    
    static function tp_by_pos_lt(source:UInt64,source_pos:Int,dest_pos:Int):UInt64 {
        return (7 << (dest_pos*3)) & ( source << ((dest_pos-source_pos)*3));
    }
    
    static function tp_by_pos_gt(source:UInt64,source_pos:Int,dest_pos:Int):UInt64 {
        return (7 << (dest_pos*3)) & ( source >> ((source_pos-dest_pos)*3));
    }
    
    static function tp_by_pos_eq(source:UInt64,source_pos:Int,dest_pos:Int):UInt64 {
        return (7 << (dest_pos*3)) & source;
    }
    
    
    public static var main_area:Area = Area.create(0);
    
    inline static function alloc(sz:Int):Pointer<Void> return main_area.alloc(sz);
    
    inline static function free(p:Pointer<Void>):Void main_area.free(p);
    
	inline static function tp_size_shift():TP_SIZE_SHIFT return sz08;
}
