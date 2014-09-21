package c.gc;

import c.Pointer;
import c.NInt.Int64;
import c.NInt.UInt64;
import c.gc.Alloc;
@:keep
class GC {
    
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
    
    
    static var main_area:Area = Area.create(0);
    
    inline static function alloc(sz:Int):Pointer<Void> return main_area.alloc(sz);
    
    inline static function free(p:Pointer<Void>):Void main_area.free(p);
    
}
