package c.gc;

import c.ConstSizeArray;
import c.Pointer;
import c.NInt.Int64;
import c.Types;
import c.Lib.dereference;
import c.CStdio.*;
import c.CStdlib.*;
import c.Struct;

import c.gc.Alloc.Utils.*;
import c.gc.Alloc.MMan.*;

#if !winapi
@:include("<sys/mman.h>")
extern class MMan {

    @:plain static var PROT_EXEC;
    @:plain static var PROT_READ;
    @:plain static var PROT_WRITE;
    @:plain static var PROT_NONE;

    @:plain static var MAP_SHARED;
    @:plain static var MAP_PRIVATE;

    @:plain static var MAP_ANONYMOUS;
    @:plain static var MAP_DENYWRITE;
    @:plain static var MAP_EXECUTABLE;
    @:plain static var MAP_FIXED;
    @:plain static var MAP_HUGETLB;
    @:plain static var MAP_LOCKED;
    @:plain static var MAP_NORESERVE;
    @:plain static var MAP_POPULATE;
    @:plain static var MAP_UNINITIALIZED;

    @:plain static var MAP_FAILED;
    
    @:plain
    static function mmap(addr:Pointer<Void>,size:Int64,prot:Int,flags:Int,fd:Int,offset:Int64):Pointer<Void>;

    @:plain static inline var MEM_RESERVE = 0;
    @:plain static inline var MEM_COMMIT = 1;
    @:plain static inline var PAGE_READWRITE = 0;

    static inline function mcommit(addr:Pointer<Void>, size:Int64, op:Int, flags:Int):Pointer<Void> {
        //trace(size);
        return addr;
    }

}
#elseif winapi
@:include("windows.h")
@:include("<sys/stat.h>")
extern class MMan {
    @:plain static var MEM_RESERVE;
    @:plain static var MEM_COMMIT;
    @:plain static var PAGE_READWRITE;

    @:plain
    static function VirtualAlloc(addr:Pointer<Void>, size:Int64, op:Int, flags:Int):Pointer<Void>;

    static inline function mcommit(addr:Pointer<Void>, size:Int64, op:Int, flags:Int):Pointer<Void>{
        return VirtualAlloc(addr, size, op, flags);
    }
}
#end

@:publicFields
class Utils {

    static function __init__(){
        c.Lib.cCode("
#define LT(n) n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n

    unsigned char logtable_arr[] = { 
    -1, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
    LT(4), LT(5), LT(5), LT(6), 
    LT(6), LT(6), LT(6), LT(7), 
    LT(7), LT(7), LT(7), LT(7), 
    LT(7), LT(7), LT(7) };
    memcpy(&c_gc_Utils_logtable,&logtable_arr,256);
");
    }
    
    static inline function div(l:Int,r:Int):Int return Std.int(l/r);

    static inline var one:Int64 = (1 : Int64);

    static var logtable:ConstSizeArray<UChar,256>;

    static function log2_64(v:Int64):Int{
        var r:UInt;
        var t,tt,ttt:UInt;
        if ( (ttt = (v >> 32).int32()) != 0) {
            if ((tt = (ttt >> 16)) != 0) {
                r = ((t = (tt >> 8)) != 0) ? 56 + logtable[t] : 48 + logtable[tt];
            } else {
                r = ((t = (ttt >> 8)) != 0) ? 40 + logtable[t] : 32 + logtable[ttt & 0xff];
            }
        } else if ((tt = (v >> 16).int32())  != 0 ) {
            r = ((t = tt >> 8)  != 0) ? 24 + logtable[t] : 16 + logtable[tt];
        } else {
            r = ((t = (v >> 8).int32()) != 0) ? 8 + logtable[t] : logtable[v.int32()];
        }
        return r;
    }

    ;
    static function log2_32(v:Int):Int {
        var r:Int;
        var t,tt:Int;
        if ((tt = (v >> 16))  != 0 ) {
            r = ((t = tt >> 8)  != 0) ? 24 + logtable[t] : 16 + logtable[tt];
        } else {
            r = ((t = (v >> 8)) != 0) ? 8 + logtable[t] : logtable[v];
        }
        return r;
    }

    static function next_pow2_log2_32(v:Int):Int {
        var r:Int;
        var t,tt:Int;
        if ((tt = (v >> 16))  != 0 ) {
            r = ((t = tt >> 8)  != 0) ? 24 + logtable[t] : 16 + logtable[tt];
        } else {
            r = ((t = (v >> 8)) != 0) ? 8 + logtable[t] : logtable[v];
        }
        r += (((1 << r)-1) & v != 0) ? 1 : 0; // round up to next power of two
        return r;
    }
    
    static function test_log2_64(){
        var b63 = (1 : Int64) << 63;
        var b62 = (1 : Int64) << 62;
        var b61 = (1 : Int64) << 61;
        var b33 = (1 : Int64) << 33;
        var b32 = (1 : Int64) << 31;
        var b14 = (1 : Int64) << 14;
        var r63 = log2_64(b63),
            r62 = log2_64(b62),
            r61 = log2_64(b61),
            r33 = log2_64(b33),
            r32 = log2_64(b32),
            r14 = log2_64(b14);
        printf("%d,%d,%d,%d,%d,%d :: %d\\n",[r63,r62,r61,r33,r32,r14,logtable[128]]);
    }
}

/*
 *  The FreeStack is a set for small integers (where small can be relatively big)
 *  the integers are assumed to start small and grow over time
 *  the set is implemented as 
 *  
 *  var bitmap:Pointer<Int64>  an array of 64 bit wide bitmaps 
 *  var stack:Pointer<Int>-    a freelist which itself is implemented using a stack 
 *      each item in the freelist holds the index of a
 *      bitmap segment with at least one bit set
 *  - var idx always indexes the top of the stack
 *  - var committed is the number of objects the FreeStack can currently hold without having to be grown
 *  - var empty indicates whether at least one bit is set
 *  
 *  lookup time for a member of the set is O(1) 
 *  adding a member is O(1)
 *  finding any member and removing it is O(1)
 * 
 *  unlike actual stacks there is no fixed LIFO order
 *  
 * The space requirements are 1.5 bits per member of the set, so for e.g. 
 *     1   GB worth of 32 byte objects,
 *  or 0.5 GB worth of 16 byte objects,
 *  or 4   GB worth of 128 byte objects
 *  which is ca. 33.5 million objects (or "small integers"),
 *  we need exactly 6 MB of memory for the FreeStack
 * 
 *  the stack is growable on demand and doesn't require any relocations, growing it is a O(1) operation.
 *  the granularity of the growth is tunable and required to be a power of 2 >= 64
 */


//@:struct
@:publicFields
class FreeStack {

    var idx:Int;
    var max:Int;
    var committed:Int;
    var stack:Pointer<Int>;
    var bitmap:Pointer<Int64>;
    var empty:Bool;

    /* 
     * 
     * 
     */
    
    inline function init(max_objects:Int64, meta_start:Int64, bitmapsize:Int64){
        bitmap      = (cast meta_start + bitmapsize : Pointer<Int64> );
        stack       = (cast meta_start + (bitmapsize * 2) : Pointer<Int> );
        empty       = true;
        idx         = 0;
        committed   = 0;
        max         = max_objects.int32();
    }

    inline function push(obidx:Int){

        var intidx  = obidx >> 6;
        var bitidx  = obidx &  63;
        var data    = bitmap[intidx];

        if (data == 0){
            stack[idx] = intidx;
            idx       += 1;
            empty      = false;
        }
        bitmap[intidx] = data ^ (( 1 : Int64) << ( bitidx : Int64 ));
    }

    inline function pop():Int64 {
        var pred         = stack[idx-1];
        var data:Int64   = bitmap[pred];
        var bitidx       = Utils.log2_64(data);
        var intidx       = ((pred) << 6);
        var obidx        = intidx | bitidx;

        bitmap[pred] = data ^ (( 1 : Int64) << bitidx);
        if (bitmap[pred] == 0){
            idx -= 1;
            if (idx == 0){
                empty = true;
            }
        }
        return obidx;
    }
    
    inline function grow(n_obs:Int){
        
        var n_obs         = n_obs < 64 ? 64 : n_obs; // n is assumed to be a POW2 >= 64
        
        var bm            = bitmap.int64(); 
        var bm_cur        = committed >> 3; // currently committed bytes (one bit per object, 8 bits per byte, number of committed objects / 8 ) 
        var bm_blocksz    = n_obs     >> 3; // bytes to grow freestack.bitmap 
        var bm_res        = MMan.mcommit( (cast bm+bm_cur : Pointer<Void> ), bm_blocksz, MEM_COMMIT, PAGE_READWRITE);
        
        var st            = stack.int64();
        var st_cur        = committed >> 4;  // same game as above, but we only need 0.5 bits / object for the stack, and hence half the number of bytes
        var st_blocksz    = n_obs >> 4;      // bytes to grow stack   (we need one int per 64 objects)
        var st_res        = MMan.mcommit( (cast st + st_cur : Pointer<Void>), st_blocksz, MEM_COMMIT, PAGE_READWRITE);
        
        committed         = committed + n_obs;
    }
}

@:publicFields
class Pool {


    var size:Int;
    var sz_shift:Int;
    var max:Int64;
    var idx:Int64;
    var committed:Int64;
    var _fstack:Struct<FreeStack>;
    var fstack:FreeStack;
    //var greystack:GreyStack;
    var markbits:Pointer<Int64>;
    var meta:Pointer<Char>;

    var mem:Pointer<Char>;

    static inline function pcast<A,B>(p:Pointer<A>,_:Pointer<B>):Pointer<B> return cast p;

    function init(area:Area,psz_shift:Int){
        fstack = _fstack;
        sz_shift       = psz_shift;

        var obsize     = one << sz_shift;
        var obmax      = one << (area.sz_shift - sz_shift);
        var bitmapsize = one << (area.sz_shift - sz_shift - 3);
        var pool_idx   = sz_shift - 3;

        max           = one << (area.sz_shift - sz_shift);
        idx           = 0;
        committed     = 0;
        var tmp0      = one << area.sz_shift;
        var tmp1      = tmp0 * pool_idx;
        mem           = area.mem + tmp1;
        meta          = area.mem + ((one << 29)*pool_idx);
        markbits      = meta.pcast(markbits);
        size          = obsize.int32();

        fstack.init(obmax,cast meta,bitmapsize);

        printf("%p pool_idx: %d idx:%d obsize: %llu obmax: %llu area:%p mem:%p sz_shift:%d\\n",[this,pool_idx,idx,obsize,obmax,area.mem,mem,sz_shift]);
        //Area.mem + ((one << (area.sz_shift)) * pool_idx);
    }

    function info(){
        printf("%p pool idx: %llu size: %d max: %llu mem:%p sz_shift:%d\\n",[this,idx,size,max,mem,sz_shift]);
    }

    static inline var grow_sz = 0x1000000; // 16 MB steps

    function check_grow(){
        if (idx == committed){
            grow( grow_sz >> sz_shift );
        }
    }

    function grow(n:Int){
        //var so_far         = ( fstack.committed : Int64 ) << sz_shift;
        printf("growing by %d (* %d)\\n",[n,1 << sz_shift]);
        if ( (idx+n) < fstack.committed) {

            // we have enough committed mem in fstack

        } else {

            var fsn = n < 64 ? 64 : n; // n is assumed to be a POT >= 64

            var fsbm           = fstack.bitmap.int64();
            var stack          = fstack.stack.int64();
            var mark           = markbits.int64();


            var bm_cur         = fstack.committed >> 3;
            var stack_cur      = fstack.committed >> 4;

            var stack_blocksz = fsn >> 4;        // bytes to grow freestack.stack (we need one int per 64 objects)
            var bm_blocksz    = fsn >> 3;        // bytes to grow freestack.bitmap and markbits


            var fsbm_res   = MMan.mcommit(cast fsbm+bm_cur, bm_blocksz, MEM_COMMIT, PAGE_READWRITE);
            var mark_res   = MMan.mcommit(cast mark+bm_cur, bm_blocksz, MEM_COMMIT, PAGE_READWRITE);
            var stack_res  = MMan.mcommit(cast stack+stack_cur, stack_blocksz, MEM_COMMIT, PAGE_READWRITE);

            fstack.committed = fstack.committed + fsn;
        }

        var data_cur       = idx << sz_shift;
        var data           = mem.int64();
        var data_blocksz   = n << sz_shift; // bytes to grow data area
        var data_res       = MMan.mcommit(cast data+data_cur, data_blocksz, MEM_COMMIT, PAGE_READWRITE);

        committed += n;

    }

    function alloc():Pointer<Void> {
        var obidx:Int64;
        if (fstack.empty) {
            //printf("idx %llu max: %llu %p \\n",[idx,max,this]);
            if (idx < max){
                obidx = idx;
                check_grow();
                idx += 1;
            } else {
                trace("return null");
                printf("idx %llu max: %llu\\n",[idx,max]);
                return null;
            }
        } else {
            obidx = fstack.pop();
        }
        var r = ( mem + (obidx << sz_shift) );
        return cast r;
    }

    function free(v:Pointer<Void>){
        var cp:Pointer<Char> = cast v;
        var obidx = (cp - mem).int64() >> sz_shift;
        fstack.push(cast obidx);
    }
}

@:publicFields
class Area {

    var idx:Int;
    var start:Int64;
    var sz_shift:Int64;
    var mem:Pointer<Char>;
    var pools:ConstSizeArray<Struct<Pool>,21>;

    function init(idx,start,mem,shift){
        this.idx      = idx;
        this.start    = start;
        this.mem      = mem;
        this.sz_shift = shift;
        printf("area idx: %d start:%llu mem:%p shift:%llu\\n",[idx,start,mem,sz_shift]);
        //untyped printf("area idx: %d start:%llu mem:%p shift:%llu\\n",idx,start,mem,sz_shift);
    }

    function alloc(sz:Int):Pointer<Void>{
        var psz_shift = Utils.next_pow2_log2_32(sz);
        if ( psz_shift > -1 && psz_shift < 5){
            var p:Pool = pools[0];
            var v = p.alloc();
            //p.info();
            //printf("alloc sz:%d -> %d idx: 0 mem:%p\\n",[sz,1 << psz_shift,v]);
            return v;
        } else {
            var p:Pool = pools[psz_shift-4];
            //p.info();
            var v = p.alloc();
            //printf("alloc sz:%d -> %d idx: %d mem:%p\\n",[sz,1 << psz_shift,psz_shift-4,v]);
            return v;
        }
        return null;
    }

    function free(p:Pointer<Void>):Void {
        var v = p.int64();
        var area_mask = (one << 39) - 1;
        var pool_mask = (one << 34) - 1;
        var pool_idx  = ((v & area_mask) >> 34)-1;

        var area_idx =  (v) >> 39;
        //printf("free area_idx %llu pool_idx: %llu mem:%p \\n",[area_idx,pool_idx,v]);

        if (pool_idx > -1 && pool_idx < 21){
            var pool:Pool = pools[pool_idx.int32()];
            //pool.info();
            pool.free(p);
            //printf("free %p ok\\n",[v]);
        } else {
            printf("free %p error %llu\\n",[v,pool_idx]);
        }
        //printf("free area_idx ---\\n",[]);
    }

    static function create(idx:Int):Area{
        var base:Int64    = ((1 : Int64) << 40);
        var size:Int64    = ((1 : Int64) << 39);
        var pool_sz_shift = 34;

        var area_start    = base + size * ( idx : Int64 );
        printf( " area pointer : %p -- ",[area_start.pointer()]);
        #if !winapi
        var mem = MMan.mmap(area_start.pointer(), size, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_FIXED|MAP_NORESERVE|MAP_PRIVATE , -1, 0);
        if (mem == MMan.MAP_FAILED){
            trace("fatal: mmap failed.");
        }
        #else
        var mem = MMan.VirtualAlloc(area_start.pointer(), size, MEM_RESERVE, PAGE_READWRITE);
        if (mem == null){
            trace("fatal: mmap failed.");
        }
        var area_res  = MMan.mcommit(cast mem, (1 << 20), MEM_COMMIT, PAGE_READWRITE);
        if (area_res == null){
            trace("fatal: committing area failed.");
        }
        #end
        var area:Area = cast mem;
        area.init(idx,area_start,cast mem,pool_sz_shift);

        var pool_idx = 4;
        for (i in 0...21){
            var p:Pool = area.pools[i];
            p.init(area,pool_idx + i);
        }
        return area;
    }
    

}

@:include("<time.h>")
extern class Time {
    @:plain
    static var CLOCKS_PER_SEC:Int64;
    @:plain
    static function clock():Int64;
    
    static inline function elapsed(t0:Int64,t1:Int64):Float{
        return t1.float()/CLOCKS_PER_SEC.float() - t0.float()/CLOCKS_PER_SEC.float();
    }
}

@:publicFields
class Bench {
    
    static var N   = 1000;
    static var MAX = 128000;
    static var ps:ConstSizeArray<Pointer<Int>,640000>;
    
    static var mmlc = false;
    
    static var area:Area;
    
    static var OBS  = 32000;
    static var FROM = 0;
    static var TO   = 11;
    
    static function alloc(sz:Int) {
        
        if (mmlc)
            return malloc(1 << (sz+4));
        else {
            var p:Pool = area.pools[sz];
            return p.alloc();
        }
    }
    static function dofree(p:Pointer<Int>,sz:Int) {
        if (mmlc)
            free(cast p);
        else
            area.free(cast p);
    }
    
    
    static function allocs(){
        var sizes = [for (i in FROM...TO) i];
        var idx = 0;
        for (ob in 0...OBS) for (sz in sizes){
            ps[idx] = cast alloc(sz);
            //printf("alloc: %p \\n",[ps[idx]]);
            ps[idx][0] = 123;
            ++idx;
        }
    }
    static function frees(){
        var sizes = [for (i in FROM...TO) i];
        var idx = 0;
        for (ob in 0...OBS) for (sz in sizes){
             //printf("free: %p \\n",[ps[idx]]);
             dofree(ps[idx++],sz);
        }
    }
    
    static function allocs4(st:Int){
        var sizes = [for (i in FROM...TO) i];
        var idx = st;
        for (sz in sizes)for (ob in 0...div(OBS,4)){
            ps[idx] = cast alloc(sz);
            //printf("alloc: %p \\n",[ps[idx]]);
            ps[idx][0] = 123;
            ps[idx][3] = 123;
            idx+=4;
        }
    }
    static function frees4(st:Int){
        var sizes = [for (i in FROM...TO) i];
        var idx = st;
        for (sz in sizes)for (ob in 0...div(OBS,4)){
             //printf("free: %p \\n",[ps[idx]]);
             dofree(ps[idx],sz);
             idx+=4;
        }
    }
    static function _run(){
        var t0 = Time.clock();
        for (i in 0...N){
            allocs();
            frees();
        }
        var t1 = Time.clock();
        return t1-t0;
    }
    static var count = 0;
    static function _run4(){
        var t0 = Time.clock();
        for (st in 0...4){
            printf("%d\\n",[st]);
        for (i in 0...N){
            allocs4(st);
            frees4(st);
            count+=1;
        }}
        var t1 = Time.clock();
        return t1-t0;
    }
    
    static function run(){
        area = Area.create(0);
        N = 250;
        mmlc = true;
        var tsysmalloc = _run4();
        //N = 1000;
        mmlc = false;
        var tcustom = _run4();
        var tsys = tsysmalloc.float()/Time.CLOCKS_PER_SEC.float();
        var tcst = tcustom.float()/Time.CLOCKS_PER_SEC.float();
        
        var pairs = N * (TO-FROM) * OBS;
        
        printf("malloc: %f\\n        %f pairs/s \\ncustom: %f\\n        %f pairs/s\\ncount: %d\\n",[tsys,pairs/tsys,tcst,pairs/tcst,count]);
    }
}


class Alloc {

    //static var ps:ConstSizeArray<Pointer<Int>,128000>;

    static function main(){
        Utils.test_log2_64();
        Bench.run();
    }
    

}


