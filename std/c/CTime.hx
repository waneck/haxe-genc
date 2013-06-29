package c;

import c.Pointer;
import c.Types;

@:native("clock_t")
@:struct
extern class ClockT { }

@:native("time_t")
@:struct
extern class TimeT { }

@:native("tm")
typedef Tm = {
	tm_sec:Int,
	tm_min:Int,
	tm_hour:Int,
	tm_mday:Int,
	tm_mon:Int,
	tm_year:Int,
	tm_wday:Int,
	tm_yday:Int,
	tm_isdst:Int
}

@:include("<time.h>")
extern class CTime {
	@:plain static public var CLOCKS_PER_SEC:Int;
	@:plain static public var NULL:Int;

	@:plain static public function clock():ClockT;
	@:plain static public function difftime(end:TimeT, beginning:TimeT):Float;
	@:plain static public function mktime(timeptr:Pointer<Tm>):TimeT;
	@:plain static public function time(timer:Pointer<TimeT>):TimeT;

	@:plain static public function asctime(timeptr:ConstPointer<Tm>):Pointer<Int8>;
	@:plain static public function ctime(timer:ConstPointer<TimeT>):Pointer<Int8>;
	@:plain static public function gmtime(timer:ConstPointer<TimeT>):Pointer<Tm>;
	@:plain static public function localtime(timer:ConstPointer<TimeT>):Pointer<Tm>;
	@:plain static public function strftime(ptr:Pointer<Int8>, maxsize:SizeT, format:ConstPointer<Int8>, timeptr:ConstPointer<Tm>):SizeT;
}