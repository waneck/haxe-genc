package c.std;

import c.Types;
import c.Pointer;

@:native("FILE")
extern class File { }

@:native("fpos_t")
@:struct
@:coreType
abstract FPosT from Int to Int { }

@:include("<stdio.h>")
extern class Stdio {
	
	@:plain static var stdout:File;
	@:plain static var stdin:File;
	@:plain static var stderr:File;

	@:plain static public var EOF:Int;
	@:plain static public var BUFSIZ:Int;
	@:plain static public var FILENAME_MAX:Int;
	@:plain static public var FOPEN_MAX:Int;
	@:plain static public var _IOFBF:Int;
	@:plain static public var _IOLBF:Int;
	@:plain static public var _IONBF:Int;
	@:plain static public var L_tmpnam:Int;
	@:plain static public var NULL:Int;
	@:plain static public var SEEK_CUR:Int;
	@:plain static public var SEEK_END:Int;
	@:plain static public var SEEK_SET:Int;
	@:plain static public var TMP_MAX:Int;
	
	static inline public function fopen(filename:String, mode:String):File {
		return untyped __call("fopen", filename, mode);
	}
	
	static public inline function freopen(filename:String, mode:String, stream:File):File {
		return untyped __call("freopen", filename, mode, stream);
	}
	
	static public inline function fclose(stream:File):Int {
		return untyped __call("fclose", stream);
	}
	
	static public inline function fflush(stream:File):Int {
		return untyped __call("fflush", stream);
	}
	
	static public inline function fwide(stream:File, mode:Int):Int {
		return untyped __call("fflush", stream, mode);
	}
	
	static public inline function setbuf(stream:File, buffer:Pointer<Int8>):Void {
		untyped __call("setbuf", stream, buffer);
	}
	
	static public inline function setvbuf(stream:File, buffer:Pointer<Int8>, mode:Int, size:SizeT):Int {
		return untyped __call("setvbuf", stream, buffer, mode, size);
	}
	
	static public inline function fread(buffer:Pointer<Void>, size:SizeT, count:SizeT, stream:File):SizeT {
		return untyped __call("fread", buffer, size, count, stream);
	}
	
	static public inline function fwrite(buffer:Pointer<Void>, size:SizeT, count:SizeT, stream:File):SizeT {
		return untyped __call("fwrite", buffer, size, count, stream);
	}
	
	static public inline function fgetc(stream:File):Int {
		return untyped __call("fgetc", stream);
	}
	
	static public inline function getc(stream:File):Int {
		return untyped __call("getc", stream);
	}
	
	static public inline function fgets(str:Pointer<Int8>, count:Int, stream:File):Pointer<Int8> {
		return untyped __call("fgets", str, count, stream);
	}
	
	static public inline function fputc(ch:Int, stream:File):Int {
		return untyped __call("fputc", ch, stream);
	}
	
	static public inline function putc(ch:Int, stream:File):Int {
		return untyped __call("putc", ch, stream);
	}
	
	static public inline function fputs(str:String, stream:File):Int {
		return untyped __call("fputs", str, stream);
	}
	
	static public inline function getchar():Int {
		return untyped __call("getchar");
	}
	
	static public inline function gets(str:Pointer<Int8>):Pointer<Int8> {
		return untyped __call("gets", str);
	}
	
	static public inline function putchar(ch:Int):Int {
		return untyped __call("putchar", ch);
	}
	
	static public inline function puts(str:Pointer<Int8>):Int {
		return untyped __call("puts", str);
	}
	
	static public inline function ungetc(ch:Int, stream:File):Int {
		return untyped __call("ungetc", ch, stream);
	}
	
	// TODO: all varargs functions
	
	static public inline function ftell(stream:File):Int32 {
		return untyped __call("ftell", stream);
	}
	
	static public inline function fgetpos(stream:File, pos:FPosT):Int {
		return untyped __call("fgetpos", stream, pos);
	}
	
	static public inline function fseek(stream:File, offset:Int32, origin:Int):Int {
		return untyped __call("fseek", stream, offset, origin);
	}
	
	static public inline function fsetpos(stream:File, pos:FPosT):Int {
		return untyped __call("fsetpos", stream, pos);
	}
	
	static public inline function rewind(stream:File):Void {
		untyped __call("frewind", stream);
	}
	
	static public inline function clearerr(stream:File):Void {
		untyped __call("clearerr", stream);
	}
	
	static public inline function feof(stream:File):Int {
		return untyped __call("feof", stream);
	}
	
	static public inline function ferror(stream:File):Int {
		return untyped __call("ferror", stream);
	}
	
	static public inline function remove(fname:String):Int {
		return untyped __call("remove", fname);
	}
	
	static public inline function rename(old_filename:String, new_filename:String):Int {
		return untyped __call("rename", old_filename, new_filename);
	}
	
	static public inline function tmpfile():File {
		return untyped __call("tmpfile");
	}
	
	static public inline function tmpnam(filename:Pointer<Int8>):Pointer<Int8> {
		return untyped __call("tmpnam", filename);
	}
}