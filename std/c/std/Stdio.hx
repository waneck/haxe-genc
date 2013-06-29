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

	@:plain static public function fopen(filename:String, mode:String):File;

	@:plain static public function freopen(filename:String, mode:String, stream:File):File;

	@:plain static public function fclose(stream:File):Int;

	@:plain static public function fflush(stream:File):Int;

	@:plain static public function fwide(stream:File, mode:Int):Int;

	@:plain static public function setbuf(stream:File, buffer:Pointer<Int8>):Void;

	@:plain static public function setvbuf(stream:File, buffer:Pointer<Int8>, mode:Int, size:SizeT):Int;

	@:plain static public function fread(buffer:Pointer<Void>, size:SizeT, count:SizeT, stream:File):SizeT;

	@:plain static public function fwrite(buffer:Pointer<Void>, size:SizeT, count:SizeT, stream:File):SizeT;

	@:plain static public function fgetc(stream:File):Int;

	@:plain static public function getc(stream:File):Int;

	@:plain static public function fgets(str:Pointer<Int8>, count:Int, stream:File):Pointer<Int8>;

	@:plain static public function fputc(ch:Int, stream:File):Int;

	@:plain static public function putc(ch:Int, stream:File):Int;

	@:plain static public function fputs(str:String, stream:File):Int;

	@:plain static public function getchar():Int;

	@:plain static public function gets(str:Pointer<Int8>):Pointer<Int8>;

	@:plain static public function putchar(ch:Int):Int;

	@:plain static public function puts(str:Pointer<Int8>):Int;

	@:plain static public function ungetc(ch:Int, stream:File):Int;

	// TODO: all varargs functions

	@:plain static public function ftell(stream:File):Int32;

	@:plain static public function fgetpos(stream:File, pos:FPosT):Int;

	@:plain static public function fseek(stream:File, offset:Int32, origin:Int):Int;

	@:plain static public function fsetpos(stream:File, pos:FPosT):Int;

	@:plain static public function rewind(stream:File):Void;

	@:plain static public function clearerr(stream:File):Void;

	@:plain static public function feof(stream:File):Int;

	@:plain static public function ferror(stream:File):Int;

	@:plain static public function remove(fname:String):Int;

	@:plain static public function rename(old_filename:String, new_filename:String):Int;

	@:plain static public function tmpfile():File;

	@:plain static public function tmpnam(filename:Pointer<Int8>):Pointer<Int8>;
}