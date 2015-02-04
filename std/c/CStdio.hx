package c;

import c.Types;
import c.Pointer;

@:native("FILE")
extern class File { }

@:native("fpos_t")
@:struct
@:coreType
abstract FPosT from Int to Int { }

@:include("<stdio.h>")
@:analyzer(no_simplification)
extern class CStdio {

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

	@:plain static public function fopen(filename:ConstPointer<Char>, mode:ConstPointer<Char>):File;

	@:plain static public function freopen(filename:ConstPointer<Char>, mode:ConstPointer<Char>, stream:File):File;

	@:plain static public function fclose(stream:File):Int;

	@:plain static public function fflush(stream:File):Int;

	@:plain static public function fwide(stream:File, mode:Int):Int;

	@:plain static public function setbuf(stream:File, buffer:Pointer<Char>):Void;

	@:plain static public function setvbuf(stream:File, buffer:Pointer<Char>, mode:Int, size:SizeT):Int;

	@:plain static public function fread(buffer:Pointer<Void>, size:SizeT, count:SizeT, stream:File):SizeT;

	@:plain static public function fwrite(buffer:ConstPointer<Void>, size:SizeT, count:SizeT, stream:File):SizeT;

	@:plain static public function fgetc(stream:File):Int;

	@:plain static public function getc(stream:File):Int;

	@:plain static public function fgets(str:Pointer<Char>, count:Int, stream:File):Pointer<Char>;

	@:plain static public function fputc(ch:Int, stream:File):Int;

	@:plain static public function putc(ch:Int, stream:File):Int;

	@:plain static public function fputs(str:ConstPointer<Char>, stream:File):Int;

	@:plain static public function getchar():Int;

	@:plain static public function gets(str:Pointer<Char>):Pointer<Char>;

	@:plain static public function putchar(ch:Int):Int;

	@:plain static public function puts(str:ConstPointer<Char>):Int;

	@:plain static public function ungetc(ch:Int, stream:File):Int;

	@:plain static public function fscanf(stream:File, format:ConstPointer<Char>, rest:Array<VarArg>):Void;

	@:plain static public function fprintf(stream:File, format:ConstPointer<Char>, rest:Array<VarArg>):Void;

	@:plain static public function scanf(format:ConstPointer<Char>, rest:Array<VarArg>):Void;

	@:plain static public function printf(format:ConstPointer<Char>, rest:Array<VarArg>):Int;

	@:plain static public function sprintf(str:Pointer<Char>, format:ConstPointer<Char>, rest:Array<VarArg>):Void;

	@:plain static public function sscanf(str:ConstPointer<Char>, format:ConstPointer<Char>, rest:Array<VarArg>):Void;

	// TODO: vprintf, vfprintf, vsprintf

	@:plain static public function ftell(stream:File):Int32;

	@:plain static public function fgetpos(stream:File, pos:FPosT):Int;

	@:plain static public function fseek(stream:File, offset:Int32, origin:Int):Int;

	@:plain static public function fsetpos(stream:File, pos:ConstPointer<FPosT>):Int;

	@:plain static public function rewind(stream:File):Void;

	@:plain static public function clearerr(stream:File):Void;

	@:plain static public function feof(stream:File):Int;

	@:plain static public function ferror(stream:File):Int;

	@:plain static public function perror(s:ConstPointer<Char>):Int;

	@:plain static public function remove(fname:ConstPointer<Char>):Int;

	@:plain static public function rename(old_filename:ConstPointer<Char>, new_filename:ConstPointer<Char>):Int;

	@:plain static public function tmpfile():File;

	@:plain static public function tmpnam(filename:Pointer<Char>):Pointer<Char>;
}