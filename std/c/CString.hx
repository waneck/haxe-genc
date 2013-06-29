package c;

import c.Types;
import c.Pointer;

@:include("<string.h>")
extern class CString {
	@:plain static public function memcpy(destination:Pointer<Void>, source:Pointer<Void>, num:SizeT):Pointer<Void>;
	@:plain static public function memmove(destination:Pointer<Void>, source:Pointer<Void>, num:SizeT):Pointer<Void>;
	@:plain static public function strcpy(destination:Pointer<Int8>, source:ConstPointer<Int8>):Pointer<Int8>;
	@:plain static public function strncpy(destination:Pointer<Int8>, source:ConstPointer<Int8>, num:SizeT):Pointer<Int8>;

	@:plain static public function strcat(destination:Pointer<Int8>, source:ConstPointer<Int8>):Pointer<Int8>;
	@:plain static public function strncat(destination:Pointer<Int8>, source:ConstPointer<Int8>, num:SizeT):Pointer<Int8>;

	@:plain static public function memcmp(ptr1:Pointer<Void>, ptr2:Pointer<Void>, num:SizeT):Int;
	@:plain static public function strcmp(str1:ConstPointer<Int8>, str2:ConstPointer<Int8>):Int;
	@:plain static public function strcoll(str1:ConstPointer<Int8>, str2:ConstPointer<Int8>):Int;
	@:plain static public function strncmp(str1:ConstPointer<Int8>, str2:ConstPointer<Int8>, num:SizeT):Int;
	@:plain static public function strxfrm(destination:Pointer<Int8>, source:ConstPointer<Int8>, num:SizeT):SizeT;

	@:plain static public function memchr(ptr:Pointer<Void>, value:Int, num:SizeT):Pointer<Void>;
	@:plain static public function strchr(str:ConstPointer<Int8>, character:Int):ConstPointer<Int8>;
	@:plain static public function strcspn(str1:ConstPointer<Int8>, str2:ConstPointer<Int8>):SizeT;
	@:plain static public function strpbrk(str1:ConstPointer<Int8>, str2:ConstPointer<Int8>):ConstPointer<Int8>;
	@:plain static public function strrchr(str:ConstPointer<Int8>, character:Int):ConstPointer<Int8>;
	@:plain static public function strspn(str1:ConstPointer<Int8>, str2:ConstPointer<Int8>):SizeT;
	@:plain static public function strstr(str1:ConstPointer<Int8>, str2:ConstPointer<Int8>):ConstPointer<Int8>;
	@:plain static public function strtok(str:Pointer<Int8>, delimiters:ConstPointer<Int8>):Pointer<Int8>;

	@:plain static public function memset(ptr:Pointer<Void>, value:Int, num:SizeT):Pointer<Void>;
	@:plain static public function strerror(errnum:Int):Pointer<Int8>;
	@:plain static public function strlen(str:ConstPointer<Int8>):SizeT;
}