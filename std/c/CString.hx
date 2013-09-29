package c;

import c.Types;
import c.Pointer;

@:include("<string.h>")
extern class CString {
	@:plain static public function memcpy<A,B>(destination:Pointer<A>, source:ConstPointer<B>, num:SizeT):Pointer<A>;
	@:plain static public function memmove<A,B>(destination:Pointer<A>, source:ConstPointer<B>, num:SizeT):Pointer<A>;
	@:plain static public function strcpy(destination:Pointer<Char>, source:ConstPointer<Char>):Pointer<Char>;
	@:plain static public function strncpy(destination:Pointer<Char>, source:ConstPointer<Char>, num:SizeT):Pointer<Char>;

	@:plain static public function strcat(destination:Pointer<Char>, source:ConstPointer<Char>):Pointer<Char>;
	@:plain static public function strncat(destination:Pointer<Char>, source:ConstPointer<Char>, num:SizeT):Pointer<Char>;

	@:plain static public function memcmp<A>(ptr1:Pointer<A>, ptr2:Pointer<A>, num:SizeT):Int;
	@:plain static public function strcmp(str1:ConstPointer<Char>, str2:ConstPointer<Char>):Int;
	@:plain static public function strcoll(str1:ConstPointer<Char>, str2:ConstPointer<Char>):Int;
	@:plain static public function strncmp(str1:ConstPointer<Char>, str2:ConstPointer<Char>, num:SizeT):Int;
	@:plain static public function strxfrm(destination:Pointer<Char>, source:ConstPointer<Char>, num:SizeT):SizeT;

	@:plain static public function memchr<A>(ptr:Pointer<A>, value:Int, num:SizeT):Pointer<A>;
	@:plain static public function strchr(str:ConstPointer<Char>, character:Int):ConstPointer<Char>;
	@:plain static public function strcspn(str1:ConstPointer<Char>, str2:ConstPointer<Char>):SizeT;
	@:plain static public function strpbrk(str1:ConstPointer<Char>, str2:ConstPointer<Char>):ConstPointer<Char>;
	@:plain static public function strrchr(str:ConstPointer<Char>, character:Int):ConstPointer<Char>;
	@:plain static public function strspn(str1:ConstPointer<Char>, str2:ConstPointer<Char>):SizeT;
	@:plain static public function strstr(str1:ConstPointer<Char>, str2:ConstPointer<Char>):ConstPointer<Char>;
	@:plain static public function strtok(str:Pointer<Char>, delimiters:ConstPointer<Char>):Pointer<Char>;

	@:plain static public function memset<A>(ptr:Pointer<A>, value:Int, num:SizeT):Pointer<A>;
	@:plain static public function strerror(errnum:Int):Pointer<Char>;
	@:plain static public function strlen(str:ConstPointer<Char>):SizeT;
}
