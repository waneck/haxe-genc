/*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

//@:coreApi
class Sys {

	public static function print( v : Dynamic ) : Void {

	}

	public static function println( v : Dynamic ) : Void {

	}

	//public static function stdin() : haxe.io.Input {
		//return null;
	//}
//
	//public static function stdout() : haxe.io.Output {
		//return null;
	//}
//
	//public static function stderr() : haxe.io.Output {
		//return null;
	//}

	public static function getChar( echo : Bool ) : Int {
		return 0;
	}

	@:access(String.ofPointerCopyNT)
	public static function args() : Array<String> {
		return [for (i in 0...c.Boot.argc) String.ofPointerCopyNT(c.Boot.argv[i])];
	}

	public static function getEnv( s : String ):String {
		return cast c.CStdlib.getenv(s);
	}

	public static function putEnv( s : String, v : String ) : Void {
		
	}

	public static function sleep( seconds : Float ) : Void {
		
	}

	public static function setTimeLocale( loc : String ) : Bool {
		return false;
	}

	public static function getCwd() : String {
		return null;
	}

	public static function setCwd( s : String ) : Void {

	}

	public static function systemName() : String {
		return null;
	}

	public static function command( cmd : String, ?args : Array<String> ) : Int {
		return c.CStdlib.system(cmd);
	}

	public static function exit( code : Int ) : Void {
		c.CStdlib.exit(code);
	}

	public static function time() : Float {
		return 0;
	}

	public static function cpuTime() : Float {
		return 0;
	}

	public static function executablePath() : String {
		return null;
	}

	//public static function environment() : haxe.ds.StringMap<String> {
		//return null;
	//}
}