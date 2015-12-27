@echo off
cd bin
call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat"
make MSVC=1
main
pause