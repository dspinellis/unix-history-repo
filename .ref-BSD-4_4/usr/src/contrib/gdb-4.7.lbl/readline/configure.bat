@echo off
if "%1" == "go32" goto h8300
if "%1" == "h8/300" goto h8300
echo Specify one of [ go32 h8/300 ] on command line
goto exit

:go32
echo Configuring readline for go32
copy Makefile.dos Makefile
copy sysdep-norm.h sysdep.h
goto exit

:h8300
echo Configuring readline for H8/300
copy Makefile.dos Makefile
copy sysdep-norm.h sysdep.h

:exit

cd examples
call configure %1
cd ..
