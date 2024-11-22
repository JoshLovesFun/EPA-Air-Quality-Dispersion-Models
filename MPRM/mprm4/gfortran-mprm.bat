@rem
@echo off

echo.
echo NOTE THAT THE RESULTING .EXE FILES MIGHT NEED A CYGWIN SYSTEM'S DLL.

echo + + + > compile_errors.txt
echo + + + >    link_errors.txt

set FLAGS=-w -ggdb -O0
set CFLAGS=%FLAGS% 2>> compile_errors.txt
set LFLAGS=%FLAGS% 2>> link_errors.txt

set FOBJ=o

set FC=gfortran %CFLAGS%
set FLINK=gfortran %LFLAGS%

@echo on
%FC% -c COMPLETE.FOR
%FC% -c HEADER.FOR
%FC% -c LIBFILE.FOR
%FC% -c LIBPC.FOR
%FC% -c MERGE.FOR
%FC% -c MP2XFOR.FOR
%FC% -c MP3XFOR.FOR
%FC% -c MP4XFOR.FOR
%FC% -c DEPMET1.FOR
%FC% -c DEPMET2.FOR
%FC% -c OSFILE.FOR
%FC% -c OSSETUP.FOR
%FC% -c OSSRPG.FOR
%FC% -c SETUP1.FOR
%FC% -c SETUP2.FOR
%FC% -c SETUPPC.FOR
%FC% -c SFFILE.FOR
%FC% -c STAGE1N2.FOR
%FC% -c STAGE3.FOR
%FC% -c UAFILE.FOR

%FLINK% -o stage1n2.exe STAGE1N2.%FOBJ% COMPLETE.%FOBJ% SETUP1.%FOBJ% SETUP2.%FOBJ% SETUPPC.%FOBJ% OSSETUP.%FOBJ% HEADER.%FOBJ% LIBFILE.%FOBJ% LIBPC.%FOBJ%  SFFILE.%FOBJ% UAFILE.%FOBJ% OSFILE.%FOBJ% MERGE.%FOBJ%

%FLINK% -o stage3.exe STAGE3.%FOBJ%   COMPLETE.%FOBJ% SETUP1.%FOBJ% SETUP2.%FOBJ% SETUPPC.%FOBJ% OSSETUP.%FOBJ% HEADER.%FOBJ% LIBFILE.%FOBJ% LIBPC.%FOBJ%  OSSRPG.%FOBJ% MP2XFOR.%FOBJ% MP3XFOR.%FOBJ% MP4XFOR.%FOBJ% DEPMET1.%FOBJ% DEPMET2.%FOBJ%

echo.
echo NOTE THAT THE RESULTING .EXE FILES MIGHT NEED A CYGWIN SYSTEM'S DLL.