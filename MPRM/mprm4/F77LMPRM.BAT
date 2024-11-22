echo off
rem   Compile and link the MPRM programs STAGE1N2 and STAGE3 using the
rem     Lahey F77L-em/32 compiler, version 5.2

rem   USAGE:  f77lmprm
rem              or
rem           f77lmprm nw

rem   The parameter nw supresses the compiler warning messages; any one
rem   of four possible switches can be used as shown below:

if "%1" == "nw"  goto nowarn
if "%1" == "NW"  goto nowarn
if "%1" == "/nw"  goto nowarn
if "%1" == "/NW"  goto nowarn

rem  Compile the source files

f77l3 COMPLETE.FOR /b /i /l /no /ns
f77l3 HEADER.FOR   /b /i /l /no /ns
f77l3 LIBFILE.FOR  /b /i /l /no /ns
f77l3 LIBPC.FOR    /b /i /l /no /ns  /d1lahey
f77l3 MERGE.FOR    /b /i /l /no /ns
f77l3 MP2XFOR.FOR  /b /i /l /no /ns
f77l3 MP3XFOR.FOR  /b /i /l /no /ns
f77l3 MP4XFOR.FOR  /b /i /l /no /ns
f77l3 DEPMET1.FOR  /b /i /l /no /ns
f77l3 DEPMET2.FOR  /b /i /l /no /ns
f77l3 OSFILE.FOR   /b /i /l /no /ns
f77l3 OSSETUP.FOR  /b /i /l /no /ns
f77l3 OSSRPG.FOR   /b /i /l /no /ns
f77l3 SETUP1.FOR   /b /i /l /no /ns
f77l3 SETUP2.FOR   /b /i /l /no /ns
f77l3 SETUPPC.FOR  /b /i /l /no /ns
f77l3 SFFILE.FOR   /b /i /l /no /ns
f77l3 STAGE1N2.FOR /b /i /l /no /ns
f77l3 STAGE3.FOR   /b /i /l /no /ns
f77l3 UAFILE.FOR   /b /i /l /no /ns
goto end

:nowarn
f77l3 COMPLETE.FOR /b /i /l /no /ns /nw
f77l3 HEADER.FOR   /b /i /l /no /ns /nw
f77l3 LIBFILE.FOR  /b /i /l /no /ns /nw
f77l3 LIBPC.FOR    /b /i /l /no /ns /nw /d1lahey
f77l3 MERGE.FOR    /b /i /l /no /ns /nw
f77l3 MP2XFOR.FOR  /b /i /l /no /ns /nw
f77l3 MP3XFOR.FOR  /b /i /l /no /ns /nw
f77l3 MP4XFOR.FOR  /b /i /l /no /ns /nw
f77l3 DEPMET1.FOR  /b /i /l /no /ns /nw
f77l3 DEPMET2.FOR  /b /i /l /no /ns /nw
f77l3 OSFILE.FOR   /b /i /l /no /ns /nw
f77l3 OSSETUP.FOR  /b /i /l /no /ns /nw
f77l3 OSSRPG.FOR   /b /i /l /no /ns /nw
f77l3 SETUP1.FOR   /b /i /l /no /ns /nw
f77l3 SETUP2.FOR   /b /i /l /no /ns /nw
f77l3 SETUPPC.FOR  /b /i /l /no /ns /nw
f77l3 SFFILE.FOR   /b /i /l /no /ns /nw
f77l3 STAGE1N2.FOR /b /i /l /no /ns /nw
f77l3 STAGE3.FOR   /b /i /l /no /ns /nw
f77l3 UAFILE.FOR   /b /i /l /no /ns /nw

:end

rem   Create the two executables

386LINK @mprm177.lrf
cfig386 STAGE1N2 -nosignon
386LINK @mprm377.lrf
cfig386 STAGE3   -nosignon
