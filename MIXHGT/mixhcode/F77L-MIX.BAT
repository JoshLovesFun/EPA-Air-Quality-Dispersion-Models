@ECHO OFF
REM   F77L-BAT.BAT  Compiles the mixing height source code
REM                 using the Lahey F77L-EM/32 compiler
REM   To suppress displaying compiler warning messages, add the /nw option to
REm     each of the four lines beginning with f77l3
REM   A math coprocessor is required to run PCRAMMET compiled with Lahey

f77l3 mixhts.for   /b /i /l /no > complist.
f77l3 mixlib.for   /b /i /l /no >> complist.

REM   Link the object files to create the executable
386link mixhts,mixlib, -stub runb

REM   Suppress the DOS Extender banner when PCRAMMET is run
cfig386 mixhts   -nosignon
