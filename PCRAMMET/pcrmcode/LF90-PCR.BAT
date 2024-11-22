@ECHO OFF
REM   F90L-PCR.BAT  Compiles the PCRAMMET source code
REM                 using the Lahey Fortran 90 compiler
REM   To suppress displaying compiler warning messages, add the /nw option to
REm     each of the four lines beginning with lf90
REM   A math coprocessor is required to run PCRAMMET compiled with Lahey
REM     -c     = suppress linking
REM     -nchk  = do not check subscript and substring bounds
REM     -tp    = generate Pentium code
REM     -trace = enable runtime error traceback
REM     -nco   = do not display compiler options
REM     -nvm   = do not bind the virtual memory manager
REM     -nwin  = do not create a windows application

lf90 pcrammet.for  -c -nchk -tp -trace -nco -nvm -nwin
lf90 pcrdata.for   -c -nchk -tp -trace -nco -nvm -nwin
lf90 pcrparam.for  -c -nchk -tp -trace -nco -nvm -nwin
lf90 pcrlib.for    -c -nchk -tp -trace -nco -nvm -nwin


REM   Link the object files to create the executable
lf90  pcrammet.obj pcrlib.obj pcrdata.obj pcrparam.obj  -bind -pack

REM   Suppress the DOS Extender banner when PCRAMMET is run
cfig386 pcrammet.exe -nosignon
