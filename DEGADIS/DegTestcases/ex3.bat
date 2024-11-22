ECHO OFF
CLS
ECHO.
ECHO Remenber to add: 
ECHO.
ECHO   SET DEGADIS="drive:\current path to your executable and ER* files"
ECHO. 
ECHO ************************
ECHO List:
ECHO.
SET | Find /I "DEGadis"
ECHO.
ECHO ************************
ECHO.
ECHO In any list above, did you see:
ECHO   Degadis="the drive plus path to your Executables and .ER* files"?
ECHO   (e.g. DEGADIS="C:\Degadis\exes_erx")
ECHO.
ECHO If not, Press (CTRL-C to stop) and add the set command 
ECHO per the DEGADIS Readme file or
Pause
ECHO.
%degadis%\jetplu    ex3
%degadis%\degbridg  ex3
copy  %DEGADIS%\example.er1  ex3.er1
copy  %DEGADIS%\example.er2  ex3.er2
ECHO.
ECHO If you see error messages above, press (CTRL-C to stop)
ECHO then add or retype the set command 
ECHO or move the *.ER* files to the directory in your SET command else
Pause
%DEGADIS%\DEG1  ex3
%DEGADIS%\DEG2S ex3
ECHO.
ECHO If you see error messages above, press (CTRL-C to stop)
ECHO then add or retype the set command 
ECHO or move the executable files to the directory in your SET command else
Pause
copy ex3.out+ex3.scl+ex3.sr3   ex3.lis
del ex3.er1
del ex3.er2
del ex3.sr3
del ex3.scl
FC ex3old.lis ex3.lis 
FC ex3old.lis ex3.lis > ex3lis.dif
ECHO.
ECHO Above is the file compare between the old test case files and
ECHO your output.  The only major differences should be those of 
ECHO date and time.  There maybe some "0.0" vs " .0" and some
ECHO "1.234567890" vs "1.234567973".  These are considered okay
ECHO and perfectly normal.
