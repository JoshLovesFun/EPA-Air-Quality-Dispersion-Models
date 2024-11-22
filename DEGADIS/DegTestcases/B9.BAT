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
copy %DEGADIS%\example.er1 b9.er1
copy %DEGADIS%\example.er2 b9.er2
ECHO.
ECHO If you see error messages above, press (CTRL-C to stop)
ECHO then add or retype the set command 
ECHO or move the *.ER* files to the directory in your SET command else
Pause
%DEGADIS%\DEG1  b9
%DEGADIS%\DEG2S  b9
ECHO.
ECHO If you see error messages above, press (CTRL-C to stop)
ECHO then add or retype the set command 
ECHO or move the executable files to the directory in your SET command else
Pause
copy b9.scl+b9.sr3  b9.lis
del b9.er1
del b9.er2
del b9.scl
del b9.sr3
FC b9old.lis b9.lis 
FC b9old.lis b9.lis > b9lis.dif
ECHO.
ECHO Above is the file compare between the old test case files and
ECHO your output.  The only major differences should be those of 
ECHO date and time.  There maybe some "0.0" vs " .0" and some
ECHO "1.234567890" vs "1.234567973".  These are considered okay
ECHO and perfectly normal.
