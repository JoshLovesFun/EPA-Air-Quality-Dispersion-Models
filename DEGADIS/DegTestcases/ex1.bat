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
%degadis%\jetplu    ex1
%degadis%\degbridg  ex1
ECHO.
ECHO  DEGBRIDG deletes any old INP file such as EX1.INP above for cases that do
ECHO  not return to ground level such as this one.  This EX1 example does not  
ECHO  stop without a clunky file-not-found message.  Therefore, in the EX1 batch 
ECHO  file, a "IF file-not-found then EXIT line" was added as a work around and  
ECHO  to provide a feel for how the model operates.
ECHO.
Pause
if not exist ex1.inp exit /b 0
fc ex1.l %DEGADIS%\ex1.lis > ex1lis.dif
ECHO.
ECHO If you see error messages above, press (CTRL-C to stop)
ECHO then add or retype the set command 
ECHO or move the executable files to the directory in your SET command else
Pause
copy  %DEGADIS%\example.er1  ex1.er1
copy  %DEGADIS%\example.er2  ex1.er2
ECHO.
ECHO If you see error messages above, press (CTRL-C to stop)
ECHO then add or retype the set command 
ECHO or move the *.ER* files to the directory in your SET command else
Pause
%DEGADIS%\DEG1  ex1
%DEGADIS%\DEG2S ex1
ECHO.
ECHO If you see error messages above, press (CTRL-C to stop)
ECHO then add or retype the set command 
ECHO or move the executable files to the directory in your SET command else
Pause
copy ex1.out+ex1.scl+ex1.sr3   ex1.lis
del ex1.er1
del ex1.er2
del ex1.sr3
del ex1.scl
FC ex1old.Lis ex1.Lis 
FC ex1old.Lis ex1.Lis > ex1Lis.dif
ECHO.
ECHO Above is the file compare between the old test case files and
ECHO your output.  The only major differences should be those of 
ECHO date and time.  There maybe some "0.0" vs " .0" and some
ECHO "1.234567890" vs "1.234567973".  These are considered okay
ECHO and perfectly normal.
