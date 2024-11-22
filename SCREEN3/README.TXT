
                          DRAFT   README.TXT  DRAFT                   2/12/2013

                          SCREEN3 (dated 13043)

This directory contains the SCREEN3 (dated 13043) model.  The SCREEN3 model
is a PC-compatible companion to the revised screening procedures document,
"Screening Procedures for Estimating the Air Quality Impact of Stationary
Sources, Revised," EPA-450/R-92-019.  The document has been incorporated into
the Guideline on Air Quality Models as a replacement for Volume 10R/PTPLU.

The following files are included:

     SCREEN3.EXE          A 32-bit PC-executable file that has been tested
                          and runs on a Windows 7, 32-bit operating system

     SCREEN3A.FOR         The FORTRAN source code, file 1
     SCREEN3B.FOR         The FORTRAN source code, file 2
     SCREEN3C.FOR         The FORTRAN source code, file 3
     MAIN.INC             The "include" statements called by the executable
     DEPVAR.INC           A module not used at this time, but needed for
                          compilation of the source code

     EXAMPLE.DAT          The input data file for the example problem
     EXAMPLE.OUT          The output file for the example problem
     EXAMPNR.DAT          Input data file exercising non-regulatory options
     EXAMPNR.OUT          Output file for EXAMPNR.DAT

     README.TXT           This background information file


Addendum for generating an input file:

As part of the SCREEN3 changes, several non-Regulatory options offered with
SCREEN2 have been incorporated into SCREEN3 along with a non-Regulatory cavity
option.  These options are selected by entering the following flags or value on
the same line as the source type input:

      N    - TO USE THE NON-REGULATORY BUT CONSERVATIVE BRODE 2
                  MIXING HEIGHT OPTION,
      nn.n - TO USE AN ANEMOMETER HEIGHT OTHER THAN THE REGULATORY
                  (DEFAULT) 10 METER HEIGHT, (n represents a number)
      SS   - TO USE THE NON-REGULATORY SCHULMAN-SCIRE CAVITY CALCULATION.
             
      Example:  P N 7.5 SS  

                where:
                   P   is for Point source input
                   N   is for using the Brode 2 mixing height option
                   7.5 is for assuming a 7.5 meter anemometer height for
                         wind speed and wind direction values
                   SS  is for using the Schulman-Scire cavity option.

The SCREEN3 Model User's Guide, EPA-454/B-95-004, is available from the
SCREEN3 area on the SCRAM web site, as SCREEN3D.ZIP.

Installation and Operating Instructions:

Unzip each file and store the files in one subdirectory (e.g.  C:\SCREEN3test)

  ***********************************************************************
  * It is best to run these executable from a Windows Command Prompt.   *
  * (e.g. Start [button] | All Programs | Accessories | Command Prompt) * 
  ***********************************************************************

If SCREEN3 is run from Windows Explorer and if there is an error, the 
execution window may open and close in a flash and any error messages will 
disappear just as fast.  **Please use the Command Prompt!!!**

Change directory to where you stored the above files (e.g. cd C:\SCREEN3test)

SCREEN3.exe is executed from a Command Prompt by typing:

    SCREEN3 < example.dat

(just as you did when testing the original SCREEN3)

In all cases, the output file is SCREEN.OUT.  SCREEN.DAT is a copy of the 
input file. Both files should be renamed before the next run or they will 
be overwritten.

DOS has a File Compare command, FC.  It can be used to compare two files to 
find differences between them.  

To use the FC command, at the Command Prompt, type:

    FC EXAMPLE.DAT Youroutput.DAT                 
        - output scrolls on the screen
    FC EXAMPLE.DAT Youroutput.DAT | more          
        - output is printed one page at a time.  
          Press the Space Bar to see the next page
    FC EXAMPLE.DAT Youroutput.DAT > savediffs.txt 
        - output is saved in the file savediffs.txt

The differences should be minor.  For instance,  date and time will different.  
Sometimes a lead "0" will not be printed such as .00 vs 0.00.  

Sometimes there may be a slight difference in output values to the right 
of the decimal point such as 12.3456789 vs 12.3456788.  Note the last digits 
are different.  This type of difference is inconsequential and is attributed 
to the greater precision offered by a 64-bit Operating System and compiler.

If you have any questions, please contact us through the "Contact Us" link on
our SCRAM Web Site at http://www.epa.gov/ttn/scram
