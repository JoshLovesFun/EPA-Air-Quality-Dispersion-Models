******************************************************************************
*                                                                            *
*                 C A L 3 Q H C    ( D A T E D  0 4 2 4 4 )                  *
*                                                                            *
*                           R E A D M E   F I L E                            *
*                                                                            *
******************************************************************************

FILE CONTENTS:
**************

The CAL3QHC (Dated 04244) files are:

CAL3QHC.FOR   The CAL3QHC source code **
CAL3QHC.EXE   The CAL3QHC executable code **

             ** - compiled using Compaq Visual Fortran Version 6.6c

*.FOR        Source code files
*.BAT        1) for executing the test cases, 
             2) for executing a DOS file comparison command for comparing user  
               results with test case results.
*.TXT        A text file such as a README file.
*.DAT        Input files for the test cases
*.OUT        Output files
*.DIF        File containing the differences between the SCRAM test case and
             user generated test case results.  Differences between the results
             should be date and time only.


CAL3QHC COMPILATION INFORMATION:
*******************************

CAL3QHC was rewritten to Fortran-90 standards and should be recompilable on other
computer systems capable of compiling a FORTRAN program.  No OPEN statements
were used in the source code.

INPUT FORMAT
************

     The following generic input format table uses variable names referenced to
definitions in the CAL3QHC user's guide:

LINE
NUMBER    VARIABLE NAME AND INPUT SEQUENCE
  1       'JOB'  ATIM  ZO  VS  VD  NR  SCAL  IOPT  IDEBUG
  2       'RCP'  XR  YR  ZR
  3       'RUN'  NL  NM  PRINT2  'MODE'
           (Note: 'MODE' has been added to a type 3 line.
            Enter 'C' for CO or 'P' for PM calculations.)
  4       IQ  (When IQ = 2, Line Number types 5a and 5b follow)
  5a       'LNK'  'TYPE'  XL1  YL1  XL2  YL2  HL  WL  NLANES
  5b       CAVG  RAVG  YFAC  IV  IDLFAC  SFR  ST  AT
  4       IQ  (When IQ = 1, Line Number type 5c follows)
  5c       'LNK'  'TYPE'  XL1  YL1  XL2  YL2  VPHL  EFL  HL  WL
  6       U  BRG  CLAS  MIXH  AMB  'VAR'  DEGR  VAI(1)  VAI(2)
               (Note: BRG refers to the wind direction, the direction the wind
               is blowing FROM.  Lines 2, and 4 through 6 are repeated as
               necessary.)

     Example input files have been included with the CAL3QHC model package.


TEST CASE RUNNING INSTRUCTIONS:
*******************************

General Instructions

To run a test case, enter one of the test case batch file names after the DOS
or command prompt.  These names will either be EX1, EX2, EX3, or EXP.  
The P in EXP refers to a PM-10 test case based on EX-1 input data.

     The output file names will have the three-letter batch fill prefix name
and a "T", just to the left of the period in the filename (ex.: EX-1T.OUT), and
an extention identifying the file contents type (eg *.OUT - which indicates an 
output file).  A list identifying each possible extension is shown in the File
Content section above.

     Once a test case has been run, the user's output files can be compared
with test case output files by using the DOS FC command as shown below. 
FC?.BAT files have been provided.  The output from the FC command will be
directed to another file with a DIF extension (eg EX1OUT.DIF - which indicates
the user and test case main output files have been compared).  A text editor or
word processing program that can import a text (ASCII) file can then be used to
view the DIF file contents.  The only differences will be the date and time of
the respective OUT files.  After each comparison is done, the user should
delete these output files.  The above general instructions are incorporated
into the command instructions below.


Checkout Instructions

To check out the test cases, type:

     C:\> EXn     : where n = 1, 2, 3, or P  for Test Cases 1, 2, 3, or P
     C:\> FCn     

      Use a text editor, word processor, or type the following to review the   
resulting DIF file:

           C:\> TYPE EXnOUT.DIF | MORE 

    Once the comparison and review are completed and there is no need to keep  
    the output files, remove the output files by typing the following:         
      (Remember to include the "T" just before the period.)

           C:\> DEL EX-nT.OUT   


Problems and questions should be addressed to: 
Peter Eckhoff.
eckhoff.peter@epa.gov  