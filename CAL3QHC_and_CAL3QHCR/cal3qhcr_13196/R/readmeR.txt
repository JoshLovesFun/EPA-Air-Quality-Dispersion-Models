******************************************************************************
*                                                                            *
*                 C A L 3 Q H C R  ( D A T E D  1 3 1 9 6 )                  *
*                                                                            *
*                        R E A D M E   P A C K A G E                         *
*                                                                            *
******************************************************************************

FILE CONTENTS:
**************

The CAL3QHCR (Dated 13196) files are:

CAL3QHCR.FOR   The CAL3QHCR source code
CAL3QHCR.EXE   The CAL3QHCR executable code **

             ** - compiled using Intel ifort compiler
                  "Intel(R) Visual Fortran Compiler XE
                   for applications running on IA-32,
                   Version 12.1.2.278 Build 2011112"

*.BAT        for executing a DOS file batch command for running or building
             CAL3QHCR or comparing user results with test case results.
*.TXT        A text file such as a README file.
*.CTL        A control file with a list of input and output filenames.  These
               names are read in by the program
*.MSG        File containing error and other messages
*.INP        Input files for the test cases
*.MET        Input meteorological data files
*.ET1        Copy of vehicular emissions, traffic volume, and signalization     
               (ETS) data as read from the input control file 
*.ET2        Preprocessed *.ET1 data
*.ILK        Link variable data output file.  The previously used *.LNK 
               extension causes the file to show up as a "Shortcut" in current
               versions of the Windows XP Operating System.
*.OUT        Main output file
*.DIF        File containing the differences between the SCRAM test case and
             user generated test case results.  Differences between results
             should be date and time only.
*.PLT        AERMOD-format Plot file.


CAL3QHCR COMPILATION INFORMATION:
*******************************

CAL3QHCR was written to Fortran-77 and Fortran-90 standards and should
be recompilable on other computer systems capable of compiling a
FORTRAN program.  This is subject to the following qualification:

   A non-F77 method of obtaining the system date and time is used,
   because there was no standard for that in F77.  The method used is
   the DATE_AND_TIME subroutine, which became standard later. If you
   need a F77 method, some of the other methods (named 'GETDAT', and
   'DATE' and 'TIME') are still in the code but commented out, or you
   can use a method that works with your compiler.

   For Lahey Fortran users, the Lahey Date and Time statements from
   CAL3QHC were added to this source code but commented out with a
   "CL" in the first two columns.  The Lahey statements have not been
   compiled and checked.

If using the Intel ifort compiler, use the command:

        BUILD.BAT

A file of Windows commands is provided, BUILD.BAT, with the command to
build the program using Intel's ifort compiler:

   	ifort /check:all /Qtrapuv /RTCu /traceback cal3qhcr.for

If one uses another compiler, bear in mind the goals of checking for
errors.

The program has also been passed through the compiler's "/stand:f90"
check for compliance with the Fortran 90 standard. Many warnings about
"obsolete" features are given, but they can be ignored as Fortran 90
mandates that they are to be supported.


TEST CASE RUNNING INSTRUCTIONS:
*******************************

General Instructions

To run a test case: 

  Copy (do not rename) one of the test case control file names to CAL3R.CTL.
  These names will either be C1C, R1C, R2C, R1P, R2P, or MED followed by a 
  ".CTL".  The CAL3R.CTL format structure is as follows:
      [full pathname] Message filename (eg MainMarket.MSG)
      [full pathname] input filename  (eg C:\CAL3QHCR\Proj\MainMarket.INP)
      [full pathname] meteorological filename (eg E:\Met\Medford\S4222590.asc)
      [full pathname] ET1 filename  (eg fname.et1)
      [full pathname] ET2 filename
      [full pathname] Output filename
      [full pathname] Link data filename
      [full pathname] Plot file name
    If all the files and executable are in the same subdirectory, the 
      full pathname is not needed.
  Copy CAL3QHCR.EXE to the subdirectory where CAL3R.CTL is located
  Double click on CAL3QHCR.EXE.  A DOS or Command Prompt window should appear
  and the status of the program should scroll on by.
  CAL3QHCR uses an additional file, CAL3R.MSG to store error and other
    types of messages.  Be sure to rename CAL3R.MSG if you want to save the 
    file.  It will be overwritten the next time CAL3QHCR is executed.


The following are approximate execution times:

                 Pentium4
     Test Case   @ 2.8 Ghz      Description
      C1C        20 sec.        Emulation of CAL3QHC test case 1  
      R1C        48 min.        Tier I example for CO       
      R2C        50 min.        Tier II example for CO       
      R1P        22 min         Tier I example for PM-10       
      R2P        23 min         Tier II example for PM-10
      MED        3 min 18 sec.  Produces WARNING messages, please ignore.

     The output file names will have the three-letter control file prefix name
and a "T", just to the left of the period in the filename (ex.: C1CT.OUT), and
an extention identifying the file contents type (eg *.OUT - which indicates 
the main output file).  A list identifying each possible extension is shown in
the File Contents section above.

     Once a test case has been run, the user's output files can be compared
with test case output files by using the DOS FC command as shown below. 
Batch files with a format of FC???.BAT have been provided.  The ??? stands for
the test case name such as C1C or MED.  The output from the FC command will be
directed to another file with a DIF extension (eg C1COUT.DIF - which indicates
the user and test case main output files have been compared).  A text editor or
word processing program can be used to FILE|OPEN the DIF file for examination.
The only differences should be any date, time, and creation date values written
to file.  The above general instructions are incorporated into the command 
instructions below.

Checkout Instructions

After running each test case above, check the results by running each set of 
test case results through File Compare (FC).  File Compare batch files have 
been created for your convenience.  To run each set of File Compares:

     Open up a DOS or Command Prompt Window  
       (eg Start | Programs | Accessories | DOS or Command Prompt)
     Change drive, if necessary (eg c:> E:)
     Change directory (cd \CAL3QHC) to where the output files are stored.
     At the prompt, type the following commands:
 
      C1C
      R1C
      R2C
      R1P
      R2P

     The following test warns that, out of the 8700 meteorological records,
     there are a couple dozen faulty ones. These messages can be ignored.
      MED

      Use a text editor, word processor, or type the following to review the   
DIF files:

           C:\> TYPE C1Cxxx.DIF | MORE  where xxx is ET1, ET2, LNK, or OUT.

      Once the comparison and review are completed and there is no need to keep 
the output files, remove the output files by typing the following:      
(Remember to include the "T" just before the period.)

           C:\> DEL C1CT.*     

Problems and questions should be addressed to:

R. Chris Owen
owen.chris@epa.gov
