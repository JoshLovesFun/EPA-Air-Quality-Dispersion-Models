****************************************************************************
*                                                                          *
*                    B P I P  ( D A T E D  0 4 1 1 2 )                     *
*                                                                          * 
*                       R E A D M E   P A C K A G E                        *
*                                                                          *
****************************************************************************


INSTALLATION:
*************

The BPIP program, source code, test cases, and an addendum to the BPIP 
user's guide have been enclosed as one ZIP file, BPIP.ZIP.  The addendum
to the user's guide is also posted with a copy of the original user's 
guide on SCRAM.

To install the BPIP software packages, simply copy everything into a 
directory of your choice on the hard disk.  One megabyte of hard disk 
space should be more than enough space for the two ZIP files.


DOCUMENTATION:
**************

The addendum mentioned above contains six page revisions to the BPIP user's
guide.  They reflect current and previous modifications that were made to 
the BPIP program.  These six pages are structured to replace the original 
user's guide pages that have the same page numbers.

For those who do not have a user's guide, a second ZIP file, BPIPD.ZIP,
contains the complete user's guide without the revised pages.  The user's 
guide and revised pages were written using WordPerfect and saved in a PDF 
format.  BPIPD.ZIP and BPIPREV.PDF are available from the Documentation 
section of SCRAM BBS.

 
DISK CONTENTS:
**************

The BPIP (Dated 04112) files are:

BPIP.FOR     The BPIP source code
BPIP.EXE     The BPIP executable code of compiled using Compaq Visual 
             Fortran compiler version 6.6
*.BAT        BAT files for executing the test cases
*.INP        Input files for the test cases
*.OUT        ASCII test case output files for BPIP
*.SUM        ASCII test case summary files for BPIP
*.PDF        Revised pages to the BPIP User's Guide
README.TXT   This file


BPIP INFORMATION:
****************

The BPIP was written to Fortran-90 standards and should be recompilable on
other computer systems capable of compiling a FORTRAN program.  No OPEN 
statements were used in the source code.

BPIP should be executed from a DOS or Coomand Prompt.  The execution line 
is:

    BPIP *.inp *.out *.sum  where * represents a filename

The building height and base elevation in test case 1 was changed from 
that shown in the user's guide.  The building height was changed from 20 
to 26 meters and the base elevation was changed from 10 to 13 meters to 
better show the program's behavior for resultant GEP stack height values 
around 65 meters.

In order to run the test cases, go to the subdirectory where you unzipped 
BPIP.  If in Windows Explorer, double click on each of the files that have 
a ".BAT" extention.  If at a DOS or Command Prompt, type:

     A1LT
     A1ST
     A5LT
     A5ST


The output files from the test case runs above will have the following
formats: AnxT.OUT and AnxT.SUM, where n stands for either 1 or 5 and x 
stands for L or S.  These files should be compared to the respective 
output files, AnxTST.OUT and AnxTST.SUM, that came with the ZIP files.  
The DOS FC command should be used.  The only differences should be the 
date and time of execution.  Some program represent zero as ".00" while 
others present zero as "0.00".  In such cases, you may want to do a global 
replace where "<blank>.00" is replaced with "0.00" before doing a compare 
between the test case results and the results you obtain in running BPIP's 
test cases.

Also, because allocatable arrays are being used, the absolute tier number
will change with respect to older runs. In the old A5STST.SUM file, the 
highest absolute tier number was 11.  In the new A5STST.SUM file, the  
highest absolute tier number is 9.  This is because the old absolute tier  
number was calculated using a constant maximum number of tiers per 
building.  That constant was set to 4.  In this version of BPIP, that 
value is a variable based upon finding the building with the maximum number 
of tiers on it.  This number was found by searching through the input file 
for the building with the most tiers on it. Please read the BPIP User's
Guide and Addendum and the source code comments for further details.

The slowest test case runs in about 2 seconds on a Pentium 4 computer.
