****************************************************************************
*                                                                          *
*                  B P I P P R M ( D A T E D  0 4 2 7 4 )                  *
*                                                                          * 
*                       R E A D M E   P A C K A G E                        *
*                                                                          *
****************************************************************************


INSTALLATION:
*************

The BPIPPRM program, source code, test cases, and a BPIPPRM user's guide, 
which is titled: Addendum to the ISC3 User's Guide (Useguide.pdf), have been
enclosed as one ZIP file, BPIPPRM.ZIP.  The addendum to the user's guide is 
also posted with a copy of the original BPIP user's guide on SCRAM as 
BPRMDOC.ZIP.

To install the BPIPPRM software packages, simply copy everything into a 
directory of your choice on a hard disk drive.  One megabyte of hard disk 
space should be more than enough space for the two ZIP files.


DOCUMENTATION:
**************

The addendum mentioned above contains six page revisions to the BPIP user's
guide.  They reflect current and previous modifications that were made to 
the BPIP program and incorporated into the BPIPPRM program.  These six pages 
are structured to replace the original user's guide pages that have the same 
page numbers.

For those who do not have a user's guide, a second ZIP file, BPIPD.ZIP,
contains the complete user's guide without the revised pages.  The user's 
guide and revised pages were written using WordPerfect and saved in a PDF 
format.  BPIPD.ZIP and BPIPREV.PDF are available from the Documentation 
section of SCRAM BBS.

 
DISK CONTENTS:
**************

The BPIPPRM (Dated 04274) files are:

BPIPPRM.FOR     The BPIPPRM source code
BPIPPRM.EXE     The BPIPPRM executable code of compiled using Compaq Visual 
                Fortran compiler version 6.6
*.BAT           BAT files for executing the test cases
*.INP           Input files for the test cases
*.OUT           ASCII test case output files for BPIPPRM
*.SUM           ASCII test case summary files for BPIPPRM
*.PDF           BPIPPRM Documentation in the form of 3 User's Guides
README.TXT      This file


BPIPPRM INFORMATION:
****************

The BPIPPRM was written to Fortran-90 standards and should be recompilable on
other computer systems capable of compiling a FORTRAN program.  No OPEN 
statements were used in the source code.  However three OPEN statements were
comented out and are available if you want to recompile the program.

BPIPPRM should be executed from a DOS or Coomand Prompt.  The execution line 
is:

    BPIPPRM *.inp *.out *.sum  where * represents a filename

The building height and base elevation in test case 1 was changed from 
that shown in the user's guide.  The building height was changed from 20 
to 26 meters and the base elevation was changed from 10 to 13 meters to 
better show the program's behavior for resultant GEP stack height values 
around 65 meters.

In order to run the test cases, go to the subdirectory where you unzipped 
BPIP.  If in Windows Explorer, double click on each of the files that have 
a ".BAT" extention.  If at a DOS or Command Prompt, type:

     A1LT - Test of [ISC]LT switch 
     A1ST - Test of [ISC]ST switch 
     A1NP - Test of no PRIME switch
     A1P  - Test of PRIME switch   
     A5LT - Test of [ISC]LT switch 
     A5ST - Test of [ISC]ST switch 
     A5NP - Test of no PRIME switch
     A5P  - Test of PRIME switch   

The output files from the test case runs above will have the following
formats: AnxT.OUT and AnxT.SUM, where n stands for either 1 or 5 and x 
stands for LT, ST, NP, or P.  These files should be compared to the respective 
output files, AnxS.OUT and AnxS.SUM, that came with the ZIP files.  
The DOS FC command should be used.  The only differences should be the 
date and time of execution.  Some program represent zero as ".00" while 
others present zero as "0.00".  In such cases, you may want to do a global 
replace where "<blank>.00" is replaced with "0.00" before doing a compare 
between the test case results and the results you obtain in running BPIP's 
test cases.

Also, because Allocatable Arrays are being used, the absolute tier number
will change with respect to older runs. In the old BPIP A5STS.SUM file, the 
highest absolute tier number was 11.  In the new BPIPPRM A5STS.SUM file, the 
highest absolute tier number is 9.  This is because the old absolute tier  
number was calculated using a constant maximum number of tiers per 
building.  That constant was set to 4.  In this version of BPIPPRM, that 
value is a variable based upon finding the building with the maximum number 
of tiers on it.  This number was found by searching through the input file 
for the building with the most tiers on it. Please read the BPIP User's
Guide and Addendum and the source code comments for further details.

The slowest test case runs in about 1 second on a Pentium 4 computer.
