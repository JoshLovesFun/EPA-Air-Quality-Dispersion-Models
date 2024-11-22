
                          README FILE

                     CTSCREEN (dated 94111)

                         March 21, 1994


     CTSCREEN is the screening mode of the Complex Terrain
Dispersion Model (CTDMPLUS).  This model uses an array of
predetermined meteorological conditions to model user supplied
source-terrain configurations.  CTSCREEN provides estimates of
maximum 1-h, 3-h, 24-h, and annual impacts.  A complete
description of the model can be found in the user's guide.

Notes on Downloading, and Dearchiving: 
=====================================
 
     CTSCRN1.ZIP contains the executable files for CTSCREEN, the
CTDMPLUS menu driver, the terrain preprocessor, and receptor
generator (RECGEN), a test case, and a README file.  

     CTSCRN1E.ZIP contains the Lahey extended memory executable
for CTSCREEN.  This executable has been compiled specifically for
use on '386 and '486 PC systems using MSDOS or PCDOS, and having
at least 4 Megabytes of Random Access Memory (RAM).

     CTSCRN2.ZIP contains the CTSCREEN source code and files for
compiling and linking purposes.  CTSCRN2.ZIP does not have to be
downloaded unless you need to examine the code or recompile the
model.  It is recommende that you download and dearchive
CTSCRN1.ZIP in a separate subdirectory.

        In case you download CTSCRN2.ZIP, make sure the CTSCREEN
source code files stay separate from any CTDMPLUS source code
files that may have been downloaded.  Two of the source code
files for each of the models have the same name but different
codings.

Notes on CTSCREEN Setup:
=======================

        CTSCREEN requires the same input files as CTDMPLUS,
except that the meteorological files are provided with the model. 
The needed terrain and receptor files can be generated using the
preprocessor programs provided with CTDMPLUS.  CTSCREEN can be
run using the CTDMPLUS menu driver if CTSCREEN.EXE is located in
the \CTDM\EXE directory and the input files are located in the
\CTDM\FILES directory.  For convenience, the menu driver and
preprocessor programs, which are located with the CTDMPLUS
archival files, are duplicated in CTSCRN1.ZIP.  Note that
CTSCREEN is a "complex" model - users are advised to read the
manuals (see list below) before attempting to setup and run
CTSCREEN.  Users should also download the file CTSCSUP.ZIP from
the Documentation area of the File Transfer section of SCRAM. 
This document contains important supplemental information to the
CTSCREEN user's guide.

     The following is a list of the documents associated with
CTSCREEN.  These documents can be ordered from NTIS (See NEWS,
Section 3 on SCRAM BBS).


  1. User's Guide to the Complex Terrain Dispersion Model Plus
     Algorithms for Unstable Situations (CTDMPLUS) Volume 1
     (EPA/600/8-89/041), NTIS Accession No.  PB89-181424, paper
     copy $31.00.

  2. The Complex Terrain Dispersion Model Terrain Preprocessor
     System User's Guide and Program Description
     (EPA/600/8-88/003), NTIS Accession No. PB88-162094, paper
     copy $23.00.

  3. User's Guide to the CTDMPLUS:  Volume 2.  The Screening Mode
     (CTSCREEN) (EPA/600/8-90/087), NTIS Accession No.  PB91-
     136564, paper copy $17.00.

     Questions related to the use of CTSCREEN in specific
regulatory applications should be directed to the appropriate
regulatory agency or Regional Modeling Contact (see State/Local
Modeling Contacts in the Agency Communications Section of SCRAM).

     Generic questions related to the use and regulatory
applicability of the model should be directed to Desmond Bailey
via SCRAM EMAIL or phone (919) 541-5248.

     Questions or problems related to program execution should be
reported to Donna Schwede via SCRAM EMAIL or phone (919)
541-3255.

     Users who would like to share comments, suggestions, or
helpful hints are encouraged to use the Model Conferencing area
on the SCRAM Bulletin Board (see Model Conferencing area in the
Public Communications Section of SCRAM).


Description of Files:
====================

.ZIP Archived File:

CTSCRN1.ZIP   Executables:    CTSCREEN.EXE, DRIVEIT.EXE,
                              FITCON.EXE, HCRIT.EXE, PLOTCON.EXE,
                              RECGEN.EXE, and DRIVEIT.OVR,
                              MENU.BAT, test case files, and
                              README.TXT file

CTSCRN1E.ZIP   Lahey extended memory executable and error files: 
               CTSCREEN.EXE, F77L3.EER, and test case output files

CTSCRN2.ZIP    FORTRAN source code for CTSCREEN and response
               files for compiling and linking with Microsoft
               FORTRAN


Executable files:

CTSCREEN  EXE  CTSCREEN model
FITCON    EXE  First terrain preprocessor program
HCRIT     EXE  Second terrain preprocessor program
PLOTCON   EXE  Program used to display the fitted hill
RECGEN    EXE  Program used to create the RECEPTOR file

Batch files:

MENU      BAT  Batch file for executing menu driver from any
               directory

Response files:

LIBCTSC   FIL  Response file for creating a library of CTSCREEN
               subroutines

LINKCTSC  FIL  Response file for linkin CTSCREEN


CTSCREEN Source Code:    Include files (*.INC), Common blocks
                         (*.CMN), and FORTRAN files (*.FOR)

PARAMS   INC 

CONST    CMN    FLVAR  CMN      HEAD     CMN    HILL     CMN
IO       CMN    PARAMS CMN      PASL     CMN    PASVAL   CMN
PASW     CMN    PROFIL CMN      RECEPT   CMN    SCREN    CMN
SFCMET   CMN    STACK  CMN      STACKS   CMN    TIME     CMN
TOP      CMN    TOWER  CMN      VARS     CMN

ANGINT   FOR    BULKFR   FOR    CONCALC  FOR    CTSCREEN FOR   DAYSCR   FOR
DEFLECT  FOR    DELWD    FOR    DTHDZ    FOR    ERF      FOR   FLAT     FOR
FLOW     FOR    FLOWSP   FOR    GETDTH   FOR    GETFR    FOR   GETHILL  FOR
GETSV    FOR    GETSW    FOR    GETTA    FOR    GETUV    FOR   GETWD    FOR
GETWS    FOR    HCRIT    FOR    HILHGT   FOR    HILROT   FOR   INPAR    FOR
INPEMS   FOR    INPREC   FOR    INPSOR   FOR    INPTER   FOR   INPTOW   FOR
KLOSE    FOR    LIFT     FOR    LIFTIN   FOR    LINES    FOR   LSTAB    FOR
LVDF     FOR    MAP      FOR    MIX      FOR    MUNU     FOR   NITCALC  FOR
PAGE     FOR    PATH     FOR    PENFCT   FOR    PICK4    FOR   PLAVG    FOR
PSRCE    FOR    RDSFC    FOR    SEQSCR   FOR    SIGB     FOR   SIGMAY   FOR
SOURCES  FOR    SPEED    FOR    SRISE    FOR    SUN      FOR   TERAX    FOR
TOPN     FOR    TRANPR   FOR    URISE    FOR    UVWD     FOR   WFLAT    FOR
WPDF     FOR    WRAP     FOR    WRAPIN   FOR    WRITIT   FOR   WRITSCR  FOR
XINTRP   FOR    

Test case using 1 source, 2 hills, 40 receptors:

CTSCREEN  PFL  PROFILE file for use with all CTSCREEN runs
CTSCREEN  SFC  SURFACE file for use with all CTSCREEN runs
TEST      CIN  CTDM.IN file
TEST      HCO  TERRAIN file
TEST      RCT  RECEPTOR file
CTDMOUT   BAK  CTDM.OUT file  (MICROSOFT VERSION)
SUMRE     BAK  SUMRE file     (MICROSOFT VERSION)
STCONC    BAK  STCONC file    (MICROSOFT VERSION)
UNCONC    BAK  UNCONC file    (MICROSOFT VERSION)
METDAT    BAK  METDAT file    (MICROSOFT VERSION)
CTDMOUT   LAH  CTDM.OUT file  (LAHEY VERSION)
SUMRE     LAH  SUMRE file     (LAHEY VERSION)
STCONC    LAH  STCONC file    (LAHEY VERSION)
UNCONC    LAH  UNCONC file    (LAHEY VERSION)
METDAT    LAH  METDAT file    (LAHEY VERSION)


Notes on Compiling, and Linking:
================================

     CTSCREEN has been compiled and linked using Microsoft
FORTRAN 5.1.  The command line used to compile was

                          FL /c /Gt *.FOR

The object files were then placed in a library (CTSC.LIB) using
the command

                          LIB @libctsc.fil

Note that libctsc.fil is provided in CTSCRN2.ZIP.  The linking
was done using the command 

                          LINK @linkctsc.fil

The response file linkctsc.fil is also provided in CTSCN2.ZIP.

     The source code for this model has been compiled and tested
on an IBM PC/386 compatible (with math coprocessor) using
Microsoft Fortran 5.1.  The extended memory executable was
compiled using the Lahey F77L-EM/32 FORTRAN compiler (version
5.2);  this executable should be run on '386 and '486 PC systems
using MSDOS or PCDOS, and having at least 4 Megabytes of Random
Access Memory (RAM).  CTSCREEN has also been compiled and tested
on a VAX 8650 running VMS.  Several accessory programs were
written and tested in Borland International's Turbo PASCAL 5.0.


Instructions for running the testcase:
======================================

     CTSCREEN can be run very easily from the CTDMPLUS menu
driver.  Users should refer to Chapter 5 in the CTDMPLUS users
guide for more complete information on using the menu driver. 
Note that the CTDM.IN file used by CTDMPLUS is slightly different
than the CTDM.IN file used by CTSCREEN.  When you are prompted by
the menu driver to select the CTDM.IN file, you will see all of
the *.CIN files found in the \CTDM\FILES directory listed.  The
menu driver can distinguish between those *.CIN files created for
use with CTSCREEN and those created for use with CTDMPLUS.  It
will alert you if the *.CIN file you have selected is not
compatible with CTSCREEN.  Note that the menu driver will not
prompt you to select SURFACE and PROFILE files for use with
CTSCREEN because CTSCREEN.SFC and CTSCREEN.PFL will always be
used with CTSCREEN.

     After running the test case, compare your results with those
in the backup files provided [(*.BAK) in CTSCRN1.ZIP or (.LAH)
in CTSCRN1E.ZIP].  The files created by the menu driver are paired
with the corresponding *.BAK (if you use the MICROSOFT executable)
or *.LAH (if you use the LAHEY executable) files in the table below.
All of the output files produced by the model will be in the
\CTDM\FILES directory.

           Menu driver file                    Backup file
           ================              ==========================
              TEST.OUT                   CTDMOUT.BAK or CTDMOUT.LAH
              TEST.SUM                   SUMRE.BAK   or SUMRE.LAH
              TEST.STC                   STCONC.BAK  or STCONC.LAH
              TEST.UNC                   UNCONC.BAK  or UNCONC.LAH
              TEST.MET                   METDAT.BAK  or METDAT.LAH

     If the menu driver system is not used, the user should be
aware that CTSCREEN uses OPEN statements with fixed filenames. 
The user must rename several of the test case files to these
specific filenames.  The following instructions and table are a
guide for executing CTSCREEN for the test case when the menu
driver is not used.

  1. Copy the input files listed under the column "Input Files"
     to the directory where the CTSCREEN executable resides. 
     Note that this differs from the file organization used by
     the menu driver which looks for these files in the
     CTDM\FILES\ subdirectory.

  2. From the table below find the name required by CTSCREEN for
     each of the input files.  Rename each file listed under the
     column "Input Files" to the name given in the column
     "Required Name".

  3. At the DOS prompt, type CTSCREEN and press ENTER.

  4. After the program finishes, verify that the model ran
     correctly by comparing the files listed under the column
     "Output Files" with those in the column "Backup Files".
 
      Input Files:   Required Name:       Output Files:   Backup files:
      ===========    =============        ============    ============
      TEST.CIN        CTDM.IN              CTDM.OUT        CTDMOUT.*
      TEST.HCO        TERRAIN              SUMRE           SUMRE.*
      TEST.RCT        RECEPTOR             STCONC          STCONC.*
      CTSCREEN.PFL    PROFILE              UNCONC          UNCONC.*
      CTSCREEN.SFC    SURFACE              METDAT          METDAT.*
                                                          (* = BAK or LAH)


     Note:  The runtime for this test case was approximately 6 hours using
a 386, 20 mHz PC.  Users should note that most of the CPU run time (well
over 95 percent) is involved in the computations for unstable hours.  To
avoid tying up a PC for hours, users may want to run the test case in the
"stable hours only" mode.  To do this, the user should edit the input
(.CIN) file; change option 12 in record 2 of the .CIN file from 3 (all
stabilities) to 1 (stable only).  The concentration results should then be
compared with the test case results provided for stable conditions
(STCONC.BAK or STCONC.LAH).
