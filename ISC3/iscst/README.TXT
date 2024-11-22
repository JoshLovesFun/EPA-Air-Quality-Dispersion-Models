                             README.TXT

                     ISCST3 Model (dated 02035)                   02/04/2002



        The ISCST3 modeling system consists of the following files:

1) ISCST.ZIP, which contains the following:

   README.TXT    - this file

   ISCST3.ZIP    - contains the Lahey-compiled (LF90 Version 4.5i)
                   executable file of the model;

   TEST-ST.ZIP   - contains the original ISCST3 test case;

   AREATEST.ZIP  - contains the area source test case;

   DEPTEST.ZIP   - contains the deposition test case;

   TEST-PM.ZIP   - contains the post-1997 PM10 test case;

   GASDTEST.ZIP  - contains the gas dry deposition test case;

   ISCSTSRC.ZIP  - contains the source code for the model and batch files
                   to recompile and link the model using the Lahey LF90
                   and LF95 compilers;

   ISC3MET.ZIP   - contains meteorological processing utilities.

2) ISC3V1.PDF - contains the ISC3 User's Guide, Volume I - User
                Instructions, with the Addendum (in PDF format); and

3) ISC3V2.PDF - contains the ISC3 User's Guide, Volume II - Description of
                Model Algorithms, with the Addendum (in PDF format).



The following describes the contents of each file contained in ISCST.ZIP:

 1. ISCST3.ZIP         - Lahey LF90-compiled executable file of the model
       ISCST3.EXE      - model executable file
       LF90.EER        - Lahey LF90 error message file
       README.TXT      - this file

 2. TEST-ST.ZIP        - original ISCST2 test case
       TEST-ST.INP     - ISCST3 model input control file
       PREPIT.ASC      - PCRAMMET-generated meteorological data
       TEST-ST.OUT     - ISCST3 model output

 3. AREATEST.ZIP       - area source test case
       AREATEST.INP    - ISCST3 model input control file
       AREATEST.MET    - meteorological input data
       AREATEST.OUT    - ISCST3 model output

 4. DEPTEST.ZIP        - deposition test case
       DEPTEST.MET     - PCRAMMET-generated meteorological data
       DEPTEST.INP     - ISCST3 model input control file
       TEST100.TER     - ISCST3 model input gridded terrain file
       DEPTEST.OUT     - ISCST3 model output

 5. TEST-PM.ZIP        - post-1997 PM10 test case
       TEST-PM.INP     - ISCST3 model input control file
       SALEM.MET       - PCRAMMET-generated meteorological data
                         for five years from Salem, OR
       TEST-PM.OUT     - ISCST3 model ouput

 6. GASDTEST.ZIP       - gas dry deposition test case
       GASDTEST.INP    - ISCST3 model input control file
       GASDTEST.MET    - meteorological input data
       GASDTEST.OUT    - ISCST3 model ouput

 7. ISCSTSRC.ZIP - model source code, batch file and link response file to
                   recompile and relink the model.
                   LF90ISCS.BAT  - compile and link ISCST3 with Lahey LF90
                   LF90ISCS.LRF  - link response file used in LF90ISCS.BAT
                   LF95ISCS.BAT  - compile and link ISCST3 with Lahey LF95
                   LF95ISCS.LRF  - link response file used in LF95ISCS.BAT

 8. ISC3MET.ZIP        - meteorological processing utilities
       BINTOASC.EXE    - executable of program to convert binary (unformatted)
                         meteorological data files to ASCII
       BINTOASC.FOR    - source code for the BINTOASC program
       METLIST.EXE     - executable of program to generate a listing of the
                         meteorological data from a preprocessed file
       METLIST.FOR     - source code for the METLIST program
       README2.TXT     - README file describing BINTOASC and METLIST

______________________________________________________________________________


The following are general instructions on running the components of ISCST3.
Complete instructions can be found in Volume I of the ISC3 User's Guide.


                           ----------------
                        --- RUNNING ISCST3 ---
                           ----------------

     To run ISCST3, type the following command at the DOS prompt:

               ISCST3  (input file)  (output file)

The deposition test case, which is for a 16 day period in March 1987, requires
less than 20 seconds to run on a 90 MHz Pentium.


______________________________________________________________________________


        Instructions for Running the ISCST3 Deposition Test Case


The following instructions will cause the original output files to be
overwritten.  The original output (DEPTEST.OUT) should be copied to another
directory to avoid this problem.  After running the following steps, the
hands-on test outputs can be compared with the original outputs.


1.  Download the ISCST.ZIP file into a unique subdirectory, i.e. such as
    ISCST3.

2.  Proceed to unzip the ISCST.ZIP file using PKUNZIP.EXE version 2.04G,
    as well as the other compressed files contained within ISCST.ZIP.

3.  When step 2 is complete, type the following at the DOS prompt:

    ISCST3 DEPTEST.INP DEPTEST.OUT

    DEPTEST.INP - the input control file
    DEPTEST.OUT - the output file from ISCST3

4.  When step 3 is complete, the ISCST3 run is complete.  The results
    (DEPTEST.OUT) may be viewed with an editor.


The other test cases may be run in a similar manner.

___________________________________________________________________________


                          *** RECOMPILING NOTE ***

    For this update, the ISCST3 code has been compiled with the Lahey LF90
    compiler (version 4.5i).  However, a batch file is also provided with
    the source code in ISCSTSRC.ZIP to recompile the model using the
    Lahey LF95 compiler (version 5.0).  No changes to the source code are
    needed to compile the model with the LF95 compiler.  The ISCST3 code
    has also been successfully compiled and executed using the DEC Visual
    Fortran compiler (version 5.0).  In order to compile the model with
    the DEC Visual Fortran compiler, SUBROUTINE GETCOM in the ISCST3.FOR
    source file must be modified.  Instructions on making the necessary
    modifications to GETCOM are provided in the ISCST3.FOR file.  Also,
    when compiling the model with DEC Visual Fortran, the MODULES.FOR
    file should be compiled first, before attempting to build the
    ISCST3 executable.


___________________________________________________________________________



Questions concerning the ISC3 model should be directed to Dennis Atkinson at
fax (919) 541-0044, or via EMAIL atkinson.dennis@epa.gov .
