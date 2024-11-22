                            README.TXT

                     ISCLT3 Model (dated 96113)                      4/22/96



        There are seven compressed (zipped) files for the ISCLT3 modeling
system:

 1) ISCLT3.ZIP    - contains the Lahey-compiled (Version 5.2) executable file
                    of the model;

 2) TEST-LT.ZIP   - contains the original ISCLT3 test case (using the Lahey
                    compiler);

 3) AREALT.ZIP    - contains the area source test case (using the Lahey
                    compiler);

 4) DEPTLT.ZIP    - contains the deposition test case (using the Lahey
                    compiler);

 5) ISCLTSRC.ZIP  - contains the source code for the model and batch files to
                    recompile and link the model for both Lahey and Microsoft
                    versions of the model;

 6) ISC3VOL1.ZIP  - contains the ISC3 User's Guide, Volume 1 - User
                    Instructions (in WP5.1)

 7) ISC3VOL2.ZIP  - contains the ISC3 User's Guide, Volume 2 - Description of
                    Model Algorithms (in WP5.1)



The following describes the content of each file:

 1. ISCLT3.ZIP         - Lahey-compiled executable file of the model
       ISCLT3.EXE      - model executable file
       README.TXT      - this file

 2. TEST-LT.ZIP        - original ISCLT2 test case (using the Lahey compiler)
       TEST-LT.INP     - ISCLT3 model input control file
       RDU87.STA       - STAR-generated meteorological data
       TEST-LT.OUT     - model output

 3. AREALT.ZIP         - area source test case (using the Lahey compiler)
       AREATEST.INP    - ISCLT3 model input control file
       AREATEST.MET    - STAR-generated meteorological data
       AREATEST.OUT    - model output

 4. DEPLT.ZIP          - deposition test case (using the Lahey compiler)
       BOSTON.JFD      - output from STAR/input to ISCLT3
       DEPTEST.INP     - ISCLT3 model input control file
       DEPTEST.OUT     - ISCLT3 model output

 5. ISCLTSRC.ZIP - model source code, batch files and link response files to
                   recompile and relink the model.  Note that the array limits
                   in the INCLUDE files assume the use of a Lahey Fortran
                   compiler.  The Lahey version of ISCLT3 assumes a 386
                   processor or higher with a math coprocessor and at least
                   4 MB of RAM.  The array limits are set for up to 300
                   sources, 1200 receptors and 5 source groups in MAIN1LT.INC,
                   and 601 by 601 terrain points in the x and y directions,
                   respectively, in DEPVAR.INC.

                   TO RECOMPILE ISCLT3 WITH MICROSOFT FORTRAN, LOWER ARRAY
                   LIMITS ARE REQUIRED; OTHERWISE THE MODEL WILL NOT RUN.
                   Suggested limits are:  up to 50 sources, 500 receptors, 5
                   source groups (in MAIN1LT.INC), and 201 by 201 terrain
                   points (in DEPVAR.INC).

                   For those who have a need to recompile ISCLT3.EXE, batch
                   files (.BAT) and link response files (.LRF) have been
                   included that can be used to compile and link the model with
                   the Lahey EM/32 Fortran Compiler, Version 5.0 or higher and
                   the Microsoft Fortran Compiler.  The batch and link response
                   files are:

                   F77LISCL.BAT  - compile and link ISCLT with Lahey
                   F77LISCL.LRF  - link response file used in F77LISCS.BAT
                   FLMSISCL.BAT  - compile and link ISCLT with Microsoft
                   FLMSISCL.LRF  - link response file used in FLMSISCS.BAT

 6. ISC3VOL1.ZIP - ISC3 User's Guide, Volume 1 - User Instructions (in WP5.1)

 7. ISC3VOL2.ZIP - ISC3 User's Guide, Volume 2 - Description of Model
                   Algorithms (in WP5.1)

______________________________________________________________________________


The following are general instructions on running the components of ISCLT3.
Complete instructions can be found in ISC3VOL1.ZIP.


                           ----------------
                        --- RUNNING ISCLT3 ---
                           ----------------

     To run ISCLT3, type the following command at the DOS prompt:

               ISCLT3  (input file)  (output file)

______________________________________________________________________________


        Instructions for Running the ISCLT3 Deposition Test Case


The following instructions will cause the original output files to be
overwritten.  The original output (DEPTEST.OUT) should be copied to another
directory to avoid this problem.  After running the above steps, the hands-on
test outputs can be compared with the original outputs.


1.  Download files 1-4 above into a unique subdirectory, i.e. such as
    ISCLT3.  The source code file (file 5) is not needed unless
    recompiling.

2.  Proceed to unzip the four files using PKUNZIP.EXE version 2.04G.

3.  When step 2 is complete, type the following at the DOS prompt:

    ISCLT3 DEPTEST.INP DEPTEST.OUT

    DEPTEST.INP - the input control file
    DEPTEST.OUT - the output file from ISCLT3

4.  When step 3 is complete, the ISCLT3 run is complete.  The results
    (DEPTEST.OUT) may be viewed with an editor.

___________________________________________________________________________

                            *** NOTE ***

Beginning with this update, the ISC3 code will be compiled with the Lahey
    compiler ONLY.  However, a batch file is available with the source code
    in ISCLTSRC.ZIP to recompile the model using the Microsoft compiler.
    NOTE:  Before recompiling the model with the Microsoft compiler you
    must edit the MAIN1LT.INC and DEPVAR.INC files and modify the array
    storage limits in the PARAMETER statements.  Suggested limits for the
    Microsoft compiler are provided in those two files in lines commented
    out with the field 'CMICRO'.

___________________________________________________________________________



Questions concerning the ISC3 model should be directed to Dennis Atkinson at
fax (919) 541-0044, SCRAM email, or INTERNET atkinson.dennis@epamail.epa.gov  .
