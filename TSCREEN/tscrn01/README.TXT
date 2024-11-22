                            TSCREEN

                     Version 2.0 (DATED 91303)

                             MCB#4

                          Prepared by:

               Pacific Environmental Services, Inc.
                  3708 Mayfair Street, Suite 202
                   Durham, North Carolina  27707
                          (919) 493-3536

                           INTRODUCTION

     The Techniques Evaluation Section (TES) of the Office of Air
Quality Planning and Standards (OAQPS) provides guidance and
recommendations on the application of air quality models for a variety
of purposes.  The continuing involvement in the Superfund
toxic/hazardous pollutant impact activities has created a need for easy
to use air quality screening modeling techniques. The EPA has developed
the document "A Workbook Of Screening Techniques For Assessing Impacts
of Toxic Air Pollutants", EPA-450/4-88-009, that can serve this purpose
based on a set of logical and mathematical procedures. TSCREEN, A Model
for Screening Toxic Air Pollutant Concentration, is a computer program
developed by Pacific Environmental Services, Inc that implements the
procedures in the workbook.  It is a model that can be easily used by
State and local agencies.

     To correctly analyze toxic emissions and their subsequent
dispersion from one of many different types of possible releases from
Superfund sites, the computer program TSCREEN should be used in
conjunction with the workbook.  With the use of these tools one can
determine the type of release and the steps to follow so that
the release can be simulated via an applicable computer model.  Then,
the dispersion characteristics and pollutant concentrations of the
resulting plume can be calculated.  The air toxics dispersion screening
models imbeded in TSCREEN that are used for the various scenarios are
SCREEN, RVD and PUFF.  Using TSCREEN, a particular release scenario is
selected via input parameters, and TSCREEN automatically selects and
executes the appropriate dispersion model to simulate that scenario.
The model to be used and the worst case meteorological conditions are
automatically selected based on criteria given in the workbook.  TSCREEN
has a front-end control program to the models that also provides, by use
of interactive menus and data entry screens, the same steps as the
workbook. The correct release scenario and associated characteristics of
the toxic emissions release are selected with the help of on-screen text
and graphics.  In addition, data entry is performed in a full-screen edit
mode.  TSCREEN saves the input data for each release scenario so that it
can be retrieved and later edited or executed. TSCREEN also provides a
method of easily viewing and saving the modeling results for each
modeled scenario.  The user's guides is available in Wordperfect 5.0
format from the SCRAM Bulletin Board.

                          GETTING STARTED

Hardware Requirements

     TSCREEN is an IBM PC-based software application written and
compiled in FOXPRO verson 2.0.  Although TSCREEN may operate with
certain memory resident programs installed, no attempt has been made to
test its operation against all memory resident programs available today.
If problems occur while TSCREEN and memory resident programs are running
simultaneously, try removing the memory resident programs from memory
and executing TSCREEN.  As scenario files are created, the hard disk space
needed will increase.  To install TSCREEN on hard disk systems make sure
there is at least 2 MB of free disk space available to load and execute
the program.

     The computer running TSCREEN must have been booted with the CONFIG.SYS
file in the root directory of the boot up disk containing the following
parameters in order to execute properly:

     Files = 30
     Buffers = 30

If the CONFIG.SYS file already has these statements with higher values
assigned, then no modification is necessary, otherwise either add the
needed statements or increase their value to that of the example and
reboot the computer.

     The files required to run TSCREEN are as follows:

        ERHANDLE.EXE
        GRAPH.EXE
        PUFFTS.EXE
        RVDTS.EXE
        SCREENTS.EXE
        CHEMBASE.DBF
        DISPLAY.DBF
        OLDRUNS.DBF
        OUTPUT.DBF
        TSHELP.DBF
        CHEMBASE.IDX
        TSHELP.FPT
        ERMSG.DAT
        VFONT101.FNT
        TSCREEN.EXE
        FOXPRO.ESL
        FOXSWAP.COM
        FOXUSER.DBF
        FOXUSER.FPT
        FOXPRO.ESO

Questions and comments concerning the model can be directed to Joe Touma at
(919) 541-5381.

