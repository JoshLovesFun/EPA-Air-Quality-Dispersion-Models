                            README FILE

                       CONCOR (Dated 90239)            Aug 27, 1990


CONCOR is a stand alone  Fortran program that converts  Latitude/Longitude
coordinates to UTM or UTM coordinates to Latitude/Longitude.

CONCOR can also be adapted to a Fortran program as a subroutine.  CONCOR
was complied using Microsoft Fortran 5.0 and will execute on a PC with or
without a math coprocessor chip.

CONCOR accepts input from Fortran unit 5 and sends output to Fortran unit 6.
On a PC, Fortran unit 5 defaults to the keyboard and unit 6 to the screen.
Coordinates can be converted one at a time from keyboard input or any number
of coordinates can be converted from a file. Converting coordinates stored
in a file can be accomplished by using the redirection features "<" and ">"
of the DOS (Disk Operating System) of your PC.


CONCOR provides 2 options for coordinate conversions:

    OPT 1  -  Latitude/Longitude coordinates to UTM

    OPT 2  -  UTM to Latitude/Longitude.


Contents of CONCOR.ARC archived file:

    CONCOR.FOR  -  CONCOR source code (Fortran)
    CONCOR.EXE  -  CONCOR executable
    LLTOUTM.EXP -  Example input data file of LL coordinates
    UTMTOLL.EXP -  Example input data file of UTM coordinates
    README.TXT  -  Readme file including operating instructions


OPERATING INSTRUCTIONS 


CONCOR can be operated in two modes:

 1) Screen Input Mode

       Coordinates can be converted one at a time from screen input.

 2) File Input Mode

       Any number of coordinates can be converted from a input data file   
       created with a text editor.


The following are examples of the two modes of operation:

 1)  Screen input mode:

      CONCOR                <== Executes CONCOR Program
      1                     <== Conversion Option 1 (LL to UTM)
      38 20 15 121 06 20    <== LL coordinates
      ^Z                    <== CTL Z to terminate the program normally


 2)  File input mode:

      CONCOR < lltoutm.exp      <== Coordinates stored in lltoutm.exp are
                                    converted and printed on the screen.


      CONCOR < utmtoll.exp > exp.out   <== Coordinates stored in utmtoll.exp
                                           are converted and output is stored
                                           in exp.out

Questions or comments concerning the operation of the program can be directed
to Hersch Rorex at (919) 541-5637.

