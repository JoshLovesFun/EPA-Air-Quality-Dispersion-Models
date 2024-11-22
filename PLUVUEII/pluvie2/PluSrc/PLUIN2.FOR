       program PLUIN2
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+                                                                     +
C+    FUNCTIONAL DESCRIPTION:                       PLUVUE II VERSION  +
C+                                                                     +
C+        The output from this program is a data                       +
C+        file which can be used as the input data for the execution   +
C+        of the PLUVUE II plume visibility model.  The input to this  +
C+        program is either a data file which is nearly correct and    +
C+        is to be modified, or a file of standard default values to   +
C+        be modified as desired.  This program is written so it is    +
C+        possible to rapidly review all input parameters, to easily   +
C+        accept correct values, and to enter data only when it is     +
C+        desired to change the value of a parameter.                  +
C+                                                                     +
C+        This version of the program listed here is a modification of +
C+        the program used on digital equipment corporation computers, +
C+        adapted to run on the IBM PC and compatibles.                +
C+                                                                     +
C+    SUBPROGRAMS CALLED : GETANS,OPNFIL,CLSFIL,UNCODE                 +
C+        GETANS - Reads the terminal input and returns flags if a     +
C+                 carriage return or "GOTO" is entered.               +
C+        OPNFIL - Opens an input data file and an output data file.   +
C+        CLSFIL - Closes the files.                                   +
C+        UNCODE - A subroutine to write the input line to an internal +
C+                 file so that its data may be read in the appropriate+
C+                 format.  This is a modification of the DEC version  +
C+                 of this routine and eliminates the call to DECODE   +
C+                 which is not supported by PC FORTRAN.               +

C+    WRITTEN BY :  L. W. Richards                                     +
C+                  J. A. McDonald                                     +
C+                                                                     +
C+                 Sonoma Technology Inc.                              +
C+                 3402 Mendoncino ave.                                +
C+                 Santa Rosa, California 95401                        +
C+                 Date: September 1983                                +
C+                                                                     +
C+    PROGRAM VERSION : 01,   MODIFICATION NUMBER : 02                 +
C+    MODIFICATION DATES : 01  9 August 1984                           +
C+                         02  19 August 1987 by Robert Hammarstrand   +
C+                             STI, to run on the IBM PC.              +
C+                         03  22 July 1992 by Donald DiCristofaro,    +
C+                             Sigma Research Corporation to remove    +
C+                             HVS inputs and ability to change        +
C+                             stability with distance.  Also input and+
C+                             output the three required filenames for +
C+                             PLUVUE II.    
C+    Modified By :  Peter Eckhoff
C+                   US EPA
C+                   http://www.epa.gov/ttn/scram/comments.htm         +
C+
C+    Modifications: 
C+        
C+        1) Removed or bypassed non standard Fortran coding
C+        2) Recompiled for 32-bit and tested on 64-bit Windows OSes
C+        3) Established a modification date of 13220 - August 10, 2013
C+---------------------------------------------------------------------+
C+                                                                     +
C+                  SPECIFICATION STATEMENTS                           +
C+                                                                     +
C+---------------------------------------------------------------------+
C
C  TYPE DECLARATIONS
C
      LOGICAL*1 CR,GONE
      CHARACTER*1 IN(80)
C  Date_AND_ Time become integer DT(8) - pae
      INTEGER TT, LUIN, LUOUT, DT
      Character*5 zon
      CHARACTER*10 thetim, thedat
      CHARACTER*33 outfile
c*** Modified by D. DiCristofaro
      CHARACTER*24 file( 3)
c*** End of modification
C
C  DIMENSION DECLARATIONS
C
      DIMENSION DIST(16),RSO2(16),TER(16),SY(16),SZ(16),DT(8)
C  If number of downwind distances > 16, increase above dimensions.
C
      DIMENSION PLANT(6), VALUE(6), ROBJCT(24), RDIST(4), AIO1P(6)
      DIMENSION JANGL(24)
C
C  DATA DECLARATIONS
C
      DATA RDIST/1.,2.,5.,10./, AIO1P/0.02,.05,.1,.2,.5,.8/
C
C+---------------------------------------------------------------------+
C+                                                                     +
C+                     PROGRAM STATEMENTS                              +
C+                                                                     +
C+---------------------------------------------------------------------+
C
C
C  Set LUIN to input device, LUOUT to output device, and open input file
      TT = 6
      LUIN= 1
      LUOUT=2
C
      call system ("CLS")
C  Standarize date and time calls - pae
      call date_and_time (thedat, thetim, zon, DT)
      write (TT, 174) DT(2),DT(3),DT(1), DT(5), DT(6), DT(7)
      write (TT, 175)
C
C  Display welcome message
C
174   format (//60x,'Date: ',2(I2,'/'),I4/60x,'Time: ',2(I2,':'),I2)
175   format (///,37x, 'PLUIN2',//,17x,
     1 'A program to assist in the preparation of input',/,28x,
     2 'data files for PLUVUE II.',//,10x,
     3 'Based on PLUIN1, Written by J.A. McDonald and L. W. Richards',
     4 /,30x,'for WEST Associates.',//,11x,
     5 'PLUIN2 written by R. G. M. Hammarstrand and L. W. Richards',/,
     6 23x,'for use on PC compatible computers.',/,21x,
     7 'Funding provided by the NPS and the EPA.',/)
C
      CALL OPNFIL (outfile)
C
C  Initialize JANGL which is used at ENTRY CODE 33
C
      DO 50 J = 1,24
      JANGL(J) = 15 * J
50    CONTINUE
C
C
C  Read input data file to be reviewed and modified
C
C
c*** Modified by D. DiCristofaro to input the three PLUVUE II filenames
      READ (LUIN,205) FILE
205   FORMAT (A,/,A,/,A)
c*** End of modification
      READ (LUIN,210) (PLANT(J),J=1,6)
      READ (LUIN,250) U,IS,ALAPSE
c*** Modified by D. DiCristofaro to eliminate stability change option
c     READ (LUIN,490) IUSFC,INEW,NXSTAB
      READ (LUIN,490) IUSFC
c*** End of modification
      READ (LUIN,610) YINITL,ZINITL
      READ (LUIN,890) HPBLM
      READ (LUIN,920) RH
      READ (LUIN,1095) IDIS
      READ (LUIN,490) IFLG1,IFLG2,IFLG3,IFLG4,NX2,NT1,NT2,NZF,
     1                NX3, NX4, NX5
      READ (LUIN,490) IDILU,I1HFAU,I1DFAU,I2FAU,I3FAU,I4FAU
      READ (LUIN,611) (DIST(I),I=1,NX2)
      READ (LUIN,670) QSO2,QNOX,QPART
      READ (LUIN,740) FLOW,FGTEMP,FGO2,WMAX
      READ (LUIN,830) UNITS,HSTACK
      READ (LUIN,890) TAMB
      READ (LUIN,920) AMBNOX,AMBNO2,O3AMB,AMBSO2
      READ (LUIN,920) ROVA,ROVC,ROVS,ROVP
      READ (LUIN,920) SIGA,SIGC,SIGS,SIGP
      READ (LUIN,920) DENA,DENC,DENS,DENP
      READ (LUIN,920) ROVCAR,SIGCAR,DENCAR,FRACTC,AMBCAR
      READ (LUIN,920) RFRSO4, RFISO4, RFRCOR, RFICOR
      READ (LUIN,920) RFRPRM, RFIPRM, RFRCAR, RFICAR
      READ (LUIN,920) CORAMB
      READ (LUIN,1095) INTYP
      IF(INTYP .EQ. 1) GO TO 100
      READ (LUIN,920) RVAMB
      GO TO 110
100   READ (LUIN,920) AMBSO4,AMBNO3
110   READ (LUIN,1220) VDSO2,VDNOX,VDCOR,VDSUB
      READ (LUIN,1095) ICON
      READ (LUIN,1270) RSO2C
      IF (ICON .EQ. 1) GO TO 120
      GO TO 130
120   READ (LUIN,1270) (RSO2(NX),NX=1,NX2)
130   READ (LUIN,1650) NC1,NC2
      IF (NC1 .EQ. 2) GO TO 140
      READ (LUIN,490) NPP,NAP,NTP,NZP,IO1P,IPP
140   IF (NC2 .EQ. 1) GO TO 150
      READ (LUIN,740) XOBS,YOBS,ZOBS
150   CONTINUE
      READ (LUIN,740) XSTACK,YSTACK,ZSTACK
      READ (LUIN,1650) IZONE,IMO,IDAY,TTIME,TZONE,IYEAR
      IF( NC2 .EQ. 1 ) GO TO 160
      READ (LUIN,1800) (TER(I),I=1,NX2)
      READ (LUIN,1800) (ROBJCT(NAZ),NAZ=1,24)
      READ (LUIN,890) WIND
160   IF( IDIS .EQ. 9) READ (LUIN,830) (SY(I),SZ(I), I=1,NX2)
C
C   End of reading initial values, begin displaying them on the
C   screen for review and modification.
C
170   CONTINUE
      call system ("CLS")
      WRITE(TT,180)
180   format(/,07h NOTES:,//5X,38h A <RETURN> accepts the current value.
     3//5X, 50h Entering "goto n" or "GOTO n" instead of any data,
     4 17h value will cause
     5 /5x,26h a branch to ENTRY CODE n./
     6/5X,56h Each entry code corresponds to a line in the data file,,
     7 /,5x,55h but options in the input parameters make it so the nth,
     8/5x,50h entry code may not generate the nth line of data.,
     9 //////,
     & 5x,' Press ENTER to continue ...')
C
      call getans (in, cr, gone)
C
190   CONTINUE
      ICARD = 1
      call system ("CLS")
      WRITE(TT, 200) ICARD
200   FORMAT(1x,//////,13H  ENTRY CODE ,I2)
210   FORMAT (6A4)
      WRITE(TT,220) (PLANT(J),J=1,6)
220   FORMAT(//,35H Plant Name (up to 24 characters): ,6A4,/
     1 41H XXXXXXXXXXXXXXXXXXXXXXXX = 24 characters,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 240
230   IF( GONE ) GO TO 1900
      NVAR = 0
      CALL UNCODE(NVAR,IN,VALUE)
      DO 235 J=1,6
      PLANT(J) = VALUE(J)
235   CONTINUE
240   CONTINUE
C
      ICARD = 2
      call system ("CLS")
      WRITE(TT, 200) ICARD
250   FORMAT (F5.1,I5,F5.2,F5.1)
      WRITE(TT,260) U
260   FORMAT(//,13H Wind speed: ,f5.1,4H mph,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 280
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      U = VALUE(1)
270   FORMAT(10F15.8)
280   CONTINUE
      WRITE(TT,290) IS
290   FORMAT(//,17H Stability index:,i3,1h./
     1 60H For Pasquill-Gifford stability classes, use 1. for class A,/
     2 57H 2. = B,  3. = C,  4. = D,  5. = E,  6. = F, AND  7. = G ,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 300
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      IS = VALUE(1)
300   CONTINUE
      WRITE(TT,310) ALAPSE
310   FORMAT(//, 26H Ambient temp. lapse rate: ,f5.2,
     1 19H deg F per 1000 ft.,/)
c Modification by John Vimont National Park Service 6/19/96
c  add in suggestion for default lapse rates for E and F
c  stabilities.  Defaults will yield potential temp lapse 
c  rates used in ISC etc.
      write (tt,311)
311   format (' Suggested values are 5.6 for E stability and 13.825 for 
     +F stability ')
c*** End of Modification
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 320
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      ALAPSE = VALUE(1)
320   CONTINUE
C
C   IUSFC=0 for wind speed aloft, non-zero for surface.
C
      ICARD = 3
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,330) IUSFC
330   FORMAT(//,44H SWITCH: 0. If wind speed at effective stack ,
     1 20h height was entered.   /
     2 53h         1. If wind speed at 10 m height was entered. //
     3 15h Current value:,I3,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 340
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      IUSFC = VALUE(1)
340   CONTINUE
C
c*** Modified by D. DiCristofaro to remove stability change option

c     WRITE(TT,350) NXSTAB
c350   FORMAT(//,57H Index for downwind distance at which the stability i
c    1ndex/32h changes to a secondary value.   //
c    2 62H If the stability index is the same at all downwind distances,
c    3/60h enter a number larger than the number of downwind distances
c    4/37h for which calculations will be done.
c    4 //56H Currently the stability index changes to the secondary
c    5 /35h value at downwind distance number: ,I3,1H.,/)
c     CALL GETANS(IN,CR,GONE)
c     IF( (CR) ) GO TO 360
c     IF( GONE ) GO TO 1900
c     NVAR = 1
c     CALL UNCODE(NVAR,IN,VALUE)
c     NXSTAB = VALUE(1)
c360   CONTINUE
c     WRITE(TT,370) NXSTAB, INEW
c370   FORMAT(//,52H Secondary stability index for downwind distance no.,
c    1 I3,12h and beyond./15H Current value:,i3,1h.,/)
c     CALL GETANS(IN,CR,GONE)
c     IF( (CR) ) GO TO 380
c     IF( GONE ) GO TO 1900
c     NVAR = 1
c     CALL UNCODE(NVAR,IN,VALUE)
c     INEW = VALUE(1)
c*** End of modification

380   CONTINUE
C
C  Read initial plume horizontal and vertical dimensions
c  for a non-point source.
C
      ICARD = 4
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,390) YINITL
390   FORMAT(//, 43H Initial plume y-dimension for area source:,f5.0,
     1 8H meters. /26h Use 0. for point sources.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 400
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      YINITL = VALUE(1)
400   CONTINUE
C
      WRITE(TT,410) ZINITL
410   FORMAT(//, 43H Initial plume z-dimension for area source:,f5.0,
     1 8H meters. /26h Use 0. for point sources.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 420
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      ZINITL = VALUE(1)
420   CONTINUE
C
      ICARD = 5
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,430) HPBLM
430   FORMAT(//,40H Depth of atmosphere that is well mixed:,f7.1,
     1 8H meters./48h If set to zero, vertical mixing is not limited.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 440
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      HPBLM = VALUE(1)
440   CONTINUE
C
      ICARD = 6
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,450) RH
450   FORMAT(//,19H Relative humidity:,f6.2,02H %,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 460
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      RH = VALUE(1)
460   CONTINUE
C
C   Read indicator for type of stability scheme.  Idis=0 for Pasquill-
c  Gifford, 1 for TVA.
C
      ICARD = 7
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,470) IDIS
470   FORMAT(//,38H Flag indicating diffusion parameters:,//,
     1 41h     0. = Pasquill-Gifford-Turner values;,/,
     2 21h     1. = TVA values;,
     3/51h     9. = User input values (at end of input data).,//,
     4 17h The flag is now:,I3,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 480
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      IDIS = VALUE(1)
480   CONTINUE
C
C    Flags for optics analysis routines.  Iflg1=1 for horizontal sight
c    path views.  Iflg2=1 for non-horizontal sight paths.  Iflg3=1 for
c    background object views.  Iflg4=1 for sight paths along the plume
c    centerline. Nt1= starting index for scattering angle array for
c    radiative transfer and scattering calculations.  Nt2= ending index
c    for scatterring angle calculations.  Normally, nt1 is set to 1
c    and nt2 is set to 7 for the 7 generic scattering angles and
c    these values should be used whenever an observer-based run (nc1=1)
c    is being made.  For a generic case run, nt1 and nt2 can be set
c    to limit calculations for less than the 7 angles. Example:  for
c    a generic run for calculations at 90 degrees only, nt1=3 and
c    nt2=4.  Nx2= ending index for array of downwind distances of
c    points for optics calculations.  Nzf=1 for calculations at
c    plume centerline only, 2 for optics calculations at plume
c    centerline and at ground level.
C
      ICARD = 8
      call system ("CLS")
      WRITE(TT, 200) ICARD
490   FORMAT(11I2)
      WRITE(TT,500) IFLG1,IFLG2,IFLG3,IFLG4
500   FORMAT(//,55H Switches to determine if the following calc. are don
     1e./48h   0. = Skip calculation,    1. = Do calculation//
     2 49h  Horizontal  Non-horiz   Background  Along plume/
     3 49h    views       views      object      centerline/
     4 I6,1H.,3(9X,I2,1H.)//
     5 15H (FOUR ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 510
      IF( GONE ) GO TO 1900
      NVAR = 4
      CALL UNCODE(NVAR,IN,VALUE)
      IFLG1 = VALUE(1)
      IFLG2 = VALUE(2)
      IFLG3 = VALUE(3)
      IFLG4 = VALUE(4)
510   CONTINUE
      WRITE(TT,520) NX2
520   FORMAT(//,51H The number of downwind distances for calculations:,
     1 i3,1h./45H At least two distances must be used, and the
     2/46h current dimensions allow up to 16. distances.,/)
C
C  The SAI-EPA PLUVUE model allows up to 16 distances.
C
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 530
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      NX2 = VALUE(1)
530   CONTINUE
C
      WRITE(TT,540) NT1,NT2
540   FORMAT(//,61H Starting and ending indices for light scattering ang
     1les used  ,
     1  /61h in the plume based calculation.  The angles corresponding t
     2o/65h indices 1 through 7 are 0, 22, 45, 90, 135, 158 and 180 degr
     3ees./71H The first angle used has an index one greater than the st
     4arting index, /50h so a starting index of 3 and an ending index of
     5 4 /44h will cause calculations only at 90 degrees.   /
     6 50H If only observer based calculations will be done,  /
     6 31h the OBC values should be used.   ,
     7//38H      Starting index      Ending index//
     8 34H     OBC:    1.                 7./
     9 9H Current:,I5,1H.,13X,I5,1H.//
     5 14H (TWO ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 550
      IF( GONE ) GO TO 1900
      NVAR = 2
      CALL UNCODE(NVAR,IN,VALUE)
      NT1 = VALUE(1)
      NT2 = VALUE(2)
550   CONTINUE
C
      WRITE(TT,560) NZF
560   FORMAT(//,48H Index for number of altitudes for visual impact  ,
     1 14h calculations.,/,
     1 60h Plume centerline only = 1., centerline and ground level =2.,
     2 //, 15H Current value:,I3,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 565
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      NZF= VALUE(1)
C
565   CONTINUE

C
C  ADDITION TO PLUIN FOR HUMAN VISUAL SYSTEM MODELLING (HVS)
c    ( or optical size modeling)
C
      WRITE (TT, 567) NX2, NX3, NX4, NX5
c567   format(' This input is only needed for the version of PLUVUE which
c     1',/,' includes the HVS calculations and is ignored by PLUVUE II.',
567   format(
     2 //, ' Enter the serial numbers of the downwind distances for ',
     3/,' which optical size modeling is to be done.',/,
     4 ' Up to three downwind distances can be selected by entering ',/,
     5 ' their serial numbers (between 1 and ',I2,').',/,
     6 ' If optical size modeling is to be done at less than three'
     7 ,' distances,',/,' enter zeros as needed to complete the three'
     7 ' entries.',//,
     8 ' The present serial numbers of the distances are:',//,3(I2,7x),
     9 //,' (THREE ENTRIES)',/)
C
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 570
      IF( GONE ) GO TO 1900
      NVAR = 3
      CALL UNCODE(NVAR,IN,VALUE)
      NX3 = VALUE(1)
      NX4 = VALUE(2)
      NX5 = VALUE(2)
C
570   CONTINUE
C
C  Switch for turning on print out of table for initial plume dilution.
C  IDILU=0 FOR NO TABLE, 1 FOR TABLE.
C
      ICARD = 9
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,580) IDILU
580   FORMAT(//,62H SWITCH: 0. = No printout of table of initial plume r
     1ise data.,/,26h         1. = Print table.,//,19h The switch is now
     2:,I3,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 582
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      IDILU = VALUE(1)
C
582   continue

C
C  ADDITION TO PLUIN FOR HUMAN VISUAL SYSTEM MODELLING (HVS)
c     (or optical size modeling)
C
      write (TT,584)  I1HFAU, I1DFAU
c584   format(' This input is only needed for the version of PLUVUE which
c    1',/,' includes the HVS calculations and is ignored by PLUVUE II.',
584    format(
     2 //, ' Enter the number of points to be generated',
     3    ' in the vertical scans',/,' by the optical size modeling. ',
     4    ' The maximum number of points is 256.',/,
     5  ' It is recommended that the vertical resolution be set at',/,
     6  ' 256 unless computer resources are limited.'//,
     7        ' The current number of points is: ', I2,1x,I2,/,
     8        ' (Separate the hundreds and tens digits by a space.)',//,
     9        ' (TWO ENTRIES)',/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 586
      IF( GONE ) GO TO 1900
      NVAR = 2
      CALL UNCODE(NVAR,IN,VALUE)
      I1HFAU = VALUE (1)
      I1DFAU = VALUE (2)
C
 586   continue
C
      write (TT,588) I2FAU
c588   format(' This input is only needed for the version of PLUVUE which
c    1',/,' includes the HVS calculations and is ignored by PLUVUE II.',
588   format(
     2 //, ' Enter the resolution desired for the printing',
     3       ' of the spatial images.',/,' 1 - Print every data point.'
     4      ,/, ' 2 - Print every other data point.',/,
     5       ' 3 - Print every third data point, etc.',//,
     6       ' The current resolution is: ', I2,'.',/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 590
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      I2FAU = VALUE(1)
C
590   continue
C
      write (TT,592) I3FAU
c592   format(' This input is only needed for the version of PLUVUE which
c    1',/,' includes the HVS calculations and is ignored by PLUVUE II.',
592   format(
     2 //,' Enter a flag to control the printing and plotting',/,
     3        ' of spatial images of the individual channels.',/,
     4        ' -1 - Each channel is individually',
     5        ' printed and plotted.',/,
     6        '  0 - Both types of printouts and plots are generated.'/,
     7        '  1 - All channels are printed together and only',
     8        ' delta F plotted.',/,
     9        ' The recommended value is 1.',//,
     A        ' The current value of the flag is: ', I2,'.',/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 594
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      I3FAU = VALUE(1)
C
594   continue
C
      write (TT, 596)  I4FAU
c596  format(' This input is only needed for the version of PLUVUE which
c    1',/,' includes the HVS calculations and is ignored by PLUVUE II.',
596   format(
     2  //,' Enter the FORTRAN unit number to be used for the HVS',
     3 ' output.',/, ' This should be set to the same unit being used',
     4 ' for the main PLUVUE output.',//,
     5    ' At present, the output is directed to unit no. ',I2,'.',/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 598
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      I4FAU = VALUE(1)
C
598   CONTINUE
C
C   Read in downwind distances for optics calulations (kilometers).
C   it is recommended that the first 4 downwind distances be set to
c   1., 2., 5., and 10. Km.
C
      ICARD = 10
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,600) NX2
600   FORMAT(//,25H Enter the values for the,i3,19h downwind distances/
     1 44h for which calculations are to be performed.,/)
C Loop to print the recommended values and read the first four distances
      DO 630 I=1,4
      IF(I.GT.NX2) GO TO 660
610   FORMAT(8F10.0)
611   FORMAT(8F10.1)
      WRITE(TT,620) I, DIST(I), RDIST(I)
620   FORMAT(//,13H Distance no.,I3,07h is now,f5.1,4H km.,/,
     1 25h The recommended value is,F5.1,4H km.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 630
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      DIST(I) = VALUE(1)
630   CONTINUE
C
C Loop to read the remaining downwind distances
      IF(NX2.LE.4) GO TO 660
      DO 645 I=5,NX2
      WRITE(TT,640) I, DIST(I)
640   FORMAT(//,44H The current value for downwind distance no.,I3,
     1 4h is:,f6.1,4H km.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 645
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      DIST(I) = VALUE(1)
645   CONTINUE
650   CONTINUE
C
C    Read in SO2, NOx, and particulate emission rates in tons/day
c    for all stacks combined.  (Short tons = 2000 lb, not metric tons)
C
660   CONTINUE
      ICARD = 11
      call system ("CLS")
      WRITE(TT, 200) ICARD
670   FORMAT (3F10.2)
c   Modified by DCD  9/26/92
      write(tt,671)
671   format(//,38H Note:  Emissions must be greater than
     1 17H 0.0 tons per day,/)
c   End of Modification
      WRITE(TT,680) QSO2
680   FORMAT(//,42H Total SO2 emissions rate from all stacks:,f8.2,
     1 14H tons per day./39h Short tons = 2000 lb, not metric tons.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 690
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      QSO2 = VALUE(1)
690   CONTINUE
      WRITE(TT,700) QNOX
700   FORMAT(//,41H Total NOx Emission rate from all stacks: ,f8.2,
     1 14H tons per day.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 710
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      QNOX = VALUE(1)
710   CONTINUE
720   FORMAT(//56H Total primary particle emission rate from all stacks:
     1  ,f7.2,14H tons per day.,/)
      WRITE(TT,720) QPART
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 730
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      QPART = VALUE(1)
730   CONTINUE
C
C    Read in flue gas flow rate per stack (cu ft per min), flue gas
c    exit temperature (deg f), flue gas oxygen concentration (mole-
c    percent), flue gas exit velocity (m/s)
C
      ICARD = 12
      call system ("CLS")
      WRITE(TT, 200) ICARD
740   FORMAT (3F10.1,F10.2)
      WRITE(TT,750) FLOW
750   FORMAT(//,33H Actual gas flow rate per stack:,f10.0,11H cu ft/min.
     1,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 760
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      FLOW = VALUE(1)
760   CONTINUE
C
      WRITE(TT,770) FGTEMP
770   FORMAT(//,27H Flue gas exit temperature:,f6.1,07H deg F.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 780
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      FGTEMP = VALUE(1)
780   CONTINUE
      WRITE(TT,790) FGO2
790   FORMAT(//,32H Flue gas oxygen concentration:,f5.1,14H mole percent
     1.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 800
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      FGO2 = VALUE(1)
800   CONTINUE
      WRITE(TT,810) WMAX
c   Modified by DCD 9/26/92
810   FORMAT(//,40H Flue gas exit velocity (must be > 0.0):,f5.1,
     1    12H meters/sec.,/)
c810   FORMAT(//,24H Flue gas exit velocity:,f5.1,12H meters/sec.,/)
c   End of Modification
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 820
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      WMAX = VALUE(1)
820   CONTINUE
C
C    Read in number of stacks and stack height.
C
      ICARD = 13
      call system ("CLS")
      WRITE(TT, 200) ICARD
830   FORMAT (2F5.1)
840   FORMAT (F10.0)
      WRITE(TT,850) UNITS
850   FORMAT(//,18H Number of stacks:,f4.0,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 860
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      UNITS = VALUE(1)
860   CONTINUE
C
      WRITE(TT,870) HSTACK
870   FORMAT(//,14H Stack height:,f5.0,06H feet.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 880
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      HSTACK = VALUE(1)
880   CONTINUE
C
C    Read in ambient air temperature at stack height.
C
      ICARD = 14
      call system ("CLS")
      WRITE(TT, 200) ICARD
890   FORMAT (F10.1)
      WRITE(TT,900) TAMB
c   Modified by DCD 9/26/92
900   FORMAT(//,40H Ambient air temperature at stack height,/,
     1 36H (must not equal stack temperature):,f6.1,
     1 7H deg F.,/)
c   End of Modification
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 910
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      TAMB = VALUE(1)
910   CONTINUE
C
C    Read in ambient background pollutant concentration in ppm
C    of NOx, NO2, O3, and SO2.
C
      ICARD = 15
      call system ("CLS")
      WRITE(TT, 200) ICARD
920   FORMAT(5F10.3)
      WRITE(TT,930) AMBNOX
930   FORMAT(//,27H Ambient NOx concentration:,f6.3,5H ppm.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 940
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      AMBNOX = VALUE(1)
940   CONTINUE
C
      WRITE(TT,950) AMBNO2
950   FORMAT(//,27H Ambient NO2 concentration:,f6.3,5H ppm.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 960
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      AMBNO2 = VALUE(1)
960   CONTINUE
      WRITE(TT,970) O3AMB
970   FORMAT(//,29H Ambient ozone concentration:,f6.3,5H ppm.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 980
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      O3AMB = VALUE(1)
980   CONTINUE
      WRITE(TT,990) AMBSO2
990   FORMAT(//,27H Ambient SO2 concentration:,f6.3,5H ppm.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1000
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      AMBSO2 = VALUE(1)
1000  CONTINUE
C
C    Read in aerosol size distribution data.  Rova=mass mean radius
c    for background accumulation mode (.1 To 1 micrometer).  Rovc =
c    mass mean radius for background coarse mode (> 1 micrometer).
C    rovs = mass mean radius for plume secondary aerosol.  Rovp =
c    mass mean radius for plume primary aerosol.  Siga = geometric
c    standard deviation of radius of background accumulation mode.
C    sigc = geometric standard deviation of background coarse mode
c    aerosol.  Sigs = geometric standard deviation of radius of
c    plume secondary aerosol.  Sigp geometric standard deviation of
c    radius of plume primary particulate.  Dena=density (g/cm**3) of
c    background accumulation mode aerosol.  Denc = density of background
c    coarse mode aerosol.  Dens = density of plume secondary aerosol
c    and denp = density of plume primary aerosol.
C
      ICARD = 16
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1010) ROVA,ROVC,ROVS,ROVP
1010  FORMAT(//,53H Mass mean radii in um for aerosol size distributions
     6/ 52h (negative values are used to look up non log-normal,/,
     7 30h particle size distributions).,
     1//4X,39H  ---Background---   ------Plume-------
     2/ 4X,39H  Accum     Coarse   Secondary  Primary
     3/ 4X,17H  mode       mode,/,1X,4F10.3,12H micrometers//
     5 15H (FOUR ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1020
      IF( GONE ) GO TO 1900
      NVAR = 4
      CALL UNCODE(NVAR,IN,VALUE)
      ROVA = VALUE(1)
      ROVC = VALUE(2)
      ROVS = VALUE(3)
      ROVP = VALUE(4)
1020  CONTINUE
      ICARD = 17
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1030) SIGA,SIGC,SIGS,SIGP
1030  FORMAT(//,49H Geometric standard deviation of the aerosol size,
     1 14h distribution.
     2/59H (The standard deviation for a monodisperse aerosol = 1.0)./
     2/ 4X,39H  ---Background---   ------Plume-------
     3/ 4X,39H  Accum     Coarse   Secondary  Primary
     4/ 4X,17H  mode       mode,/,1X,4F10.2//
     5 15H (FOUR ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1040
      IF( GONE ) GO TO 1900
      NVAR = 4
      CALL UNCODE(NVAR,IN,VALUE)
      SIGA = VALUE(1)
      SIGC = VALUE(2)
      SIGS = VALUE(3)
      SIGP = VALUE(4)
1040  CONTINUE
      ICARD = 18
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1050) DENA,DENC,DENS,DENP
1050  FORMAT(//,63H Density in grams per cubic centimeter of the aerosol
     1 material.
     2//,4X,39H  ---Background---   ------Plume-------
     3/ 4X,39H  Accum     Coarse   Secondary  Primary
     4/ 4X,17H  mode       mode,/,1X,4F10.2,06H g/cm3//
     5 15H (FOUR ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1055
      IF( GONE ) GO TO 1900
      NVAR = 4
      CALL UNCODE(NVAR,IN,VALUE)
      DENA = VALUE(1)
      DENC = VALUE(2)
      DENS = VALUE(3)
      DENP = VALUE(4)
C
1055  continue
      icard = 19
      call system ("CLS")
      write (TT,200) icard
      write (TT,1058) rovcar, sigcar
1058  format(//,' Log-normal size distribution parameters for the',
     1 ' carbonaceous aerosol.',//,
     2 ' Current values:  Geometric mean      Standard',/,
     3 '                  radius in um        deviation',/,
     4 19x,f5.3, 15x, f5.3,//,' (TWO ENTRIES)',/)
C
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1060
      IF( GONE ) GO TO 1900
      NVAR = 2
      CALL UNCODE(NVAR,IN,VALUE)
      rovcar = value(1)
      sigcar = value (2)
C
1060  WRITE (TT, 1062) DENCAR
1062  FORMAT(//,' Particle density of the carbonaceous aerosol: ',f5.3,
     1 ' g/cm3',/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1064
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      DENCAR = value(1)
C
1064  write (TT, 1066) fractc
1066  format (//,' Fraction (on a scale from zero to one) of the plume',
     1 /,' primary aerosol which is carbonaceous: ',f5.3,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1068
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      fractc = value(1)
C
1068  write (TT,1070) ambcar
1070  format (//,' Concentration of carbonaceous aerosol',
     1 ' in the background atmosphere: ', f5.3,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1075
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      AMBCAR = value(1)
C
 1075 CONTINUE
      icard = 20
      call system ("CLS")
      write (TT,200) icard
      write (TT, 1078) rfrso4, rfiso4,rfrcor, rficor
1078  format (//,' Indices of refraction for the background aerosol'/,
     1 ' in the format m - ik:',//,
     2 4x, 'Accumulation mode             Coarse mode',/,
     3 4x, 'Real    Imaginary          Real    Imaginary',/,
     4 4x, ' m          k               m          k',/,
     5 4x, f5.3, 5x, f5.3, 12x, f5.3, 5x, f5.3,//,' (FOUR ENTRIES)',/)
C
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1080
      IF( GONE ) GO TO 1900
      NVAR = 4
      CALL UNCODE(NVAR,IN,VALUE)
      rfrso4 = value(1)
      rfiso4 = value (2)
      rfrcor = value (3)
      rficor = value (4)
C
1080  CONTINUE
      icard = 21
      call system ("CLS")
      write (TT,200) icard
      write (TT, 1082) rfrprm, rfiprm, rfrcar, rficar
1082  format(//,' Indices of refraction for the emitted primary aerosol'
     1 ,/,' and the carbonaceous aerosol in the format m - ik:',//,
     2 4x, 'Emitted primary aerosol       Carbonaceous aerosol',/
     3 4x, 'Real    Imaginary          Real    Imaginary',/,
     4 4x, ' m          k               m          k',/,
     5 4x, f5.3, 5x, f5.3, 12x, f5.3, 5x, f5.3,//,' (FOUR ENTRIES)',/)
C
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1085
      IF( GONE ) GO TO 1900
      NVAR = 4
      CALL UNCODE(NVAR,IN,VALUE)
      rfrprm = value(1)
      rfiprm = value (2)
      rfrcar = value (3)
      rficar = value (4)
C
1085  CONTINUE
C
C    Read in ambient coarse mode aerosol concentration  (background,)
c    in ug/m.
C
      ICARD = 22
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1088) CORAMB
1088  FORMAT(//,46H Background coarse mode aerosol concentration:,f6.1,
     1 7H ug/m3.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1090
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      CORAMB = VALUE(1)
C
1090  CONTINUE
C
C    INTYP = Parameter to determine input data.  If INTYP=1,
C    background SO4 and NO3 concentrations are input and model
c    computes background visual range.  If INTYP .Ne. one,
c    background visual range is input and model computes background
c    accumulation mode concentration.
C
      ICARD = 23
      call system ("CLS")
      WRITE(TT, 200) ICARD
1095  FORMAT(I5)
      WRITE(TT,1100) INTYP
1100  FORMAT(//,66H Switch: 1. = Input background sulfate and nitrate co
     1ncentrations./8X,49h Any other value = input background visual ran
     2ge.,//,15h Current value:,I3,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1110
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      INTYP = VALUE(1)
1110  CONTINUE
      IF(INTYP.EQ.1) GO TO 1150
C
C    READ IN BACKGROUND VISUAL RANGE.
C
1120  CONTINUE
      ICARD = 24
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1130) RVAMB
1130  FORMAT(//,33H Ambient background visual range:,f6.1,4H km.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1140
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      RVAMB = VALUE(1)
C
C    Read in background sulfate and  nitrate concentrations. (ug/m**3)
C
1140  CONTINUE
      GOTO 1190
C
1150  CONTINUE
      ICARD = 25
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1160) AMBSO4
1160  FORMAT(//,39H Background sulfate mass concentration:,f6.1,
     1 7H ug/m3.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1170
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      AMBSO4 = VALUE(1)
1170  CONTINUE
C
      WRITE(TT,1180) AMBNO3
1180  FORMAT(//,39H Background nitrate mass concentration:,f6.1,
     1 7H ug/m3.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1190
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      AMBNO3 = VALUE(1)
1190  CONTINUE
C
C    Read in deposition velocities. Vdso2 = depostion vel for SO2.
C    vdnox = deposition velocity for NOx.  Vdcor = dep. Vel. For
c    coarse mode particulate. Vdsub =deposition velocity for
c    sub-micron particulate.  Units are cm/s.
C
      ICARD = 26
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1200) VDSO2,VDNOX,VDCOR,VDSUB
1200  FORMAT(//,37H Deposition velocities in cm/sec for:/
     1/59H                                   Coarse-mode   Accum-mode
     2/58H                    SO2     NOx      aerosol       aerosol/
     3/16H Current values:,F6.2,F8.2,F11.2,F14.2//
     5 15H (FOUR ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1210
      IF( GONE ) GO TO 1900
      NVAR = 4
      CALL UNCODE(NVAR,IN,VALUE)
      VDSO2 = VALUE(1)
      VDNOX = VALUE(2)
      VDCOR = VALUE(3)
      VDSUB = VALUE(4)
1210  CONTINUE
1220  FORMAT(16F5.2)
C
C   Read in flag for SO2-to-SO4 conversion rate (in addition to HO
c   chemistry) to be constant with downwind distance (icon=0) or
c   to change with distance from the source (icon=1).
C
      ICARD = 27
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1230) ICON
1230  FORMAT(//,42H Switch to determine the sulfur dioxide to  ,
     1 24h sulfate conversion rate /18h added to the rate  ,
     1 44h calculated from hydroxyl radical chemistry.  //
     1 50H SWITCH: 0. = Added rate the same at all distances ,
     1 18h from the source.,
     2 /,8x,39h 1. = Added rate changes with distance.  /
     2 /22H Recommended value: 0.
     3 /15H Current value: ,I3,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1240
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      ICON = VALUE(1)
1240  CONTINUE
C
C   Read in SO2-to-SO4 conversion rate (%/hr) to be added to rate
c   calculated from HO chemisry.
C
      ICARD = 28
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1250) RSO2C
1250  FORMAT(//,53H Rate of SO2 to sulfate conversion to be added to the
     1 /52h value predicted from the HO chemistry in the model:,f5.1,
     2 12H % per hour. //
     3 24h Recommended value is 0.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1260
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      RSO2C = VALUE(1)
1260  CONTINUE
1270  FORMAT(8F10.7)
      IF(ICON.EQ.1)GO TO 1280
      GO TO 1310
C
C  Read in SO2-to-SO4 conversion rate to be added to rate calculated by
c  HO model, with values corresponding to each of the analysis points
c  downwind from the source.
C
1280  CONTINUE
      ICARD = 29
      call system ("CLS")
      WRITE(TT, 200) ICARD
      DO 1300 NX = 1, NX2
      WRITE(TT,1290) NX, RSO2(NX)
1290  FORMAT(//,71H SO2 to sulfate conversion rate to be added to the ra
     1te calculated from,/,38h the HO model at downwind distance no.,I3,
     21h: ,f5.1,12H % per hour.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1300
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      RSO2(NX) = VALUE(1)
1300  CONTINUE
1310  CONTINUE
C
C  NC1 and NC2 control output
C  NC1=1,NC2=1 for standard tables only (not site specific)
C  NC1=2,NC2=2 for site specific tables only
C  NC1=1, NC2=2 for both tables-- site specific and standard
C
      ICARD = 30
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1320) NC1,NC2
1320  FORMAT(//,44H Switches to control calculations performed./
     1 56H If only observer-based calcuations are done, there will ,
     2 13h be no output / 27h of plume-based calculation ,
     3 22h results for plotting. /,
     4 54h The switch settings available and current values are:,//,
     4 42H    Plume-based calculations only:  1.  1./
     5 42H Observer-based calculations only:  2.  2./
     6 42H                Both calculations:  1.  2.//
     7 34H                   Current values:,i3,1h.,I3,1h.//
     8 15H (TWO  ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1330
      IF( GONE ) GO TO 1900
      NVAR = 2
      CALL UNCODE(NVAR,IN,VALUE)
      NC1 = VALUE(1)
      NC2 = VALUE(2)
1330  CONTINUE
      IF(NC1.EQ.2)GO TO 1560
C
C   Read in indices for controlling plume-based data saved for plotting.
C
1340  CONTINUE
      ICARD = 31
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1350)
1350  FORMAT(//,51H Plume-based calculations have been selected, and a /
     1 53h subset of results must be selected for plotting with ,
     2  9h visplot./45H The six indices to be entered next determine ,
     3 22h the subset of results /32h that will be written to logical ,
     4 17h file unit eight.,/)
      WRITE(TT,1360) NPP
1360  FORMAT(/47H The index NPP determines the distance from the  ,
     1 23h observer to the plume.  /
     1 /,50H                  Distance from observer to plume
     1 / 52h       NPP     (Fraction of background visual range)
     1// 40H        1                    0.02
     1 / 40H        2                    0.05
     1 / 40H        3                    0.10
     1 / 40H        4                    0.20
     1 / 40H        5                    0.50
     1 / 40H        6                    0.80
     1// 26H A recommended value is 3. /
     1   22H The current value is:  ,I3,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1370
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      NPP = VALUE(1)
1370  WRITE(TT,1380) NAP
1380  FORMAT(// 50H The index NAP determines the horizontal azimuthal  ,
     1 20h angle ALPHA between /34h the plume centerline and the line ,
     1 31h of sight for a sky background:
     1// 34H       NAP        ALPHA (degrees)
     1// 30H        1               30
     1 / 30H        2               45
     1 / 30H        3               60
     1 / 30H        4               90
     1// 26H A recommended value is 4. /
     1 22H The current value is:  ,I5,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1390
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      NAP = VALUE(1)
1390  continue
      NT2M1 = NT2 - 1
      WRITE(TT,1400) NT1, NT2M1, NTP
1400  FORMAT(//,51H The index NTP selects the scattering angle between ,
     1 22h the direct solar beam /31h and the line of sight from the ,
     1 36h point of analysis to the observer.  /
     1 56H The value of the index must be greater than or equal to ,
     1 i3,1h./26h and less than or equal to ,I3, 1h.  ,
     1///43H       NTP     Scattering angle (degrees)
     1// 28H        1               22
     1 / 28H        2               45
     1 / 28H        3               90
     1 / 28H        4              135
     1 / 28H        5              158
     1 / 28H        6              180
     1// 25H  The current value is : ,I5,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1410
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      NTP = VALUE(1)
1410  GO TO (1420,1440), NZF
1420  WRITE(TT,1430)
1430  FORMAT(//,50H The index NZF selects the height above ground for  /
     1 51h the line of sight through the plume to be plotted.  /
     1 52H Only plume centerline calculations will be done, so  //
     1 28h you must enter the value 3. )
      GO TO 1460
1440  WRITE(TT,1450)
1450  FORMAT(//,50H The index NZF selects the height above ground for  /
     1 51h the line of sight through the plume to be plotted.  /
     1 55H An index of 3. selects plume centerline height and an  /
     1 44h index of 6. selects a view at ground level.   )
1460  WRITE(TT,1470) NZP
1470  FORMAT(// 25H  The current value is:  ,I5,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1480
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      NZP = VALUE(1)
C
C The order of reading IPP and IO1P are reversed for clarity
C
1480  WRITE(TT,1490) IPP
1490  FORMAT(//,45H The index IPP selects the distance from the
     1 / 51h observer to the plume for plotting results of the
     1 / 52h calculations for views with white, gray, and black
     1 / 30h objects behind the plume.
     1// 50H               Distance from observer to plume
     1 / 50h       IPP  (Fraction of background visual range)
     1// 30H        1               0.02
     1 / 30H        2               0.05
     1 / 30H        3               0.10
     1 / 30H        4               0.20
     1 / 30H        5               0.50
     1 / 30H        6               0.80
     1// 26H    The current value is:  ,I5,1H.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1500
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      IPP = VALUE(1)
1500  WRITE(TT,1510)
1510  FORMAT(//,46H The index IO1P selects the distance from the
     1 / 52h observer through the plume to the white, gray, and
     1 / 52h black background objects behind the plume.  The
     1 / 52h value of IO1P is limited by the value of IPP
     1 / 52h because the object background can be no farther
     1 / 52h than a distance equivalent to 80 percent of the
     1 / 52h background visual range from the observer.
     1 / 52H You may select any value shown in the table.
     1// 52H                Distance from observer to object
     1 / 52h      IO1P   (Fraction of background visual range)
     1 / )
      IF(IPP .EQ. 0) IPP = 1
      K = 7 - IPP
      DO 1530 I = 1,K
      J = I - 1 + IPP
      IWRT = I
      WRITE(TT,1520) IWRT,AIO1P(J)
1520  FORMAT(2X,I5,15X,F4.2)
1530  CONTINUE
      WRITE(TT, 1540) IO1P
1540  FORMAT( // 23H The current value is:  , i5,1h.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1550
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      IO1P = VALUE(1)
1550  CONTINUE
1560  IF(NC2.EQ.1)GO TO 1590
C
C    For runs with specific case calculations, increase number of
c    scattering angles for all the specific viewer line-of-sight
c    geometries.
C
c    Read in observer position:  UTM x-coordinate (km), UTM y-coordinate
c    (km), elevation (ft,msl) for observer-based calculations.
C
1570  CONTINUE
      ICARD = 32
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1580) XOBS,YOBS,ZOBS
1580  FORMAT(//,26H Position of the observer:,//,
     1 36h       UTM coordinates    Elevation  ,/,
     2 35h      X (km)     Y (km)    (ft msl),/,1x,3f11.1//
     5 16H (THREE ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1590
      IF( GONE ) GO TO 1900
      NVAR = 3
      CALL UNCODE(NVAR,IN,VALUE)
      XOBS = VALUE(1)
      YOBS = VALUE(2)
      ZOBS = VALUE(3)
1590  CONTINUE
C
C     Read in source position:  UTM x-coordinate, y-coordinate, elevation
c    for all runs.
C
      ICARD = 33
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1600) XSTACK,YSTACK,ZSTACK
1600  FORMAT(//,40H UTM coordinates of the source in km and/
     1 48h elevation of the base of the stack in feet msl.//
     2 37h                      UTM coordinates/
     4 49h                       X          Y     Elevation/
     5 16h Current values:,3F11.1//
     5 16H (THREE ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1610
      IF( GONE ) GO TO 1900
      NVAR = 3
      CALL UNCODE(NVAR,IN,VALUE)
      XSTACK = VALUE(1)
      YSTACK = VALUE(2)
      ZSTACK = VALUE(3)
1610  CONTINUE
C
C    Read UTM grid zone number, month, day, time (24 hour military),
c    time zone number, and year.
C   IZONE is the universal transverse mercator grid zone number
c   read from a USGS map
c   TZONE is the time zone number, counting west from Greenwich,
c   add one for daylight savings time.
C
      ICARD = 34
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1620)
1620  FORMAT(/41H The code numbers for the time zones are: //
     1 44H Time zone         Standard        Daylight    /
     1 44H                     time            time      /
     2 44H Eastern               5.              4.      /
     3 44H Central               6.              5.      /
     4 44H Mountain              7.              6.      /
     5 44H Pacific               8.              7.      //
     6 38H A time of 3:45 pm is entered as 1545.   )
      WRITE(TT,1630) IZONE,IMO,IDAY,TTIME,TZONE,IYEAR
1630  FORMAT(/,16X,
     1 59H   UTM grid   Month    Day of     Time        Time     Year/
     2 18X,51H zone no.  (number)   month  (24hr clock)  zone no./
     3 16H Current values:,I7,1H.,7X,I2,1H.,6X,I2,1H.,2F11.0,6X,I4,1H.//
     5 14H (SIX ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1640
      IF( GONE ) GO TO 1900
      NVAR = 6
      CALL UNCODE(NVAR,IN,VALUE)
      IZONE = VALUE(1)
      IMO = VALUE(2)
      IDAY = VALUE(3)
      TTIME = VALUE(4)
      TZONE = VALUE(5)
      IYEAR = VALUE(6)
1640  CONTINUE
1650  FORMAT(3I5,2F5.0,I5)
C
C    Skip over read statements if run is for a plume-based case only.
C
      IF(NC2.EQ.1)GO TO 1820
C
C    Read in elevation of terrain at each downwind point (ft msl). If
C    TER(1)=0, model sets up flat terrain for all points.  These changes
c    are used only in the calculation of the elevation angle BETA of the
c    specific lines of sight used for the observer-based calculations.
C    the gaussian dispersion calculations use flat terrain.
C
1660  CONTINUE
      ICARD = 35
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1670) TER(1)
1670  FORMAT(/47H Elevation of terrain at each downwind point in  ,
     1 10h feet msl,
     2 /56H to be used only in calculation of view elevation angle./
     3 60H If zero is entered for the first distance, the elevation at/
     4 58h the base of the stack is used for all downwind distances./
     5 47H Enter a value for each distance one at a time.//
     6 38H Current value for the first distance:  ,f6.0,8H ft msl.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1680
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      TER(1) = VALUE(1)
1680  CONTINUE
      IF (TER(1).EQ.0.) GO TO 1720
1690  DO 1710 I = 2,NX2
      WRITE(TT,1700) I,TER(I)
1700  FORMAT(//,27H Elevation for distance no.,I3,04h is:,f6.0,8H ft msl
     1.,/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1710
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      TER(I) = VALUE(1)
1710  CONTINUE
1720  CONTINUE
C
C    Read in background object distances from observer through plume
c    to background terrain for line-of-sight azimuths of 15 deg.,
C    30 deg., 45 deg., ... , 360 deg.  The distance for each line-
c    of-sight azimuth actually used is interpolated from these values.
C    If ROBJCT(i) = 0, the object distance is set equal to the plume-
c    observer distance along the line-of-sight to the ith downwind point
c
      ICARD = 36
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1730)
1730  FORMAT(//,36H Read in background object distances
     1/50h from observer through plume to background terrain
     2/73h for line-of-sight azimuths of 15, 30, 45, ... , 360 deg from
     3true north.,
     4/58H The distance for each line-of-sight azimuth actually used
     5/35h is interpolated from these values.
     6//58H If the distance at 15 deg is set equal to zero, the model
     7/57h will set the object distance equal to the plume-observer  ,
     8/57h distance along the line-of-sight to each downwind point. )
C
      DO 1760 I = 1,4
      IB = (I - 1) * 6 + 1
      IE = I * 6
      WRITE(TT,1740) ( JANGL(J),J=IB,IE)
1740  FORMAT(//,48H  Distances to the background objects in km are:
     1 //6(i6,4h deg))
      WRITE(TT,1750) (ROBJCT(NAZ),NAZ=IB,IE)
1750  FORMAT(6F10.2, // 14H (SIX ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1756
      IF( GONE ) GO TO 1900
      NVAR = 6
      CALL UNCODE(NVAR,IN,VALUE)
      DO 1754 J = 1,6
      NAZ = (I-1)*6 + J
      ROBJCT(NAZ) = VALUE(J)
1754  CONTINUE
1756  IF(ROBJCT(1).EQ. 0.) GO TO 1770
1760  CONTINUE
      GO TO 1790
1770  DO 1780 NAZ = 2,24
      ROBJCT(NAZ) = 99.
1780  CONTINUE
1790  CONTINUE
1800  FORMAT(8F10.1)
C
C  Read in wind direction (from which wind is blowing)
c  in degrees from north
C
      ICARD = 37
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT, 1810) WIND
c   Modified by DCD   9/26/92
1810  FORMAT(//,42H Direction from which the wind is blowing ,/,
     1 48H (Do not point plume directly at an observer): ,F6.1,
     2 10H deg true.,/)
c   End of Modification
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1820
      IF( GONE ) GO TO 1900
      NVAR = 1
      CALL UNCODE(NVAR,IN,VALUE)
      WIND = VALUE(1)
1820  CONTINUE
      IF (IDIS .NE. 9) GO TO 1880
1830  ICARD = 38
      call system ("CLS")
      write(TT, 200) ICARD
      write(TT,1840)
      call display (6)  ! pause as message may scroll off screen.
      write (TT, 1845)
1840  FORMAT(//10x,'The index at entry code 7. indicates that user',/,
     1 10x,'input dispersion parameters are to be used.',//,
     2 10x,'The current values in meters are:',//)
1845  format( 37H       DIST       SY         SZ        / )
      WRITE(TT,1850) (DIST(I),SY(I),SZ(I), I=1,NX2)
1850  FORMAT(3(5X,F5.1))
      DO 1870 I=1,NX2
      WRITE(TT,1860) I, DIST(I),SY(I),SZ(I)
1860  FORMAT(/49H Enter the dispersion parameters for distance no. ,
     1 I3,11h., which is,f6.1,4H km.  /
     1 29H The current values are SY = ,f5.1, 17H meters and SZ = ,
     1 f5.1, 8H meters.  //14H (TWO ENTRIES),/)
      CALL GETANS(IN,CR,GONE)
      IF( (CR) ) GO TO 1870
      IF( GONE ) GO TO 1900
      NVAR = 2
      CALL UNCODE(NVAR,IN,VALUE)
      SY(I) = VALUE(1)
      SZ(I) = VALUE(2)
1870  CONTINUE
1880  CONTINUE
      ICARD = 39
      call system ("CLS")
      WRITE(TT, 200) ICARD
      WRITE(TT,1890)
1890  FORMAT(// 28h To save the current values,
     1 25h enter a carriage return. /
     2 38h Otherwise, enter "goto n" or "GOTO N",
     3 29h to go back to entry point n./ )
      CALL GETANS(IN,CR,GONE)
      IF (CR) GO TO 1920
      IF( GONE ) GO TO 1900
      GO TO 1920
C
C Branch to the location in the input data file requested by an
c input of "goto" followed by a number.
C
1900  NVAR = -1
      CALL UNCODE(NVAR,IN,VALUE)
      IGO = VALUE(1)
      if (igo .ge. 39) igo = 39
      GO TO (190,240,320,380,420,440,460,480,570,598,650,730,820,880,
     1 910,1000,1020,1040,1055,1075,1080,1085,1090,1120,1150,1190,1210,
     2 1240,1280,1310,1340,1570,1590,1610,1660,1720,1790,1830,1880), IGO
C
C Write the corrected pluvue input to the output file
C
1920  CONTINUE
      call system ("CLS")
      WRITE(TT, 1930)
1930  FORMAT(//////////,20x,21H Writing output file.)
c*** Modified by D. DiCristofaro to output the three PLUVUE II filenames
      WRITE (LUOUT,205) FILE
c*** End of modification
      WRITE(LUOUT,210) (PLANT(J),J=1,6)
      WRITE(LUOUT,250) U,IS,ALAPSE
c*** Modified by D. DiCristofaro to remove stability change option
c     WRITE(LUOUT,490) IUSFC,INEW,NXSTAB
      WRITE(LUOUT,490) IUSFC
c*** End of modification
      WRITE(LUOUT,610) YINITL,ZINITL
      WRITE(LUOUT,890) HPBLM
      WRITE(LUOUT,920) RH
      WRITE(LUOUT,1095) IDIS
      WRITE(LUOUT,490) IFLG1,IFLG2,IFLG3,IFLG4,NX2,NT1,NT2,NZF,
     1                 NX3, NX4, NX5
      WRITE(LUOUT,490) IDILU,I1HFAU,I1DFAU,I2FAU,I3FAU,I4FAU
      WRITE(LUOUT,611) (DIST(I),I=1,NX2)
      WRITE(LUOUT,670) QSO2,QNOX,QPART
      WRITE(LUOUT,740) FLOW,FGTEMP,FGO2,WMAX
      WRITE(LUOUT,830) UNITS,HSTACK
      WRITE(LUOUT,890) TAMB
      WRITE(LUOUT,920) AMBNOX,AMBNO2,O3AMB,AMBSO2
      WRITE(LUOUT,920) ROVA,ROVC,ROVS,ROVP
      WRITE(LUOUT,920) SIGA,SIGC,SIGS,SIGP
      WRITE(LUOUT,920) DENA,DENC,DENS,DENP
      WRITE(LUOUT,920) ROVCAR,SIGCAR,DENCAR,FRACTC,AMBCAR
      WRITE(LUOUT,920) RFRSO4, RFISO4, RFRCOR, RFICOR
      WRITE(LUOUT,920) RFRPRM, RFIPRM, RFRCAR, RFICAR
      WRITE(LUOUT,920) CORAMB
      WRITE(LUOUT,1095) INTYP
      IF(INTYP .EQ. 1) GO TO 1940
      WRITE(LUOUT,920) RVAMB
      GO TO 1950
1940  WRITE(LUOUT,920) AMBSO4,AMBNO3
1950  WRITE(LUOUT,1220) VDSO2,VDNOX,VDCOR,VDSUB
      WRITE(LUOUT,1095) ICON
      WRITE(LUOUT,1270) RSO2C
      IF (ICON .EQ. 1) GO TO 1960
      GO TO 1970
1960  WRITE(LUOUT,1270) (RSO2(NX),NX=1,NX2)
1970  WRITE(LUOUT,1650) NC1,NC2
      IF (NC1 .EQ. 2) GO TO 1980
      WRITE(LUOUT,490) NPP,NAP,NTP,NZP,IO1P,IPP
1980  IF (NC2 .EQ. 1) GO TO 1990
      WRITE(LUOUT,740) XOBS,YOBS,ZOBS
1990  CONTINUE
      WRITE(LUOUT,740) XSTACK,YSTACK,ZSTACK
      WRITE(LUOUT,1650) IZONE,IMO,IDAY,TTIME,TZONE,IYEAR
      IF( NC2 .EQ. 1 ) GO TO 2000
      WRITE(LUOUT,1800) (TER(I),I=1,NX2)
      WRITE(LUOUT,1800) (ROBJCT(NAZ),NAZ=1,24)
      WRITE(LUOUT,890) WIND
2000  IF( IDIS .EQ. 9) WRITE (LUOUT,830) (SY(I), SZ(I), I=1,NX2)
C
C   CLOSE FILES
C
       CALL CLSFIL
      call system ("CLS")
      write (TT,2900) outfile
2900  format (//////,20x, 'Current values saved to output file ',//,
     1 20x,A33,//,20x,'Carriage return to exit',//,20x,
     2 'or enter "goto n" to return to entry code n.',/)
C
      CALL GETANS(IN,CR,GONE)
      IF (CR) then
          GO TO 2950
      else if ( GONE ) then
          open (unit=luout,file=outfile)
          GO TO 1900
      endif
C
2950  continue
      call system ("CLS")
      write (TT,3000)
3000  format (////////////,20x,'Program PLUIN2 terminating.')
C
3010  CONTINUE
C  Display message for 2 seconds
C
      call display(2)
      CALL EXIT
      END
       subroutine GETANS(IN,CR,GONE)
*
*  Version for the IBM PC and compatibles
*
*  Modified 19-August 1987 by Robert G. M. Hammarstrand,
*  Sonoma Technology Inc. (STI).
*
*  This subroutine checks to see if a carriage return or "GOTO"
*  has been entered and sets the value of CR and GONE accordingly.
*
       CHARACTER*1 IN(80)
       LOGICAL*1 CR,GONE
*
*     initialise
*
       DO 10 I = 1,80
       IN(I) = ' '
  10   CONTINUE
       CR = .TRUE.
       GONE = .FALSE.
*      GET IT
       READ(5,100) (in(i), i = 1, 80)
 100   FORMAT(80A1)
*
*  If all 80 characters are blank, it is assumed a carriage return was
*  entered and CR is left true.  Any non-blank character will cause
*  the flag CR to be set false.
*
       DO 600 I=1,80
        IF(IN(I) .NE.' ') CR = .FALSE.
 600   CONTINUE
       IF (  CR  ) RETURN
*
*  Check to see if the first four characters are "GOTO"
*  and if so, set the flag GONE true.
*
       IF((IN(1).EQ.'G').AND.(IN(2).EQ.'O').AND.(IN(3).EQ.'T')
     1    .AND.(IN(4).EQ.'O')) GONE = .TRUE.
       IF((IN(1).EQ.'g').AND.(IN(2).EQ.'o').AND.(IN(3).EQ.'t')
     1    .AND.(IN(4).EQ.'o')) GONE = .TRUE.
       RETURN
       END
      subroutine UNCODE(NVAR,IN,VALUE)
*
*  Version for the IBM PC and compatibles
*
*  Modified 19-August 1987 by Robert G. M. Hammarstrand,
*  Sonoma Technology Inc. (STI).
*
*  The UNCODE routine writes the input line to an internal (virtual)
*  file so that its data may be read in the required format,
*  depending on the designated branch. The internal file only exists
*  during the execution of the program. This routine replaces the UNCODE
*  and DECODE calls in the DEC version of this program.
*
*  NVAR is the number of values to be returned in the array VALUE.
*
*  NVAR = 0 and -1 are used as flags to change the format associated
*  with the UNCODE.
*
      CHARACTER*1 IN(80)   ! input string
*
      CHARACTER*95,RAMFILE /'
     1                                                    '/
      DIMENSION VALUE(6)
*
*  Determine the number of values entered
*
      numvals = 0
      do 5 i = 1, 79
          if((in(i) .ne. ' ') .and. (in(i+1) .eq. ' ')) then
              numvals = numvals + 1
          endif
5     continue
*
      write (ramfile, 100) (in(i), i=1,80)
      IF (NVAR) 10, 30, 50
10    read (ramfile, 20) value(1)
      RETURN
30    read (ramfile, 40) (value(i), i=1,6)
      RETURN
50    read (ramfile, *) (value(i), i=1, numvals)
      RETURN
20    FORMAT(5X,F3.0)
40    FORMAT(6A4)
100   FORMAT (95A1)
      END
      subroutine DISPLAY (N)
*
* Waits N seconds before continuing execution.
*
      PAUSE('Press Enter to continue')
C      N = N*100
C      call timer (iticks1)
C      do 100 i = 1, 10000
C          call timer (iticks)
C          if ((iticks-iticks1) .gt. N) goto 200
C100   continue
*
200   return
      end
      subroutine OPNFIL (outfile)
*
*  Version for the IBM PC and compatibles
*
*  Written 19-August-1987 by Robert G. M. Hammarstrand,
*  Sonoma Technology Inc. (STI).
*
* Open input and output files on IBM PC and compatible systems.
*
c*** Modified 22 July 1992 by Donald C. DiCristofaro
c*** Sigma Research Corporation
c*** To output outfile name to scratch file and to increase size of path
      logical*1 cr, gone
      integer ioval, luin, luout
      character*1 filename(12), path(50), dir(55), in(80), reply
      character*53 infile, outfile
      character*55 shodir
      character*100 ramfile
*
      luin = 1
      luout = 2
      dir(1) = 'd'
      dir(2) = 'i'
      dir(3) = 'r'
      dir(4) = ' '
*
C  Open the file with the name of the template and final output file
      open(unit=1,file='PLUin2.CTL', status="old", iostat=ioval)
       if (ioval .eq. 0) THEN
        read(1,*) infile
        read(1,*) outfile
       close (1)
      
       open(unit=1,file=infile, status="old", iostat=ioval)
         write(*,202) infile
 202      format(1x,' Template file successfully opened.  '
     1          /4x,'File: ',A53/)
       open(unit=2,file=outfile, iostat=ioval)
         write(*,204) outfile
 204      format(1x,' Output file successfully opened.  '
     1          /4x,'File: ',A53/)
      endif 

      if (ioval .gt. 0) then
        Write(*,198)
 198      format (/" Program could not find PLUIN2.CTL or one of the"/ 
     1             " files listed inside the CTL file. The program"/
     2             " will continue to execute using manually entered"/ 
     3             " data."//)
        Write(*,199)
 199      format ( " To use a CTL file, please enter CTL+C on the next"/
     5             " available entry line. This will stop the program."/
     6             " Then create or rename your file to PLUIN2.CTL."/)
50    write (*,200)
      read (*,190) (path(j), j=1,50)
      if ((path(1).eq.'a'.or.path(1).eq.'b').and.path(2).eq.':') then
          call system ("CLS")
          write (*,203) path(1), path(2)
          call getans (in,cr,gone)
      end if
      do 60 i = 1, 50
          if (path(i) .eq. ' ') goto 70
60    continue
70    lenpath = i-1
      write(ramfile,*) (dir(j), j=1,4), (path(j), j=1,50)
      read(ramfile,195) shodir
      call system ("CLS")   ! clear the screen !
c      call system (shodir)  ! directory where data files located
      write (*,205)
      read (*,207) (filename(j), j = 1,12)
      if (lenpath .gt. 0) then
          write(ramfile,*) (path(j), j=1,lenpath), '\',
     1                     (filename(j), j=1,12)
      else
          write (ramfile, *) (filename(j), j=1,12)
      endif
      read(ramfile,*) infile
      open (unit=luin, file=infile, status="old", iostat=ioval,
     1 recl=132, err=100)
100   if (ioval .gt. 0) then
          write (*,220)
          goto 50
      end if
120       write (*,210)
      read (*,207) (filename(j), j=1,12)
      if (lenpath .gt. 0) then
          write(ramfile,*) (path(j), j=1,lenpath), '\',
     1                     (filename(j), j=1,12)
      else
          write (ramfile, *) (filename(j), j=1,12)
      endif
      read(ramfile,*) outfile
      open (unit=luout, file=outfile, status="new", iostat=ioval,
     1 recl=132, err=150)
150   if (ioval .eq. 0) then
          write (*,230) outfile
c*** Modified by D. DiCristofaro
          open( 7,file='scratch2',status='unknown')
          write( 7,180) outfile
180       format( a)
c*** End of modification
      else
          close (luout)
          write (*,400) outfile
          read (*,*) reply
          if((reply .eq. 'y') .or. (reply .eq. 'Y')) then
              open (unit=luout, file=outfile, status='OLD')
c*** Modified by D. DiCristofaro
              open (7,file='scratch2',status='unknown')
              write (7,180) outfile
c*** End of modification
          else
              call system ("CLS")
              call system (shodir)  ! directory where data files located
              goto 120
          end if
      end if
      end if
190   format(50A1)
195   format(A55)
200   format(//' Enter drive and path where data files are located.'
     1 ,/,' (Carriage return to select default directory).'
     2 ,/,'Example: C:\Pluvue\testcase',
     3 ,/,'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX = ',
     4 '50 characters maximum.',/)
203   format (///////,10x,'Check floppy diskette in drive ',2A1,//
     1 10x, 'ENTER  to continue ...')
205   format (' Enter input filename, 12 characters maximum.',
     1 /, ' XXXXXXXX.XXX',/)
207   format (13A1)
c   Modified by DCD 9/26/92
210   format(' Enter filename which contains revisions. ',
     1  '12 characters maximum. ',/,' (The filename can be the same ',
     2  'as the input filename listed above.)',
     3 /, ' XXXXXXXX.XXX',/)
c210   format(' Enter output filename. 12 characters maximum. ',
c     1 /, ' XXXXXXXX.XXX',/)
c   End of Modification
220   format (' Error opening file. File does not exist. Try again.')
230   format (' Opening new file ',A33,/)
400   format (1x,'File ',A33,/,' exists. Do you wish to overwrite it? ')
      return
      end
      subroutine CLSFIL
*
*  Version for the IBM PC and compatibles
*
*  Written 19-August-1987 by Robert G. M. Hammarstrand,
*  Sonoma Technology Inc. (STI).
*
* Close files on the IBM PC and clones
*
      integer willard, luin, luout
*
      luin = 1
      luout = 2
*
* Close input file
*
      close (unit = luin, iostat = willard, err = 2001, status = 'keep')
*
* Close output file
*
      goto 2005
*
2001  write (6,3000) willard
*
2005  close(unit = luout, iostat = willard, err = 2010, status = 'keep')
*
* Return if file closure successful
*
      goto 4000
*
2010  write (6,3010) willard
*
3000  format (//' ERROR on closing INPUT file.',/,' Completion status
     1 code = ',i2)
*
3010  format (//' ERROR on closing OUTPUT file.',/,' Completion status
     1 code = ',i2)
*
4000  return
      end
