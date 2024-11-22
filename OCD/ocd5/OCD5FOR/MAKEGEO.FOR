      program makegeo
c
c -------------------------------------------------------------------
c    This is program is adapted from TOPO-MAP.FOR, originally
c    developed by Minerals Management Service, to prepare the
c    shoreline geometry file (file of Ws and Ls) to be included
c    in the OCD 5 self-documenting control file.  The main changes
c    include (1) all screen outputs are disabled, (2) all keyboard
c    inputs are directed to a file, and (3) the output file format
c    is changed.
c
c    The program must be compiled with the Lahey Fortran compiler
c    with the option that headers are added to the direct access file,
c
c    ========>      i.e., the /nD switch specified.    <=========
c
c    This is because the database (*.BIN) files were created by MMS
c    with that assumption.
c
c    Joseph C. Chang
c    EARTH TECH
c    196 Baker Avenue
c    Concord, MA 01742
c    Tel (508)371-4256
c    Fax (508)371-4280
c    E-mail chang@src.com or jchang@earthtech.com
c
c
c OCD             Revision History:
c    First created: 961122
c    Last revised:  970203
c
c -------------------------------------------------------------------
c
C  PROGRAM THAT MAKES A TOPO MAP FILE (FILE OF Ws AND Ls) FOR USE
C   IN THE OCD MODEL

C  PROGRAM CREATES A GENERAL FILE BASED ON USE INPUTS THEN MAKES
C   A TEMP TARGRT MATRIX FILE (DRX) THEN MAKES THE TOPO FILE
      CHARACTER*60 FILE
      REAL*8 SLON1,SLON2,SLAT1,SLAT2
      CHARACTER*1 BELL
cjc  JC modifications of 1/24/97.  Also declare LEXIST.
      logical lexist
cjc  End of JC modifications.
      COMMON /PARMS/ A1,B1,C1,A2,B2,C2,IX,IY,LEVMIN                     
      COMMON /LL/ SLON1,SLON2,SLAT1,SLAT2
      DATA BELL/Z'07'/

cjc  JC modifications of 1/9/97.  Open input file that includes all
cjc  input parameters.
      open (unit=1,file='makegeo.dat',status='old')
cjc  End of JC modifications.

C  CALL ROUTINE TO CALC COEF VALUES
      CALL GENERAL(NAREA)

cjc  JC modifications of 1/9/97.  Disable the following screen output
cjc  and hardwire output file names
cjc      PRINT *
cjc      PRINT *, 'What is the name of the Topo output file?'
cjc      PRINT *, 'Enter here: '
cjc      READ(*,1) FILE
cjc1     FORMAT(A60)
cjc      OPEN(3,FILE=FILE,FORM='FORMATTED',STATUS='NEW')
c
c     Ensure that the file MAKEGEO.SHR is always up-to-date.
c
      inquire (file='makegeo.shr',exist=lexist)
      if (lexist) then
        open (unit=3,file='makegeo.shr',status='old')
        close (unit=3,status='delete')
      end if
      open (unit=3,file='makegeo.shr',status='new')
c
c     Ensure that the file MAKEGEO.REF is always up-to-date.
c
      inquire (file='makegeo.ref',exist=lexist)
      if (lexist) then
        open (unit=9,file='makegeo.ref',status='old')
        close (unit=9,status='delete')
      end if
      open (unit=9,file='makegeo.ref',status='new')
c
c     Ensure that the file MAKEGEO.CAR is always up-to-date.
c
      inquire (file='makegeo.car',exist=lexist)
      if (lexist) then
        open (unit=8,file='makegeo.car',status='old')
        close (unit=8,status='delete')
      end if
      open (unit=8,file='makegeo.car',status='new')
cjc  End of JC modifications.

cjc  JC modifications of 1/9/97.  Read input from input file,
cjc  MAKEGEO.DAT.
cjc11    PRINT *
cjc      PRINT *
cjc      PRINT *
cjc      PRINT *, 'How many cells on X (values from 60 - 120 in '
cjc      PRINT *, '    increments of 10): '
cjc      READ(*,2) IXG
cjc2     FORMAT(I6)
cjc      IF(IXG.LT.60 .OR. IXG.GT.120 .OR. ((IXG/10)*10).NE.IXG) THEN
cjc        PRINT *, BELL
cjc        WRITE(*,2001)
cjc2001    FORMAT(25(/))
cjc        PRINT *, 'You entered a bad value - TRY AGAIN' 
cjc        GO TO 11
cjc      ENDIF
cjc13    PRINT *
cjc      PRINT *
cjc      PRINT *
cjc      PRINT *, 'How many cells on Y (values from 60 - 120 in '
cjc      PRINT *, '    increments of 10): '
cjc      READ(*,2) IYG
cjc      IF(IYG.LT.60 .OR. IYG.GT.120 .OR. ((IYG/10)*10).NE.IYG) THEN
cjc        PRINT *, BELL
cjc        WRITE(*,2001)   
cjc        PRINT *, 'You entered a bad value - TRY AGAIN' 
cjc        GO TO 13
cjc      ENDIF
      print *
      print *,'No. of cells on X must be from 60 to 120, and in incremen
     &ts of 10.'
      print *,'No. of cells on Y must be from 60 to 120, and in incremen
     &ts of 10.'
      read (1,*) ixg
      read (1,*) iyg
      if(ixg.lt.60 .or. ixg.gt.120 .or. ((ixg/10)*10).ne.ixg) then
        print *,'No. of cells on X must be from 60 to 120, and in increm
     &ents of 10.'
        stop
      end if
      if(iyg.lt.60 .or. iyg.gt.120 .or. ((iyg/10)*10).ne.iyg) then
        print *,'No. of cells on Y must be from 60 to 120, and in increm
     &ents of 10.'
        stop
      end if
cjc  End of JC modifications.

cjc  JC modifications of 1/9/97.  Add the following screen message
      print *
      print *,'Calculations started, which might take a few minutes.  Be
     & patient...'
C NOW CREATE THE TEMP DRX FILE (SOURCE CODE OF TGTMATRX IS USED)
      CALL TGTMATRX(NAREA)

      CALL DRXMAP(IXG, IYG)
     
      CLOSE(40,STATUS='DELETE')
      CLOSE(50,STATUS='DELETE')
      print *
      PRINT *, 'ALL IS DONE AND OK'
      STOP
      END
C*************************************************************
      SUBROUTINE DRXMAP(IXGRID,IYGRID)
      COMMON /PARMS/ A1,B1,C1,A2,B2,C2,IX,IY,LEVMIN                     
      LOGICAL*1 A
      COMMON /MAP/  A(250000)                               
      INTEGER*4 DRX(1200)  
      CHARACTER*1 MAP(240,240), LETL, LETW
      CHARACTER*60 FILE
cjc  JC modifications on 1/9/97.  Add the following statements.
      real*8 slon1,slon2,slat1,slat2
      common /ll/ slon1,slon2,slat1,slat2
      real xsw,ysw,xne,yne
      integer iz0
      common /utm/xsw,ysw,xne,yne,iz0
      character*20 ifmt2,ifmt3
      character*10 b10
      character*2 dls,dle
      character*3 ender
      data b10/'          '/
      data dls/'! '/
      data dle/' !'/
      data ender/'END'/
cjc  End of JC modifications.
      DATA LETW/'W'/, LETL/'L'/

      DO 10 I=1,240
      DO 10 J=1,240
10    MAP(I,J)=' '

C  REWIND FILE 50 - DRX FILE
      REWIND (50)

C  CALC HOW MANY X AND Y DRX CELLS = A MAP CELL
      CELLX=FLOAT(IX)/FLOAT(IXGRID)
      CELLY=FLOAT(IY)/FLOAT(IYGRID)
      POSY=0.0

C  READ IN THE DRX FILE AND FILL LOGIC FILE
      ILAND=0
      IWATER=0
      MM=0
      DO 100 J=1,IY
cjc  JC modifications of 1/9/97.  Disable the following print statement
cjc      IF((J/20)*20.EQ.J) PRINT *, J,' OF',IY,
cjc     X   '    LAND/WATER',ILAND, IWATER, '  OF',MM
cjc  End of JC modifications.
      READ(50,*) (DRX(N),N=1,IX)

      DO 70 I=1,IX
      MM=MM+1
      IF (DRX(I).NE.0) THEN
        A(MM)=.TRUE.
        ILAND=ILAND+1
        ELSE
        A(MM)=.FALSE.
        IWATER=IWATER+1
      ENDIF
70    CONTINUE
100   CONTINUE

C  NOW GO THRU AND SEE WHICH MAP CELLS HAVE 50% OR MORE = TO LAND

      IENDY=0
      DO 1000 J=IYGRID, 1, -1
cjc  JC modifications of 1/9/97.  Disable the following print statement
cjc      PRINT *, J
cjc  End of JC modifications.
      ISTARTY=IENDY+1         
      POSY=POSY+CELLY
      IENDY=INT(POSY + 0.5)

      IENDX=0
      POSX=0.0
      DO 1000 I=1, IXGRID
      ISTARTX=IENDX+1        
      POSX=POSX+CELLX
      IENDX=INT(POSX + 0.5)
 
      AVG=0.0
      TOT=0.0
      DO 500 NY=ISTARTY, IENDY
      DO 500 NX=ISTARTX, IENDX

      IP=(NY-1)*IX + NX
      AVG=AVG+1.0
      IF(A(IP)) TOT=TOT+1.0
500   CONTINUE
 
      IF((TOT/AVG).GE.0.5) THEN 
        MAP(I,J)=LETW
        ELSE
        MAP(I,J)=LETL
      ENDIF

1000  CONTINUE

C  MAKE MAP FILE

cjc  JC modifications of 1/9/97.  Add the following comment lines to
cjc  the output file.  Also write necessary information to the reference
cjc  file MAKEGEO.REF for later use

      write (9,*) xsw,ysw,iz0   ! UTM-coord and zone for the origin
                                ! (SW corner of the domain)
      write (9,*) sngl(slat1),sngl(slon2)
                                ! Lat. and long. for the above origin
c
c *** Write shoreline geometry data, to be included in the control file
c
      delx=(xne-xsw)/float(ixgrid)
      dely=(yne-ysw)/float(iygrid)
      write (  3,*) 'INPUT GROUP 16a -- Shoreline geometry (mandatory)'
      write (  3,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (  3,*) dls,'X0       = ',0.      ,dle
      write (  3,*) b10,'X0       -- X-coord of the NORTHWEST corner of 
     &the mapped area,'
      write (  3,*) b10,'            km'
      write (  3,*) b10,'            The NORTHWEST corner of the domain 
     &corresponds to:'
      write (  3,*) b10,'            N. latitude = ',sngl(slat2)
      write (  3,*) b10,'            W. latitude = ',sngl(slon2)
      write (  3,*) b10,'            or,'
      write (  3,*) b10,'            UTM-E (km)  = ',xsw
      write (  3,*) b10,'            UTM-N (km)  = ',yne
      write (  3,*) b10,'            UTM zone    = ',iz0
      write (  3,*) b10,'No default'
      write (  3,*) dls,'Y0       = ',yne-ysw ,dle
      write (  3,*) b10,'Y0       -- Y-coord of the NORTHWEST corner of 
     &the mapped area,'
      write (  3,*) b10,'            km'
      write (  3,*) b10,'No default'
      write (  3,*) dls,'NX       = ',ixgrid  ,dle
      write (  3,*) b10,'NX       -- Number of grid rectangles along the
     & x-axis'
      write (  3,*) b10,'            (1 to MAXMAP specified in PARAMS.CM
     &N)'
      write (  3,*) b10,'No default'
      write (  3,*) dls,'NY       = ',iygrid  ,dle
      write (  3,*) b10,'NY       -- Number of grid rectangles along the
     & y-axis'
      write (  3,*) b10,'            (1 to MAXMAP specified in PARAMS.CM
     &N)'
      write (  3,*) b10,'No default'
      write (  3,*) dls,'DELX     = ',delx    ,dle
      write (  3,*) b10,'DELX     -- The length of each grid delta-x, km
     & (>0)'
      write (  3,*) b10,'No default'
      write (  3,*) dls,'DELY     = ',dely    ,dle
      write (  3,*) b10,'DELY     -- The length of each grid delta-y, km
     & (>0)'
      write (  3,*) b10,'No default'
      write (  3,*) dls,'WMIN     = ',1.      ,dle
      write (  3,*) b10,'WMIN     -- The minimum along wind width for a 
     &land or water body'
      write (  3,*) b10,'            to be considered significant, km (>
     &0)'
      write (  3,*) b10,'            WMIN allows OCD to neglect insignif
     &icant water bodies'
      write (  3,*) b10,'            or land masses (e.g., a very narrow
     & barrier beach or'
      write (  3,*) b10,'            a very narrow lagoon).  As a guidan
     &ce value, a maximum'
      write (  3,*) b10,'            of ten times the estimated plume he
     &ight is suggested'
      write (  3,*) b10,'            for WMIN.'
      write (  3,*) b10,'No default'
      write (  3,*) dls,'AVGDIST  = ',1.      ,dle
      write (  3,*) b10,'AVGDIST  -- The average distance from source to
     & shoreline,'
      write (  3,*) b10,'            km (>0)'
      write (  3,*) b10,'            AVGDIST is only used to determine t
     &he range of acceptable'
      write (  3,*) b10,'            values for DELX and DELY, and is no
     &t used in actual'
      write (  3,*) b10,'            calculations.'
      write (  3,*) b10,'No default'
      write (  3,*) dls,ender,dle
      write (  3,*)

      write (ifmt2,121) ixgrid
      write (ifmt3,131) ixgrid
121   format ('(',i3,'a1,'','')')
131   format ('(',i3,'a1,''!'')')
      write (  3,*) 'INPUT GROUP 16b -- Shoreline geometry (mandatory)'
      write (  3,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (  3,*) b10,'In the following, for each row (from north to s
     &outh) of grid'
      write (  3,*) b10,'rectangles to be mapped, a series of L''s or W'
     &'s (from west to east)'
      write (  3,*) b10,'is used to represent the distribution of (L)and
     & vs. (W)ater.'
      write (  3,*) b10,'Note that only the letters L and W are allowed.
     &'
      write (  3,*) b10,'There should be NY rows of data, and each row s
     &hould have NX letters'
      write (  3,*) b10,'of either L or W.'
      write (  3,*)
      write (  3,*) b10,'Although it is not necessary for receptors or s
     &ources to be located'
      write (  3,*) b10,'on the grid, it is very important that a gridde
     &d land sector be'
      write (  3,*) b10,'included between all sources and overland recep
     &tors.  Otherwise,'
      write (  3,*) b10,'the model assumes the receptor is over water.'
      write (  3,*)
      write (  3,*) dls,'LWFLAG ='

cjc  JC modifications of 1/9/97.  Modify the following write statement.
cjc       DO 2000 J=1, IYGRID
cjc      
cjc       WRITE(3,1500) (MAP(I,J),I=1,IXGRID)
cjc1500   FORMAT(240A1)
cjc2000   CONTINUE
      do 2005 j=1, iygrid-1
      write(3,ifmt2) (map(i,j),i=1,ixgrid)
2005  continue
      write(3,ifmt3) (map(i,iygrid),i=1,ixgrid)
      write (3,*) dls,ender,dle
      write (3,*)

c
c *** Write Cartesian receptor data, to be included in the control file
c
      delx_c=(xne-xsw)/19.  ! Default: 20x20 Cartesian receptors
      dely_c=(yne-ysw)/19.  ! Default: 20x20 Cartesian receptors
      write (  8,*) 'INPUT GROUP 11 -- Cartesian coordinate receptors (c
     &onditional)'
      write (  8,*) '(Only necessary if IOPT(8)=2, 3, 5, or 6)'
      write (  8,*) 'Any combination of receptors located at sea or on l
     &and is allowed.'
      write (  8,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (  8,*) dls,'X0_C     = ',0.      ,dle
      write (  8,*) b10,'X0_C     -- X-coord of the NORTHWEST corner of 
     &the Cartesian'
      write (  8,*) b10,'            receptor network, user units'
      write (  8,*) b10,'            The NORTHWEST corner of the domain 
     &corresponds to:'
      write (  8,*) b10,'            N. latitude = ',sngl(slat2)
      write (  8,*) b10,'            W. latitude = ',sngl(slon2)
      write (  8,*) b10,'            or,'
      write (  8,*) b10,'            UTM-E (km)  = ',xsw
      write (  8,*) b10,'            UTM-N (km)  = ',yne
      write (  8,*) b10,'            UTM zone    = ',iz0
      write (  8,*) b10,'No default'
      write (  8,*) dls,'Y0_C     = ',yne-ysw ,dle
      write (  8,*) b10,'Y0_C     -- Y-coord of the NORTHWEST corner of 
     &the Cartesian'
      write (  8,*) b10,'            receptor network, user units'
      write (  8,*) b10,'No default'
      write (  8,*) dls,'NX_C     = ',20      ,dle
      write (  8,*) b10,'NX_C     -- Number of receptors along the x-axi
     &s'
      write (  8,*) b10,'            (1 to MAXCAR specified in PARAMS.CM
     &N)'
      write (  8,*) b10,'Default = 20'
      write (  8,*) dls,'NY_C     = ',20      ,dle
      write (  8,*) b10,'NY_C     -- Number of receptors along the y-axi
     &s'
      write (  8,*) b10,'            (1 to MAXCAR specified in PARAMS.CM
     &N)'
      write (  8,*) b10,'            The total number of Cartesian recep
     &tors is NX_C*NY_C.'
      write (  8,*) b10,'            The sum of polar, Cartesian, and di
     &screte receptors'
      write (  8,*) b10,'            cannot > MAXREC specified in PARAMS
     &.CMN.'
      write (  8,*) b10,'Default = 20'
      write (  8,*) dls,'DELX_C   = ',delx_c  ,dle
      write (  8,*) b10,'DELX_C   -- The distance between successive Car
     &tesian receptors'
      write (  8,*) b10,'            along the x-axis, user units (>0)'
      write (  8,*) b10,'No default'
      write (  8,*) dls,'DELY_C   = ',dely_c  ,dle
      write (  8,*) b10,'DELY_C   -- The distance between successive Car
     &tesian receptors'
      write (  8,*) b10,'            along the y-axis, user units (>0)'
      write (  8,*) b10,'No default'
      write (  8,*) dls,ender,dle
      write (  8,*)


cjc  End of JC modifications.
   
       RETURN
       END

C**************************************************************
      SUBROUTINE TGTMATRX(NAREA)

      CHARACTER*50 FILE
      LOGICAL*1 A,BUG                                                   
      COMMON /DEBUG/ BUG                                                
      COMMON /DIVERS/ KOBJ,MOBJ,NOBJ,MFILE,NFILES,JALL,LOBJ
      COMMON /MAP/  A(250000)                               
      COMMON /PARMS/ A1,B1,C1,A2,B2,C2,IX,IY,LEVMIN                     
      REAL*8 SLON1,SLON2,SLAT1,SLAT2
      COMMON /LL/ SLON1,SLON2,SLAT1,SLAT2
      COMMON /TEMP/ TEMP(250000)

C  CREATE SCRATCH FILE FOR LMAP
      OPEN(40,STATUS='SCRATCH',FORM='UNFORMATTED')
C
      IF(NAREA.EQ.1) OPEN (30,FILE='WEST.BIN',STATUS='OLD',
     X  FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8)
      IF(NAREA.EQ.2) OPEN (30,FILE='GOM.BIN',STATUS='OLD',
     X  FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8)
      IF(NAREA.EQ.3) OPEN (30,FILE='EAST.BIN',STATUS='OLD',
     X  FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8)
      IF(NAREA.EQ.4) OPEN (30,FILE='ALASKA.BIN',STATUS='OLD',
     X  FORM='UNFORMATTED',ACCESS='DIRECT',RECL=8)

      IF(NAREA.GE.1 .AND. NAREA.LE.4) GO TO 12
      PRINT *
      PRINT *
      PRINT *, 'SOMETHING IS WRONG WITH YOUR LAT/LONG ENTRIES'
      CLOSE(40,STATUS='DELETE')
      STOP
12    CONTINUE
C  OPEN  TEMP OUTPUT FILE
       OPEN(50,STATUS='SCRATCH',FORM='FORMATTED')
C
      DO 1235 I=1,250000    
cjc  JC modifications of 1/9/97.  Disable the following print statement
cjc      IF ((I/10000)*10000 .EQ. I) PRINT *, 'ZERO',I
cjc  End of JC modifications
      TEMP(I)=0.0              
1235  CONTINUE
C               
C
      LEVMIN=11
      NOBJ=1
      KOBJ=31
      NFILES=1
      KNOW=1
      LOBJ=2**(31-KOBJ)                                                 
C                  (RE)INITIALIZE ALL OBJECT DEPENDENT PARAMETERS.      
      JALL = 0                                                          
      BUG = (KNOW.NE.0)                                                 
         DO 10011 IKH=1,250000
10011    A(IKH)=.FALSE.
C
      REWIND 40                                                         
      DO 800 MFILE=1,NFILES                                             
C                  FOR EACH FILE, CALL MAPOBJ                           
      CALL MAPOBJ                                                       
  800 CONTINUE                                                          
C                  JALL IS THE NUMBER OF SHADOW POINTS.                 
C                  FILLER FILLS THE OBJECT'S INTERIOR.                  
      CALL FILER2                                                       
C                  IF KNOW > 9 , PRINT OUT THE RESULTS OF FILER2.       
C                  GO GET THE NEXT OBJECT.                              
  900 CONTINUE                                                          
C                  NOUT OUTPUTS THE RESULTS OF FILLER IN FILE 50.       
      CALL NOUT                                                         
  999 CONTINUE                                                          
      RETURN                                                            
 1000 FORMAT( 20I4)                                                     
 3128 FORMAT( T12,'JALL,THE NUMBER OF SHADE POINTS, IS',I4)             
 6000 FORMAT( 12X,'WE ENTERED FILE #',I3)                               
 6025 FORMAT( T12,2A4,6(2X,D12.4),2I4)                                  
 6050 FORMAT( T12,' ONLY LEVELS ABOVE',I4,' WILL BE BOUNDARIES.')       
 6075 FORMAT( T12,'NOBJ=',I3)                                           
 6080 FORMAT( T12,'KOBJ,NFILES, AND KNOW ARE',3I5,'.  IF KNOW IS NOT',  
     *  ' ZERO, WE WILL HAVE EXTENSIVE OUTPUT.')                        
 6200 FORMAT( 10X,'NO FILES (NFILES<1) FOR OBJECT #',I3)                
 6100 FORMAT( 10X,'NO INPUT OBJECTS!!  NOBJ<1')                         
 6300 FORMAT( 12X,'THE VALUE OF KOBJ (',I4,') IS UNACCEPTABLE.  MOVE ', 
     *  'ONTO THE NEXT OBJECT')                                         
 6500 FORMAT( 12X,'WE START PROCESSING OBJECT #',I3)                    
      END                                                               
C*********************************************************************
      SUBROUTINE FILER2                                                 
C                  FILLER READS THE SHADOW POINTS AND,IF THEY DO NOT    
C                  BELONG TO THE BOUNDARY, CALLS FILLIN                 
      LOGICAL*1 A                                                       
      COMMON /DIVERS/ KOBJ,MOBJ,NOBJ,MFILE,NFILES,JALL,LOBJ
      COMMON /MAP/  A(250000)                               
      COMMON /PARMS/ A1,B1,C1,A2,B2,C2,IX,IY                            
      COMMON /TEMP/TEMP(250000)
C     DIMENSION X(5000),Y(5000)                                             
      DIMENSION X(3000),Y(3000)                                             
      REWIND 40                                                         
  100 IF (JALL.LE.0) GO TO 999                                          
cjc  JC modifications of 1/9/97.  Disable the following print statement
cjc          PRINT *, 'JALL=',JALL
cjc  End of JC modifications.
      READ(40,ERR=980,END=980) M,LEVEL,(X(IL),IL=1,100),
     X  (Y(IL),IL=1,100)
C     DO 101 IK1=101,5000,100
      DO 101 IK1=101,3000,100
      IK2=IK1+99
101   READ(40,ERR=980,END=980) (X(IL),IL=IK1,IK2),(Y(IL),IL=IK1,IK2)
C
      JALL = JALL-M                                                     
      DO 200 K=1,M                                                      
      I = X(K)+1                                                        
      IF (I.LT.1.OR.I.GT.IX) GO TO 200                                  
      J = Y(K)+1                                                        
      IF (J.LT.1.OR.J.GT.IY) GO TO 200                                  
      IPOINT=IX*(J-1)+I                                                 
      IF (A(IPOINT)) GO TO 200                                          
      CALL FILIN2(I,J)                                                  
  200 CONTINUE                                                          
      GO TO 100                                                         
  980 CONTINUE                                                          
  999 CONTINUE                                                          
      JALL = 0                                                          
      RETURN                                                            
      END                                                               
C****************************************************************
      SUBROUTINE FILIN2(I,J)                                            
C                  FILIN2 WORKS ON A 'CIRCULAR' QUEUE (IX,JY). POINTER  
C                  IN POINTS AT THE QUEUE'S FIRST ELEMENT AND POINTER   
C                  IOUT POINTS AT THE FIRST FREE ELEMENT.  AT EVERY STEP
C              A)  WE REMOVE THE FIRST ELEMENT.                         
C              BE  WE ADD TO THE QUEUE ALL ITS NEIGHBORS WHICH HAVE     
C                  VALUE .FALSE. (ARE NOT BOUNDARY POINTS) AND WE SET   
C                  THEIR VALUE .TRUE. .                                 
C                  THE ROUTINE IS EXITED WHEN IN=IOUT.                  
      LOGICAL*1 A                                                       
      INTEGER*2 IX(2500),JY(2500)                                       
      COMMON /DIVERS/ KOBJ,MOBJ,NOBJ,MFILE,NFILES,JALL,LOBJ             
      COMMON /MAP/  A(250000)                               
      COMMON /PARMS/ R(6),MX,NY                                         
      COMMON /TEMP/TEMP(250000)
      NMAX = 2500                                                       
C                  INITIALIZE THE QUEUE                                 
      IX(1) = I                                                         
      JY(1) = J                                                         
      JPOINT=MX*(J-1)+I                                                 
      IPOINT=JPOINT                                                     
      A(IPOINT)=.TRUE.                                                  
        MCHIP=TEMP(IPOINT)
        MCHIP=IOR(MCHIP,LOBJ)
        TEMP(IPOINT)=MCHIP
      IN = 1                                                            
      IOUT = 2                                                          
C                  START PROCESSING.                                    
  100 IF (IN.EQ.IOUT) GO TO 999                                         
      K = IX(IN)                                                        
      L = JY(IN)                                                        
      JPOINT=MX*(L-1)+K                                                 
      IN = IN+1                                                         
      IF (IN.GT.NMAX) IN = 1                                            
      IF (K.EQ.1) GO TO 200                                             
      IPOINT=JPOINT-1                                                   
      IF (A(IPOINT)) GO TO 200                                          
      A(IPOINT)=.TRUE.                                                  
        MCHIP=TEMP(IPOINT)
        MCHIP=IOR(MCHIP,LOBJ)
        TEMP(IPOINT)=MCHIP
      IX(IOUT) = K-1                                                    
      JY(IOUT) = L                                                      
      IOUT = IOUT+1                                                     
      IF (IOUT.GT.NMAX) IOUT=1                                          
      IF (IOUT.EQ.IN) GO TO 800                                         
  200 IF (L.EQ.1) GO TO 300                                             
      IPOINT=JPOINT-MX                                                  
      IF (A(IPOINT)) GO TO 300                                          
      A(IPOINT)=.TRUE.                                                  
        MCHIP=TEMP(IPOINT)
        MCHIP=IOR(MCHIP,LOBJ)
        TEMP(IPOINT)=MCHIP
      IX(IOUT) = K                                                      
      JY(IOUT) = L-1                                                    
      IOUT = IOUT+1                                                     
      IF (IOUT.GT.NMAX) IOUT=1                                          
      IF (IOUT.EQ.IN) GO TO 800                                         
  300 IF (K.EQ.MX) GO TO 400                                            
      IPOINT=JPOINT+1                                                   
      IF (A(IPOINT)) GO TO 400                                          
      A(IPOINT)=.TRUE.                                                  
        MCHIP=TEMP(IPOINT)
        MCHIP=IOR(MCHIP,LOBJ)
        TEMP(IPOINT)=MCHIP
      IX(IOUT) = K+1                                                    
      JY(IOUT) = L                                                      
      IOUT = IOUT+1                                                     
      IF (IOUT.GT.NMAX) IOUT = 1                                        
      IF (IOUT.EQ.IN) GO TO 800                                         
  400 IF (L.GE.NY) GO TO 100                                            
      IPOINT=JPOINT+MX                                                  
      IF (A(IPOINT)) GO TO 100                                          
      A(IPOINT)=.TRUE.                                                  
        MCHIP=TEMP(IPOINT)
        MCHIP=IOR(MCHIP,LOBJ)
        TEMP(IPOINT)=MCHIP
      IX(IOUT) = K                                                      
      JY(IOUT) = L+1                                                    
      IOUT = IOUT+1                                                     
      IF (IOUT.GT.NMAX) IOUT = 1                                        
      IF (IOUT.NE.IN) GO TO 100                                         
C                  ENTERED IN THE UNLIKELY EVENT THAT OVERFLOW MENACES. 
800   CONTINUE
      IN = IN+1                                                         
      IF (IN.GT.NMAX) IN = 1                                            
      GO TO 100                                                         
  999 CONTINUE                                                          
      RETURN                                                            
 6000 FORMAT( T12,'DANGER OF OVERFLOWING. IN AND IOUT ARE',2I5 ,        
     *  '. AND THE POINT(',I4,',',I4,') WAS DITCHED')                   
 6100 FORMAT( T12,'IN EQUALS NMAX AND IOUT IS',I5)                      
      END                                                               
C***************************************************************
      SUBROUTINE MAPOBJ                                                 
C                  TRANSFORMS THE INPUT POINTS TO GRID COORDINATES AND  
C                  MARKS THE BOUNDARY OF THE OBJECT.                    
      LOGICAL*1 BUG                                                     
      COMMON /DEBUG/ BUG                                                
      COMMON /DIVERS/ KOBJ,MOBJ,NOBJ,MFILE,NFILES,JALL,LOBJ
      COMMON /PARMS/ A1,B1,C1,A2,B2,C2,IX,IY,LEVMIN                     
      COMMON /CORNER/CORNX1,CORNX2,CORNY1,CORNY2
      COMMON /TEMP/TEMP(250000)
C     DIMENSION X(5000),Y(5000)                                            
      DIMENSION X(3000),Y(3000)                                            
C                  THE FIRST RECORD CONTAINS NO POINTS.                 
cjc  JC modifications of 1/9/97.  Disable the following print statement
cjc           PRINT *, CORNX1,CORNX2,CORNY1,CORNY2
cjc  End of JC modifications
1223  FORMAT(1X)
      IREC=0
      NREAD=0
   50 CONTINUE                                                          
      IREC=IREC+1
      READ(30,REC=IREC) N,LEVEL
C          IF N=0, THIS IS THE LAST RECORD 
      IF (N.EQ.0) GO TO 999                                             
      NAB=IABS(N)
      NREAD=NREAD+NAB
cjc  JC modifications of 1/9/97.  Disable the following print statement.
cjc      PRINT *, NREAD,' READ'
cjc  End of JC modifications.
      MFIND=0
      DO 1233 ICH=1,NAB
      IREC=IREC+1
      READ(30,REC=IREC) X(ICH), Y(ICH)

C  SEE IF ANY OF THE POINTS ARE IN AREA OF INTEREST
      IF(X(ICH).GE.CORNX1 .AND. X(ICH).LE.CORNX2 .AND. Y(ICH).GE.CORNY1
     X  .AND. Y(ICH).LE.CORNY2) MFIND=1
1233  CONTINUE                                    
      IF(MFIND.EQ.0) GO TO 50
cjc  JC modifications of 1/9/97.  Disable the following print statement
cjc             PRINT *, '                       FOUND',NAB
cjc  End of JC modifications.
      M = IABS(N)                                                       
C                  GET THE GRID COORDINATES.                            
      CALL CXYXY3(X,Y,X,Y,A1,B1,C1,A2,B2,C2,M)                          
      IF (LEVEL.GE.LEVMIN) GO TO 80                                     
C                  IF SHADOW (LEVEL<LEVMIN) RECORD THE NUMBER OF POINTS 
C                  AND GO GET THE NEXT RECORD.                          
      JALL = JALL+M                                                     
      WRITE(40) M,LEVEL,(X(IL),IL=1,100),(Y(IL),IL=1,100)
      DO 101 IK1=101,3000,100
      IK2=IK1+99
101   WRITE(40) (X(IL),IL=IK1,IK2),(Y(IL),IL=IK1,IK2)
C
      GO TO 50                                                          
   80 CONTINUE                                                          
      J = 1                                                             
      IF (N.GT.0) GO TO 100                                             
      XO = X(1)                                                         
      YO = Y(1)                                                         
      IF (M.EQ.1) GO TO 50                                              
      J = 2                                                             
  100 CONTINUE                                                          
C                  ELSE GO AND MARK ALL BOUNDARY CELLS.                 
      DO 200 I=J,M                                                      
      CALL NTRACK(XO,YO,X(I),Y(I))                                      
      XO = X(I)                                                         
      YO = Y(I)                                                         
  200 CONTINUE                                                          
      GO TO 50                                                          
  999 RETURN                                                            
 6123 FORMAT( T12,'FROM MAPOBJ YOU GET N,LEVEL,AND GRID COORDINATES',   
     *  2I5, /, 12(2X,F8.2))                                            
      END                                                               
C************************************************************
      SUBROUTINE NOUT                                                   
      LOGICAL*1 A                                                       
      COMMON /MAP/ A(250000)                               
      COMMON /PARMS/ A1,B1,C1,A2,B2,C2,MX,NY                            
        DIMENSION LCH(1000)
      COMMON /TEMP/TEMP(250000)
      CHARACTER*240 T
      CHARACTER*1 TC(240), Y(240)
      EQUIVALENCE (T,TC)
C     
      K1=1                                                              
      K2=MX                                                             
        IST=0
      NUML=(MX+19)/20
C
      DO 500 J=1,NY                                                     
cjc  JC modifications of 1/9/97.  Disable the following print statement
cjc       IF ((J/20)*20 .EQ.J) PRINT *, 'WRITE',J,' OF',NY
cjc  End of JC modifications.
         KKK=0
         DO 300 K=K1,K2
         IST=IST+1
         KKK=KKK+1
300      LCH(KKK)=TEMP(IST)
C
      NS=-19
      DO 1000 IQQ=1,NUML
      NS=NS+20
      NE=NS+19
      IF(NE.GT.MX) NE=MX
      WRITE(T,301) (LCH(LLL),LLL=NS,NE)
301   FORMAT(20I12)
      NSP=0
      LN=0
      DO 400 JQQ=1,240
      IF(TC(JQQ).EQ.' ' .AND. NSP.EQ.0) GO TO 400
      IF(TC(JQQ).EQ.' ' .AND. NSP.EQ.1) GO TO 320
310   LN=LN+1
      Y(LN)=TC(JQQ)
      NSP=1
      GO TO 400
320   LN=LN+1
      Y(LN)=' '
      NSP=0
400   CONTINUE 
C
C   IF ANY BLANKS ON END OF LINE - GET RIDO OF THEM
401   IF(LN.LT.1) GO TO 1000
      IF(Y(LN).NE.' ') GO TO 405
      LN=LN-1
      GO TO 401
405   WRITE(50,1233) (Y(LLL),LLL=1,LN)
1233  FORMAT(240A1)
1000  CONTINUE
C
      K1=K2+1                                                           
      K2=K2+MX                                                          
  500 CONTINUE                                                          
      RETURN                                                            
      END                                                               
C***************************************************************
      SUBROUTINE NTRACK(XL,YL,XI,YI)                                    
C     SUBROUTINE TRACK -- VERSION 1.0 -- 14 APR 77.                     
C     J. R. SLACK, U. S. GEOLOGICAL SURVEY.                             
C     MARKS EACH CELL ON THE LINE BETWEEN TWO POINTS.                   
      LOGICAL*1 T                                               
      LOGICAL*1 A                                                       
      COMMON /DIVERS/ KOBJ,MOBJ,NOBJ,MFILE,NFILES,JALL,LOBJ             
      COMMON /MAP/ A(250000)                               
      COMMON /PARMS/ A1,B1,C1,A2,B2,C2,MX,NY                            
      COMMON /TEMP/ TEMP(250000)
      DATA T/.TRUE./
      LX=XL                                                             
      LY=YL                                                             
      IX=XI                                                             
      IY=YI                                                             
C     TRACK THRU EACH CELL BETWEEN THE OLD POINT                        
C        AND THE NEW POINT.                                             
      IF (IX.EQ.LX) GO TO 30                                            
C     NON-VERTICAL PATH.                                                
      SLOPE=(YI-YL)/(XI-XL)                                             
      IXDO=IABS(IX-LX)+1                                                
      IXADD=ISGN(IX-LX)                                                 
      XADD=IXADD                                                        
      IYADD=ISGN(IY-LY)                                                 
      KX=LX-IXADD                                                       
      XN=LX                                                             
      IF (XADD.LT.0.0) XN=XN+1.0                                        
      DO 20 KK=1,IXDO                                                   
      XN=XN+XADD                                                        
      KX=KX+IXADD                                                       
      IF (KK.EQ.IXDO) XN=XI                                             
      KY=(XN-XL)*SLOPE+YL                                               
      IYDO=IABS(KY-LY)+1                                                
      LY=LY-IYADD                                                       
      DO 10 LL=1,IYDO                                                   
      LY=LY+IYADD                                                       
C     RECORD THE TRACK THRU THIS CELL.                                  
      IF (KX.LT.0.OR.KX.GE.MX.OR.LY.LT.0.OR.LY.GE.NY) GO TO 10          
      IPOINT=LY*MX+KX+1                                                 
      A(IPOINT)=.TRUE.                                                  
        MCHIP=TEMP(IPOINT)
        MCHIP=IOR(MCHIP,LOBJ)
        TEMP(IPOINT)=MCHIP
   10 CONTINUE                                                          
      LY=KY                                                             
   20 CONTINUE                                                          
      RETURN                                                            
C     VERTICAL PATH.                                                    
   30 CONTINUE                                                          
      IYADD=ISGN(IY-LY)                                                 
      IYDO=IABS(IY-LY)+1                                                
      LY=LY-IYADD                                                       
      DO 40 LL=1,IYDO                                                   
      LY=LY+IYADD                                                       
C     RECORD THE TRACK THRU THIS CELL.                                  
      IF (IX.LT.0.OR.IX.GE.MX.OR.LY.LT.0.OR.LY.GE.NY) GO TO 40          
      IPOINT=MX*LY+IX+1                                                 
      A(IPOINT)=.TRUE.                                                  
        MCHIP=TEMP(IPOINT)
        MCHIP=IOR(MCHIP,LOBJ)
        TEMP(IPOINT)=MCHIP
   40 CONTINUE                                                          
      RETURN                                                            
      END                                                               
C*****************************************************************
      FUNCTION ISGN(I)
      ISGN=0
      IF(I.LT.0) ISGN= -1
      IF(I.GT.0) ISGN= 1
      RETURN
      END
C*****************************************************************
      SUBROUTINE CXYXY3(XD,YD,XP,YP,A1,B1,C1,A2,B2,C2,M)
      REAL XD(1), YD(1), XP(1), YP(1)
C
      DO 10 I=1, M
      XP(I)=A1+B1*XD(I)+C1*YD(I)
      YP(I)=A2+B2*XD(I)+C2*YD(I)
10    CONTINUE
      RETURN
      END

C*********************************************************************
      SUBROUTINE GENERAL(NAREA)
C  ROUTINE TO CALG GENERAL FILE INFO AND COEF VALUES
      COMMON /PARMS/ A1,B1,C1,A2,B2,C2,IX,IY,LEVMIN                     
      COMMON /LL/ SLON1,SLON2,SLAT1,SLAT2
      COMMON /CORNER/CORNX1,CORNX2,CORNY1,CORNY2
      CHARACTER*1 BELL
  
      REAL*8 XM(3),YM(3),XG(3),YG(3),SLAT1,SLAT2,SLON1,SLON2
      REAL*8 SLATMI,SLATMA,SLONMI,SLONMA,DUM1,DUM2,AAA,BBB
      CHARACTER*60 FILE
      CHARACTER*1 QQ, Q1,Q2,Q3,Q4
cjc  JC modifications of 1/9/97.  Add the following statements to pass
cjc  the UTM coordinates (in KM) for the southwest and northeast corners
cjc  of the modeling domain, and the reference UTM zone for the southwest
cjc  corner.
      real xsw,ysw,xne,yne
      integer iz0
      common /utm/xsw,ysw,xne,yne,iz0
cjc  End of JC modifications.

      DATA Q1/'Y'/, Q2/'y'/, Q3/'N'/, Q4/'n'/
      DATA BELL /Z'07'/
C
C     READ THE LIMITS OF LATITUDE AND LONGITUDE.
cjc  JC modifications of 1/9/97.  Disable the following print statement.
cjc      WRITE(*,1)   
cjc1     FORMAT(25(/))
cjc  End of JC modifications

1234  PRINT *, 'The latitudes & longitudes must be entered as unsigned'
      PRINT *, 'decimal values.  For example, a latitude of 41 degrees'
      PRINT *, '35 minutes and 15 seconds would be entered as 41.5875.'
      PRINT *
      PRINT *, 'A longitude of 95 degress would be enterd as 95, while'
      PRINT *, 'one of 95 degrees and 30 minutes would be 95.5.'
      PRINT *
      PRINT *, 'The minimum and maximum latitudes/longitudes are:'
      PRINT *, 'Area              Min Lat   Max Lat   Min Lon   Max Lon'
      PRINT *, 'Pacific Coast          31        49       116       128'
      PRINT *, 'Gulf of Mexico Coast   23        32        80        98'
      PRINT *, 'Atlantic Coast         23        47        65        83'
      PRINT *, 'Alaska Coast           50        72       128       188'

cjc  JC modifications of 1/9/97.  Read the following information from
cjc  the input file, MAKEGEO.DAT.
cjc      PRINT * 
cjc      PRINT *, 'What is one latitude limit: '
cjc      READ(5,*) SLAT1
cjc      PRINT *, 'What is the other latitude limit: '
cjc      READ(5,*) SLAT2
cjc      PRINT *
cjc      PRINT *, '(NOTE: LOGITUDE GOES FROM 0 TO 360  IN WEST DIRECTION)'
cjc      PRINT *
cjc      PRINT *, 'What is one longitude limit: '
cjc      READ(5,*) SLON1
cjc      PRINT *, 'What is the other longitude limit: '
cjc      READ(5,*) SLON2
      read (1,*) slat1
      read (1,*) slat2
      read (1,*) slon1
      read (1,*) slon2
cjc  End of JC modifications.
   
C ORDER THE MIN AND MAX - AND CHECK
cjc  JC modifications of 1/9/97.  Modify the following check.
cjc      IF(SLAT1.EQ.SLAT2 .OR. SLON1.EQ.SLON2) THEN
cjc        PRINT *, BELL
cjc        WRITE(*,1)
cjc        PRINT *, 'There was an error in your lat/lons -- TRY AGAIN'
cjc        PRINT *
cjc        GO TO 1234
cjc      ENDIF
      if(slat1.eq.slat2 .or. slon1.eq.slon2) then
        print *,'The two latitudes and the two longitudes should not be 
     &the same.'
      end if
cjc  End of JC modifications.

      IF(SLAT1.GT.SLAT2) THEN
        A=SLAT1
        SLAT1=SLAT2
        SLAT2=A
      ENDIF
      IF(SLON1.GT.SLON2) THEN
        A=SLON1
        SLON1=SLON2
        SLON2=A
      END IF

      NAREA=0

C  PACIFIC COAST ?

      IF(SLAT1.GE.31. .AND. SLAT1.LE.49. .AND. SLON1.GE.116 .AND.
     X  SLON1.LE.128 .AND. SLAT2.GE.31. .AND. SLAT2.LE.49. .AND.
     X  SLON2.GE.116 .AND. SLON2.LE.128) THEN
        NAREA=1
        GO TO 1235
      ENDIF

C  GOM ?

      IF(SLAT1.GE.23. .AND. SLAT1.LE.32. .AND. SLON1.GE.80 .AND.
     X  SLON1.LE.98 .AND. SLAT2.GE.23. .AND. SLAT2.LE.32. .AND.
     X  SLON2.GE.80 .AND. SLON2.LE.98) THEN
        NAREA=2
        GO TO 1235
      ENDIF 

C  ATLANTIC ?

      IF(SLAT1.GE.23. .AND. SLAT1.LE.47. .AND. SLON1.GE.65 .AND.
     X  SLON1.LE.83 .AND. SLAT2.GE.23. .AND. SLAT2.LE.47. .AND.
     X  SLON2.GE.65 .AND. SLON2.LE.83) THEN
        NAREA=3
        GO TO 1235
      ENDIF

C NOW CHK TO SEE IF IN AK AREA

C  ALASKA?

      IF(SLAT1.GE.50. .AND. SLAT1.LE.72. .AND. SLON1.GE.128 .AND.
     X  SLON1.LE.188 .AND. SLAT2.GE.50. .AND. SLAT2.LE.72. .AND.
     X  SLON2.GE.128 .AND. SLON2.LE.188) THEN
        NAREA=4
        GO TO 1235
      ENDIF

cjc  JC modifications of 1/9/97.  Modify the following check.
cjc      PRINT *, BELL
cjc      WRITE(*,1)
cjc      PRINT *, 'There was an error in your lat/lons -- TRY AGAIN'
cjc      PRINT *
cjc      GO TO 1234
      print *,'The latitudes and/or longitudes specified are out of the 
     &ranges for'
      print *,'Pacific coast, Gulf of Mexico coast, Atlantic Coast, and 
     &Alaska Coast.'
cjc  JC modifications of 1/9/97.  Disable the following screen output.
cjc1235  PRINT *
cjc      PRINT *, 'You entered this information:'
cjc      PRINT *  
cjc      WRITE(6,1236) SLAT1,SLAT2,SLON1,SLON2
cjc1236  FORMAT(' Latitudes: ',2F10.5,/' Longitudes: ',2F10.5)
cjc      PRINT *
cjc      PRINT *, 'Is this correct (Y/N): '
cjc      READ(5,8006) QQ
cjc8006  FORMAT(A1)
cjc      IF(QQ.EQ.Q3 .OR. QQ.EQ.Q4) GO TO 1234
cjc      IF(QQ.NE.Q1 .AND. QQ.NE.Q2) GO TO 1235
1235  continue
cjc  End of JC modifications.

cjc  JC modifications of 1/9/97.  Calculate UTM coordinates for the
cjc  southwest corner of the modeling domain.  Note that the MAKEGEO
cjc  program assumes longitudes to be positive, whereas the convention
cjc  and the LL2UTM program require that the western longitudes to be
cjc  negative.
      call ll2utm(sngl(slat1),sngl(-slon2),0,xsw,ysw,iz0)
      call ll2utm(sngl(slat2),sngl(-slon1),iz0,xne,yne,iz)
cjc  End of JC modifications.
C 
C  CALC CORNERS OF AREA IN MERCATOR
      AAA=SLAT1*3600.
      BBB=SLON1*3600.
      CALL CLLME(AAA,BBB,DUM1,DUM2,IERRO)
      CORNX1=DUM1
      CORNY1=DUM2
      AAA=SLAT2*3600.
      BBB=SLON2*3600.
      CALL CLLME(AAA,BBB,DUM1,DUM2,IERRO)
      CORNX2=DUM1
      CORNY2=DUM2
C     FIND MAXIMUMS AND MINIMUMS, AND CONVERT TO SECONDS.
      SLATMA=3600.0*DMAX1(SLAT1,SLAT2)
      SLATMI=3600.0*DMIN1(SLAT1,SLAT2)
      SLONMA=3600.0*DMAX1(SLON1,SLON2)
      SLONMI=3600.0*DMIN1(SLON1,SLON2)
C     FIND MERCATOR COORDINATES OF THE ORIGIN (POINT1), THE END POINT
C        OF THE X-AXIS (POINT2), AND THE END POINT OF THE Y-AXIS
C        (POINT3). THESE ARE THE REFERENCE POINTS.
      CALL CLLME (SLATMI,SLONMA,XM(1),YM(1),IERR)
      CALL CLLME (SLATMI,SLONMI,XM(2),YM(2),IERR)
      CALL CLLME (SLATMA,SLONMA,XM(3),YM(3),IERR)
      
C
C     THE FOLLOWING VALUES ARE ALWAYS THE SAME.
      XG(1)=0.0
      YG(1)=0.0
      YG(2)=0.0
      XG(3)=0.0
      DX=ABS(XM(2)-XM(1))
      DY=ABS(YM(3)-YM(1))
      AA=SQRT(250000/(DX*DY))
  100 CONTINUE
      IX=AA*DX+1
      IY=AA*DY+1
      IF ((IX*IY).LE.250000)   GO TO 200
      IF (DX.GT.DY) AA=AA-1.0/DX
      IF (DY.GE.DX) AA=AA-1.0/DY
      GO TO 100
  200 CONTINUE
      XG(2)=AA*DX
      YG(3)=AA*DY
      SLATMI=SLATMI/3600.0
      SLATMA=SLATMA/3600.0
      SLONMI=SLONMI/3600.0
      SLONMA=SLONMA/3600.0
C
C     FIND THE CONVERSION COEFFICIENTS FROM MERCATOR TO GRID.
C     USES THE SAME FORMAT AS CXYXY1.
      A1=-XG(2)*XM(1)/(XM(2)-XM(1))
      B1=XG(2)/(XM(2)-XM(1))
      C1=0.0
      A2=-YG(3)*YM(1)/(YM(3)-YM(1))
      B2=0.0
      C2=YG(3)/(YM(3)-YM(1))
C
      RETURN
      END
C
C*****************************************************************************
      SUBROUTINE CLLME(SLAT,SLON,X,Y,IERROR)
      REAL*8 SECRAD, PION4,X,Y,SLAT,SLON
      REAL*8 A, EPSIL, SECMET
      REAL*8 EPSIL2, HEM, ESINPH, SECRA2
      DATA SECRAD/4.848137E-6/, PION4/0.7853982/
      DATA A/6378206.4/, EPSIL/8.227186E-2/, SECMET/30.922417/
      DATA SECRA2/2.424068E-6/, EPSIL2/4.113593E-2/
      IERROR=0
      X=SECMET*SLON
      HEM=0.0
      IF(SLAT.GT.0.0) HEM= 1.0
      IF(SLAT.LT.0.0) HEM= -1.0
      SLAT=ABS(SLAT)
      IF(SLAT.GE.324000.) IERROR=1
      ESINPH=EPSIL*DSIN(SLAT*SECRAD)
      Y=A*HEM*DLOG(DTAN(PION4+SLAT*SECRA2)*
     X ((1.0-ESINPH)/(1.0+ESINPH))**EPSIL2)
      RETURN  
      END
C**************************************************************************
c----------------------------------------------------------------------
      subroutine ll2utm(rlat,rlon,iz0,x,y,iz)
c----------------------------------------------------------------------
c
c --- TERREL   Version: 2.0       Level: 961004                  LL2UTM
c
c --- PURPOSE:  Converts latitude/longitude to UTM coordinates
c           *** Universal Transverse Mercator (UTM) grid system divides
c           *** the world into 60 north-south zones, covering a 6 degree
c           *** strip of longitude. Zone 1 begins between 180 and 174 degree
c           *** west longitude and progresses eastward to zone 60.
c               Works in both Northern & Southern Hemispheres
c               Reference --
c                 "Map Projections--A Working Manual", p61,
c                  U.S. Geological Survey Professional Paper 1395,
c                    Note: assumes the Clarke 1866 ellipsoid
c               Adapted from --
c                  EPS version 2.0; subroutine mapgtu
c
c --- INPUTS:
c               RLAT - Real        - N Latitude in decimal degrees
c                                    (use negative for southern hemisphere)
c               RLON - Real        - E Longitude in decimal degrees
c                                    (use negative for western hemisphere)
c                IZ0 - Integer     - UTM zone override
c
c --- OUTPUT:
c                  X - Real        - UTM easting in km
c                  Y - Real        - UTM northing in km
c                 IZ - Integer     - UTM zone
c
c --- LL2UTM called by:  RDUSGSHD, CORNERS, LOAD
c --- LL2UTM calls:      none
c----------------------------------------------------------------------

      real k0
      real N,M

      parameter (k0=0.9996)
      parameter (a=6378206.4)
      parameter (e2=0.00676866)
      parameter (ep2=0.0068148)
      parameter (false_e=500000.0)
      parameter (dtr=3.141592654/180.0)

      if (iz0 .eq. 0) then
c ---   Locate natural zone
          iz = int((180.0+rlon)/6.0) + 1
      else
c ---   Zone override
          iz = iz0
      endif

c --- Compute delta longitude in radians
      dl = dtr*(rlon - (6.0*iz-183.0))

c --- Convert phi (latitude) to radians
      p = dtr*rlat

      sinp = sin(p)
      N = a/sqrt(1.0-e2*sinp*sinp)
      tanp = tan(p)
      T = tanp*tanp
      cosp = cos(p)
      C = ep2*cosp*cosp
      A1 = dl*cosp
      M = 111132.0894*rlat - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p)
     &  - 0.02*sin(6.0*p)

      A2 = A1**2
      A3 = A2*A1
      A4 = A2**2
      A5 = A4*A1
      A6 = A4*A2
      T2 = T**2

c --- Compute UTM x and y (km)
      x = 0.001*(k0*N*(A1+(1.0-T+C)*A3/6.0
     &  + (5.0-18.0*T+T2+72.0*C-58.0*ep2)*A5/120.0)
     &  + false_e)
      y = (M+N*tanp * (A2/2.0 + (5.0-T+9.0*C+4.0*C*C)*A4/24.0
     &  + (61.0-58.0*T+T2+600.0*C-330.0*ep2)*A6/720.0))
      false_n = 0.
      if (rlat .lt. 0.) then
c --- in km, unlike false_e
        false_n = 10000.
      endif
      y = 0.001*k0*y + false_n

      return
      end
c----------------------------------------------------------------------
      subroutine utm2ll(x,y,iz,lsohem,rlat,rlon)
c----------------------------------------------------------------------
c
c --- TERREL   Version: 2.0       Level: 961004                  UTM2LL
c
c --- PURPOSE:  Converts UTM coordinates to latitude/longitude
c               Works in both Northern & Southern Hemispheres
c               Reference--
c                 "Map Projections--A Working Manual", p61,
c                  U.S. Geological Survey Professional Paper 1395,
c                    Note: assumes the Clarke 1866 ellipsoid
c               Adapted from --
c                  EPS version 2.0; subroutine maputg
c
c --- INPUTS:
c                  X - Real        - UTM easting in km
c                  Y - Real        - UTM northing in km
c                 IZ - Integer     - UTM zone (6 deg N-S strip, range=1,60)
c             LSOHEM - Logical     - TRUE = southern hemisphere
c                                    FALSE = northern hemisphere
c
c --- OUTPUT:
c               RLAT - Real        - N Latitude in decimal degrees
c               RLON - Real        - E Longitude in decimal degrees
c
c --- UTM2LL called by:  SPAN, RDUSGSHD, LOAD
c --- UTM2LL calls:      none
c----------------------------------------------------------------------

      real k0,M,N1,l
      logical lsohem

      parameter (k0=0.9996)
      parameter (a=6378206.4)
      parameter (e1=0.001697916)
      parameter (e11=3.0*e1/2.0 - 27.0*e1*e1*e1/32.0)
      parameter (e12=21.0*e1*e1/16.0 - 55.0*e1*e1*e1*e1/32.0)
      parameter (e13=151.0*e1*e1*e1/96.0)
      parameter (e14=1097.0*e1*e1*e1*e1/512.0)
      parameter (e2=0.00676866)
      parameter (e4=e2*e2)
      parameter (e6=e2*e4)
      parameter (ep2=0.0068148)
      parameter (false_e=500000.0)
      parameter (rtd=180.0/3.141592654)

c --- Parameter definitions
c      k0        -  scale on central meridian
c      a         -  Clarke 1866 equatorial radius
c      e2        -  squared Clarke 1866 eccentricity
c      ep2       -  (e2/(1.0-e2)
c      false_e   -  false easting
c      rtd       -  radians to degrees conversion

c --- Central meridian
      rlon0 = iz*6.0 - 183.0

c --- Correct for false easting, southern hemisphere and change to meters
      xm = 1000.0*x - false_e
      if(LSOHEM) then
        ym = 1000.0 * (y-10000.)
      else
        ym = 1000.0 * y
      endif

      M = ym/k0
      u = M/(a*(1.0-e2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0))
      p1 = u + e11*sin(2.0*u) + e12*sin(4.0*u) + e13*sin(6.0*u) +
     1         e14*sin(8.0*u)
      cosp1 = cos(p1)
      C1 = ep2*cosp1**2
      C2 = C1**2
      tanp1 = tan(p1)
      T1 = tanp1**2
      T2 = T1**2
      sinp1 = sin(p1)
      sin2p1 = sinp1**2
      N1 = a/sqrt(1.0-e2*sin2p1)
      R0 = 1.0-e2*sin2p1
      R1 = a*(1.0-e2)/sqrt(R0**3)

      D = xm/(N1*k0)
      D2=D**2
      D3=D*D2
      D4=D*D3
      D5=D*D4
      D6=D*D5

      p = p1 - (N1*tanp1/R1) * (D2/2.0
     1       - (5.0+3.0*T1+10.0*C1-4.0*C2-9.0*ep2)*D4/24.0
     2       + (61.0+90.0*T1+298.0*C1+45.0*T2-252*ep2-3.0*C2)*D6/720.0)
      rlat = rtd*p
      l = (D - (1.0+2.0*T1+C1)*D3/6.0
     1       + (5.0-2.0*C1+28*T1-3.0*C2+8.0*ep2+24.0*T2)*D5/120.0)/cosp1
      rlon = rtd*l + rlon0

      return
      end
