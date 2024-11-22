C***********************************************************************VIS00005
C                                                                       VIS00006
C                        VISCREEN (DATED 13190)                         VIS00010
C                                                                       VIS00011
C           *** SEE VISCREEN MODEL CHANGE BULLETIN MCB#2 ***            VIS00012
C                                                                       VIS00013
C     ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD    VIS00014
C                                                                       VIS00015
C                    http://www.epa.gov/ttn/scram/                      VIS00016
C                                                                       VIS00017
C***********************************************************************VIS00018
C
C-----  MODIFIED BY:  Peter Eckhoff
C                     U.S. EPA, OAQPS/AQAD
C                     Air Quality Modeling Group
C
C                     July 5, 2013
C
C       Availability: This model is available for download from:
C
C         http://www.epa.gov/ttn/scram/dispersion_screening.htm#viscreen 
C
C-----  Modifications:
C
C   1.)  VISCREEN (88341) was modified.  The areas are marked with 
C          comments above each respective change:
C
C        Line Action Labeled 
C         221 C    LFLAG never referenced - pae 7/5/13
C         858 C    Float removed from around DCPLUM - pae 7/5/13
C         875 C    Float removed from around DCPLUM - pae 7/5/13
C        1504 C    IVAL never referenced - pae 7/5/13
C
C   2.)  The program was recompiled using the latest Intel Fortran Compiler
C
C   3.)  Comments and issues can be emailed through: 
C
C          http://www.epa.gov/ttn/scram/comments.htm
C
C   4.)  An F5.3 format was changed to an F8.3 format
C
C***********************************************************************

********         Version 1.01   06 December 1988              *********

                       PROGRAM VISCREEN

C***********************************************************************
C
C                 Systems Applications, Inc.     (415) 472-4011
C                 101 Lucas Valley Road
C                 San Rafael, Ca     94903
C
C                 Telefax: 415/472-0907    Telex: 469287
C
C***********************************************************************
C 
C       VISCREEN is designed to calculate visual effects parameters 
C       (delta E and contrast) for a plume as observed from a given 
C       vantage point.  Parameters are calculated using three wavelengths 
C       of light (0.4, 0.55, 0.7 um) and for backgrounds against sky 
C       and dark terrain.  VISCREEN is designed to calculate these 
C       parameters for emission sources of particulate, NO, NO2, and 
C       soot.  The emissions are assumed to create an  infinitely long, 
C       straight plume whose position is specified by the program user.  
C       VISCREEN can be used for the first two levels of plume visual 
C       impact screening.  It is not designed for  analyses of regional 
C       haze.  As implied by its name, VISCREEN is designed as a
C       conservative screening tool.  Additional analysis using a more
C       refined plume visibility model should be conducted for sources
C       that exceed screening criteria with VISCREEN.
C 
C********************************************************************** 
C 
C       The major variables used in VISCREEN include the following: 
C 
C       ALPHA   -    angle between the line of sight and plume  
C                       centerline 
C       BABS    -    light absorption coefficient (m-1) 
C       BABSN   -    light absorption coefficient for NO2 (m2/g) 
C       BEXT    -    light extinction (scat + abs) coefficient (m-1) 
C       BSCAT   -    light scattering coefficient (m-1) 
C       BSCATV  -    light scattering per unit volume (m2/cm3) 
C       CGREEN  -    green plume contrast screening  criterion (= 0.05) 
C       CMASS   -    conversion factor from units of input to grams 
C       CPLUME  -    contrast of plume against sky 
C       CTIME   -    conversion factor from units of input to seconds 
C       D_____  -    density for fine, coarse, plume particulate, soot, 
C                       and primary SO4 (DFINE,DCOARS,DPART,DSOOT,DSO4) 
C       DCPLUM  -    double precision variable for CPLUME calculation
C       DDELCR  -    double precision variable for contrast against terrain
C       DELAB   -    delta E (L*A*B*), color difference parameter
C       DELSKY  -    delta E for plume against the sky 
C       DELTER  -    delta E for plume against terrain 
C       DELCR   -    contrast of plume against terrain 
C       DIST    -    distance between emission source and observer (km)
C       FLCK    -    double precisioin check for underflow (=1.e-20)
C       FS      -    solar light intensity
C       GAMMA   -    offset angle between plume centerline and line
C                       between emissions source and observer
C       IERR    -    error flag returned from OPENA and RESPND (=0,ok)
C       ILAMB   -    index for wavelength of light (e.g., 1= 0.4 um)
C       IPLTUS  -    character path name for result file
C       IPSMRY  -    character path name for summary file
C       ISIZE   -    index for particle size distribution
C       ISTAB   -    index for stability (1=A;2=B;...6=F)
C       ITHETA  -    index for scattering angle theta (angle between 
C                       ray between sun and observer and the line of sight
C       LAMBDA  -    wavelength of light (0.4, 0.55, 0.7 um)
C       L1DFLT  -    Level 1 default flag (=1,use defaults)
C       LMDFLT  -    Meteorology default flag (=1, use defaults)
C       LPDFLT  -    Particulate profile default flag (=1,use defaults)
C       LTDFLT  -    Green & Delta E threshold default flag (=1,use defaults)
C       NSCAT   -    wavelength dependence of light scattering
C       O3      -    ambient ozone concentration (ppm)
C       OMEGA   -    albedo (ratio of bscat to bext)                 
C       P       -    phase function (f of ISIZE, ILAMBDA, ITHETA)
C       PBACK   -    phase function for background atmosphere
C       PHI     -    azimuthal angle between line connecting emission
C                       source and observer and line of sight
C       PLUSKY  -    radiance of plume when observed against the sky
C       PLUTER  -    radiance of plume when observed against terrain
C       PRAY    -    phase function for Rayleigh (particle-free) atmosphere
C       Q_____  -    emission rate for plume particulate, NOx, NO2, soot,
C                       and SO4 (QPART,QNOX,QNO2,QSOOT,QSO4)
C       RO      -    distance between observer and the terrain background
C                       viewed behind plume (km)
C       RP      -    distance between observer and observed portion of
C                       plume (km)
C       RV      -    background visual range (km)
C       SKY     -    radiance of sky without plume
C       SKYMAX  -    maximum delta E for plume against the sky
C       TAUPLU  -    plume optical thickness
C       TERAIN  -    radiance of terrain without plume
C       TERMAX  -    maximum delta E for plume against terrain
C       THRESH  -    threshold delta E for screening success/failure (=2.0)
C       U       -    wind speed (meters/sec)
C       X       -    downwind distance between emission source and
C                       observed plume parcel (km)
C       XMIN    -    distance along plume to closest Class I area 
C                       boundary (km)
C       XMAX    -    distance along plume to most distant Class I area
C                       boundary (km)
C       XBAR    -    chromaticity weighting function (also YBAR and ZBAR)
C       XCAP0   -    chromaticity tristimulus value (also YCAP0 and ZCAP0)
C                       for white reference
C***********************************************************************
C
C      DATE        PROGRAMMER                    MODIFICATION
C   =========   ==================           =========================
C
C   12/06/87    D. A. Latimer                Original algorithm
C               Gaia Associates
C               1268 Idylberry Rd
C               San Rafael CA 94903
C               (415) 499-0955
C
C   01/07/88    M. C. Causley                Develop I/O structure
C               Systems Applications, Inc.
C               101 Lucas Valley Rd          
C               San Rafael CA 94903          
C               (415) 472-4011
C
C   02/08/88    R. G. Ireson                 Restructure screening checks
C               Systems Applications, Inc.   Implement narrow plume threshold
C
C
C   05/31/88    M. C. Causley                Revise output formats
C               Systems Applications, Inc.
C
C
C   08/23/88    M. C. Causley                Trap math underflow where
C               Systems Applications, Inc.   contrast values are very
C                                            small (CPLUME,DELCR)
C
C   09/09/88    M. C. Causley                Finalize version 1.00
C               Systems Applications, Inc.
C
C   12/06/88    M. C. Causley                Remove line 666 which checks
C               Systems Applications, Inc.   for angle less than 11.25 then
C                                            resets incorrectly. Now at
C                                            version 1.01
C
C
C**********************************************************************
C
      COMMON/COLOR/DELAB,XBAR(3),YBAR(3),ZBAR(3)
      COMMON/REF/XCAP0,YCAP0,ZCAP0
      COMMON /IO/ ITERM,ISMRY,ILOTUS
      COMMON /IFL/ IPSMRY,IPLTUS
      COMMON /COMR/ ALPHA(39), BABS,BEXT(3),
     +        CPLUME(3,2,39), DELCR(3,2,39), DIST, GAMMA,
     &        O3, P(3,2,9), PHI(39), RP(39), RV, TAU, X(39),
     &        U,OMEGA,XMIN,XMAX,DFINE,DCOARS,DPART,DSOOT,DSO4,
     &        SKYMAX(2),TERMAX(2),THRESH,CGREEN,
     &        QPARTI,QPART,QNOXI,QNOX,QNO2I,QNO2,QSOOTI,QSOOT,QSO4I,
     &        QSO4,RO(39),PRAY(2),PBACK(3,2),SCTANG(2),SKY(3,2),
     &        TERAIN(3,2,39),PLUSKY(3,2,39),PLUTER(3,2,39),DELSKY(2,39),
     &        DELTER(2,39),SPECB(3),SPECP(3),
     &        PSI(39),PERTHR(39),THRSKY(2,39),THRTER(2,39),RATIO,
     &        RATSKY(2),RATTER(2)
      COMMON /COMI/ ISIZE,ISTAB,ITHETA,IFINE,ICOARS,IPART,ISOOT,
     &         ISO4,ISKYMX(2),ITERMX(2),IANS,IEMISS,IDIST,IPAR,IMET,
     &         L1DFLT,LSCLAS(39),MXANG,MXLOS,IMASS,ITIME,
     &         LMDFLT,LPDFLT,LTDFLT
      INTEGER IERR,IANS
      COMMON /CRGI/ LFIRST,ISCYMX(2),RATSCY(2),ITRCMX(2),RATTRC(2)
      COMMON /COMC/ MASS,TIME,SOURCE,RECEPT,CLASSI,OBJSKY,OBJTER
      CHARACTER*2 OBJSKY(2,39),OBJTER(2,39)
      CHARACTER*3 MASS(5),TIME(5)
      CHARACTER*7 CLASSI(2)
      CHARACTER*24 SOURCE,RECEPT
C
      DOUBLE PRECISION DCPLUM,DDELCR,FLCK
      REAL CGREEN,BABSN(3),BSCATV(9),LAMBDA(3),NSCAT(9),CMASS(5),
     &     CTIME(5),LAMB,PDUM(54),FS(3),ANGLE(13),CONT(13)
C      INTEGER IERR,IANS
      CHARACTER*40 CSTR,IPSMRY,IPLTUS
C  LFLAG never referenced - pae 7/6/13
C      LOGICAL LFLAG
C
      DATA FLCK / 1.0E-20 /
      DATA LAMBDA/0.45,0.55,0.65/
      DATA NSCAT/2.8,2.1,1.6,1.0,0.2,4*0./
      DATA BSCATV/1.7,4.5,6.,6.7,5.,2.6,0.9,0.8,0.4/
      DATA BABSN/0.691,0.144,0.015/
      DATA ANGLE/0.02,0.025,0.033,0.05,0.1,0.2,0.33,0.5,1.,2.,5.,10.,
     &           16.67/
      DATA CONT/2.,0.572, 0.182,0.058,0.0190,0.010,0.0086,0.0084,
     &  0.0100,0.0154,0.032,0.06,0.10/
      DATA PDUM/
     &   5.17,7.76,9.61,11.94,15.09,15.84,10.98,8.39,7.28,
     &   0.330,0.199,0.172,0.169,0.174,0.143,0.082,0.064,0.046,
     &   4.24,6.49,8.11,10.33,13.64,16.07,13.64,11.67,9.23,
     &   0.429,0.247,0.193,0.165,0.166,0.156,0.094,0.085,0.055,
     &   3.64,5.62,7.14,9.27,12.54,15.47,14.83,12.83,10.55,
     &   0.517,0.296,0.219,0.175,0.170,0.170,0.136,0.106,0.075/
      DATA FS/1712., 1730., 1414./
      DATA CMASS/1.,1.E3,1.E6,453.6,9.072E5/
      DATA CTIME/1.,60.,3600.,86400.,3.154E7/
C
C***********************************************************************
C
      CALL INIT (IERR)
        IF (IERR.NE.0) GO TO 999

C     ----- INITIALIZE SOME MORE VARIABLES

      PI=ACOS(-1.)
      RAD=PI/180.
      SQRT2P = SQRT(2.*PI)
      TWOTAN = 2. * TAN(RAD*22.5/2.)
      K = 1
      DO 5 ILAMB = 1,3
        XCAP0 = XCAP0 + FS(ILAMB)/(2.*PI) * XBAR(ILAMB)
        YCAP0 = YCAP0 + FS(ILAMB)/(2.*PI) * YBAR(ILAMB)
        ZCAP0 = ZCAP0 + FS(ILAMB)/(2.*PI) * ZBAR(ILAMB)
        DO 5 ITHETA = 1,MXANG
        DO 5 ISIZE = 1,9
          P(ILAMB,ITHETA,ISIZE) = PDUM(K)
          K = K + 1
5     CONTINUE

C**********************************************************************

C       PROGRAM INPUT
C
C**********************************************************************
C
C     ----- WRITE OUT SCREEN INTRO
C
      WRITE (*,*) ' '
      WRITE (*,*)'=============================================='
      WRITE (*,*) ' '
      WRITE (*,*)'   WELCOME TO PROGRAM VISCREEN! (Ver 1.01)'
      WRITE (*,*)'               (Dated 13190)'
      WRITE (*,*) ' '
      WRITE (*,*)'=============================================='
      WRITE (*,*) ' '
C
C     ------ OPEN SUMMARY AND RESULTS FILE
C
      CALL OPENA (IERR)
        IF (IERR.NE.0) GO TO 999
C
      WRITE (*,*) ' '
      WRITE (*,'(A\)')' Input the name of the emissions source: '
      CALL RESPND (4,IDUM,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      SOURCE = CSTR(1:24)
      WRITE (*,'(A\)')' Input the name of the receptor (Class I area): '
      CALL RESPND (4,IDUM,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      RECEPT = CSTR(1:24)

C     ----- SET DEFAULT FLAGS FIRST TIME AROUND

      LMDFLT = 1
      LPDFLT = 1
      LTDFLT = 1
      L1DFLT = 1
      LFIRST = 1

C     ----- BACK TO HERE (LABEL 10) FOR SUBSEQUENT RUNS

10    CONTINUE
      IF(IEMISS.EQ.0) GO TO 150
11    WRITE (*,*) ' '
      WRITE (*,*)'Select the units of mass for emission',
     &            ' rates--'
      WRITE (*,*)'1=gram (g); 2=kg; 3=metric tonne (mt); 4=lb; 5=ton: '
      WRITE (*,'(A\)')' Enter no. (1-5): '
      CALL RESPND (2,IMASS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (IMASS.LT.1 .OR. IMASS.GT.5) THEN
        WRITE (*,*) ' Invalid mass unit = ',IMASS,' try again...'
        GO TO 11
      ENDIF

15    WRITE (*,*) ' '
      WRITE (*,*)'Select the units of time for emission rates--'
      WRITE (*,'(A\)')' 1=sec; 2=min; 3=hr; 4=day; 5=yr:',
     &                 ' Enter no. (1-5): '
      CALL RESPND (2,ITIME,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (ITIME.LT.1 .OR. ITIME.GT.5) THEN
        WRITE (*,*) ' Invalid unit of time = ',ITIME,' try again...'
        GO TO 15
      ENDIF

100   CONTINUE
      WRITE (*,*) ' '
      WRITE (*,*) 'Input the emission rates for the following',
     &                   ' species: '
      WRITE (*,'(A,A,A,A,A\)')' Particulates (',MASS( IMASS),'/',
     &                   TIME(ITIME),' ): '
      CALL RESPND (3,IDUM,QPARTI,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,'(A,A,A,A,A\)')' NOx (as NO2) (',MASS(IMASS),'/',
     &                  TIME(ITIME),' ): '
      CALL RESPND (3,IDUM,QNOXI,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999

      QNO2I = 0.
      QSOOTI = 0.
      QSO4I = 0.
      WRITE (*,*)' '
      WRITE (*,'(A\)')' Do you want to use default (zero) emission ',
     &                 ' rates for primary NO2,'
      WRITE (*,'(A\)')' soot, and sulfate (y/n)? '
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF(IANS.EQ.1) GO TO 110

      WRITE (*,'(A,A,A,A,A\)')' Primary NO2 (',MASS(IMASS),'/',
     &             TIME(ITIME),' ): '
      CALL RESPND (3,IDUM,QNO2I,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,'(A,A,A,A,A\)') ' Soot (',MASS(IMASS),'/',
     &                TIME(ITIME),' ): '
      CALL RESPND (3,IDUM,QSOOTI,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,'(A,A,A,A,A\)')' Primary SO4 (',MASS(IMASS),'/',
     &                   TIME(ITIME),' ): '
      CALL RESPND (3,IDUM,QSO4I,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999

110   WRITE (*,*)' '
      WRITE (*,*)'SUMMARY: Emissions for ',SOURCE
      WRITE (*,*)'Particulates ',QPARTI,' ',MASS(IMASS),'/',TIME(ITIME)
      WRITE (*,*)'NOx (as NO2) ',QNOXI,' ',MASS(IMASS),'/',TIME(ITIME)
      WRITE (*,*)'Primary NO2  ',QNO2I,' ',MASS(IMASS),'/',TIME(ITIME)
      WRITE (*,*)'Soot         ',QSOOTI,' ',MASS(IMASS),'/',TIME(ITIME)
      WRITE (*,*)'Primary SO4  ',QSO4I,' ',MASS(IMASS),'/',TIME(ITIME)
      WRITE (*,*)' '
      WRITE (*,'(A\)')
     &  ' Are these the emission rates you meant to use (y/n)? '
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF(IANS.EQ.0) THEN
        WRITE (*,*)'You may now enter new values of emission rates.'
        GO TO 100
      END IF

150   CONTINUE
      IF(IDIST.EQ.0) GO TO 200
      WRITE (*,*)' '
      WRITE (*,*)'Input the distance between the emissions source and'
      WRITE (*,'(A\)')' the observer (in kilometers): '
      CALL RESPND (3,IDUM,DIST,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,*) ' '
      WRITE (*,*)'Input the distance between the emissions source ',
     &            'and the'
      WRITE (*,'(A\)')' closest Class I area boundary (in kilometers): '
      CALL RESPND (3,IDUM,XMIN,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,*) ' '
      WRITE (*,*)'Input the distance between the emissions source ',
     &             'and the'
      WRITE (*,'(A,A\)')' most distant Class I area boundary ',
     &                '  (in kilometers):'
      CALL RESPND (3,IDUM,XMAX,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999

190   CONTINUE
      WRITE (*,*) ' '
      WRITE (*,'(A,A\)')' Input the background visual range for ',
     &               'the area (km): '
      CALL RESPND (3,IDUM,RV,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,*)' '

200   CONTINUE
      IF(IPAR.EQ.0) GO TO 250
      IF (LFIRST.EQ.1) THEN
        WRITE (*,*) ' '
        WRITE (*,'(A,A\)')' Do you wish to use Level-1 default ',
     &                   'parameters (y/n)? '
        CALL RESPND (1,IANS,RDUM,CSTR,IERR)
          IF (IERR.NE.0) GO TO 999
        IF (IANS.EQ.1) GO TO 360
      ENDIF
      L1DFLT = 0
      LPDFLT = 0
C**********************************************************************
C
C       LEVEL-2, NON-DEFAULT INPUT SPECIFICATION
C
C**********************************************************************
225   CONTINUE
      WRITE (*,*) 'SPECIFICATION OF PARTICLE DENSITY AND SIZE '
      WRITE (*,*) ' '
      WRITE (*,*) 'Enter the density and the index corresponding to the'
      WRITE (*,*) '  mass median diameter of the size distribution for '
      WRITE (*,*) '  BACKGROUND fine and coarse particulate, and'
      WRITE (*,*) '  PLUME particulate, soot, and primary sulfate).'
      WRITE (*,*) ' '
      WRITE (*,*)'Mass median diameter (in um): 1=0.1 um; 2=0.2 um; '
      WRITE (*,*)' 3=0.3 um; 4=0.5 um; 5=1 um; 6=2 um; 7=5 um; 8=6 um;'
      WRITE (*,*) ' 9=10 um.   '
      WRITE (*,*) ' '
      WRITE (*,*) 'Enter density (g/cm3) and size index'
      WRITE (*,'(A\)') ' (default values are shown in parentheses): '
      WRITE (*,*) ' '
      WRITE (*,'(A\)') ' Background Fine Particulate Density (1.5): '
      CALL RESPND (3,IDUM,DFINE,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
227   WRITE (*,'(A\)') ' Background Fine Particulate Size Index (3): '
      CALL RESPND (2,IFINE,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (IFINE.LT.1 .OR. IFINE.GT.9) THEN
        WRITE (*,*) ' Invalid size index = ',IFINE,' try again...'
        WRITE (*,*) ' '
        GO TO 227
      ENDIF

      WRITE (*,*) ' '
      WRITE (*,'(A\)') ' Background Coarse Particulate Density (2.5): '
      CALL RESPND (3,IDUM,DCOARS,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
229   WRITE (*,'(A\)') ' Background Coarse Particulate Size Index (8): '
      CALL RESPND (2,ICOARS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
       IF (ICOARS.LT.1 .OR. ICOARS.GT.9) THEN
        WRITE (*,*) ' Invalid size index = ',ICOARS,' try again...'
        WRITE (*,*) ' '
        GO TO 229
       ENDIF

      WRITE (*,*) ' '
      WRITE (*,'(A\)') ' Plume Particulate Density (2.5): '
      CALL RESPND (3,IDUM,DPART,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
231   WRITE (*,'(A\)') ' Plume Particulate Size Index (6): '
      CALL RESPND (2,IPART,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (IPART.LT.1 .OR. IPART.GT.9) THEN
        WRITE (*,*) ' Invalid size index = ',IPART,' try again...'
        WRITE (*,*) ' '
        GO TO 231
      ENDIF

      WRITE (*,*) ' '
      WRITE (*,'(A\)') ' Plume Soot Density (2.0): '
      CALL RESPND (3,IDUM,DSOOT,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
233   WRITE (*,'(A\)') ' Plume Soot Size Index (1): '
      CALL RESPND (2,ISOOT,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (ISOOT.LT.1 .OR. ISOOT.GT.9) THEN
        WRITE (*,*) ' Invalid size index = ',ISOOT,' try again...'
        WRITE (*,*) ' '
        GO TO 233
      ENDIF


      WRITE (*,*) ' '
      WRITE (*,'(A\)') ' Plume Primary SO4 Density (1.5): '
      CALL RESPND (3,IDUM,DSO4,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
235   WRITE (*,'(A\)') ' Plume Primary SO4 Size Index (4): '
      CALL RESPND (2,ISO4,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (ISO4.LT.1 .OR. ISO4.GT.9) THEN
        WRITE (*,*) ' Invalid size index = ',ISO4,' try again...'
        WRITE (*,*) ' '
        GO TO 235
      ENDIF

      WRITE (*,*) ' '
      WRITE (*,*)' Are you sure these are the values ',
     &                'you want for '
      WRITE (*,'(A\)')' particle densities and sizes (y/n)? '
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF(IANS.EQ.0) GO TO 225

250   CONTINUE
      IF(IMET.EQ.0) GO TO 360
      LMDFLT = 0
      WRITE (*,*)' '
      WRITE (*,*)'Enter Background Ozone (O3) Concentration in ppm '
      WRITE (*,'(A\)')'   (default = 0.04 ppm): '
      CALL RESPND (3,IDUM,O3,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999

300   CONTINUE
      WRITE (*,*)' '
      WRITE (*,'(A\)')' Enter the wind speed (in meters/sec): '
      CALL RESPND (3,IDUM,U,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
310   WRITE (*,*) 'Enter the stability index--'
      WRITE (*,'(A\)') '  (1=A; 2=B; 3=C; 4=D; 5=E; 6=F): '
      CALL RESPND (2,ISTAB,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (ISTAB.LT.1 .OR. ISTAB.GT.6) THEN
        WRITE (*,*) ' Invalid stability index = ',ISTAB,' try again...'
        WRITE (*,*) ' '
        GO TO 310
      ENDIF


350   CONTINUE
      WRITE (*,*) ' '
      WRITE (*,*)'Enter the plume offset angle (i.e., the angle between'
      WRITE (*,*)'  the plume centerline and the line between the '
      WRITE (*,*)'  observer and the emissions source) in degrees. '
      WRITE (*,'(A\)')'   Default is 11.25 degrees (1/2 sector width): '
      CALL RESPND (3,IDUM,GAMMA,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
C
C     ---CHECK TO SEE IF GAMMA IS ACCEPTABLE. IF NOT, REJECT OR WARN USER.
      WRITE (*,*) ' '
      IF(GAMMA.LT.11.25) THEN
        WRITE (*,*)'Your input value is less than recommended minimum!'
        IF(GAMMA.LE.0.) GO TO 350
        WRITE (*,'(A\)')' Do you still want to use your ',
     &                    ' input value (y/n)? '
        CALL RESPND (1,IANS,RDUM,CSTR,IERR)
          IF (IERR.NE.0) GO TO 999
        IF(IANS.EQ.0)  GO TO 350
      END IF
      IF(GAMMA.GT.180.) THEN
         WRITE (*,*)'Your value is greater than maximum of 180 degrees!'
         WRITE (*,'(A\)')' Please input value less than 180 degrees: '
         CALL RESPND (3,IDUM,GAMMA,CSTR,IERR)
           IF (IERR.NE.0) GO TO 999
      END IF
      IF(GAMMA.GT.168.75) THEN
        WRITE (*,*)'Your input value is greater than recommended',
     &                 ' maximum!'
        WRITE (*,'(A\)')' Do you still want to use your ',
     &                     'input value (y/n)? '
        CALL RESPND (1,IANS,RDUM,CSTR,IERR)
          IF (IERR.NE.0) GO TO 999
        IF(IANS.EQ.0) GO TO 350
      END IF
C**********************************************************************
C       END OF NON-DEFAULT SPECIFICATION
C
C***********************************************************************
C
C       PRINT A SUMMARY OF MAJOR INPUT VALUES
C
C***********************************************************************

360   CONTINUE
      WRITE (*,*) ' '
      WRITE (*,*) 'SUMMARY OF ALL EMISSIONS AND METEOROLOGICAL INPUT'
      WRITE (*,*) ' '
      WRITE (*,*) 'Emissions for ',SOURCE,' in ',MASS(IMASS),'/',
     &           TIME(ITIME),':'
      WRITE (*,*)'  Particulate = ',QPARTI
      WRITE (*,*)'  NOx         = ',QNOXI
      WRITE (*,*)'  Primary NO2 = ',QNO2I
      WRITE (*,*)'  Soot        = ',QSOOTI
      WRITE (*,*)'  Primary SO4 = ',QSO4I
      WRITE (*,*) ' '
      WRITE (*,*) 'Meteorological and Ambient Data for ',RECEPT
      WRITE (*,*) ' '
      WRITE (*,*) '  Wind speed (m/s)  =  ',U
      WRITE (*,*) '  Stability Index   =  ',ISTAB
      WRITE (*,*) '  Visual Range (km) =  ',RV
      WRITE (*,*) '  Ozone Conc. (ppm) =  ',O3
      WRITE (*,*) '  Plume Offset Angle=  ',GAMMA,' degrees'
      WRITE (*,*) ' '
      WRITE (*,*) 'Distances Between ',SOURCE,' and ',RECEPT
      WRITE (*,*) ' '
      WRITE (*,*) '  Source-Observer      = ',DIST, ' km'
      WRITE (*,*) '  Min. Source-Class I  = ',XMIN, ' km'
      WRITE (*,*) '  Max. Source-Class I  = ',XMAX, ' km'
      WRITE (*,*) ' '
      WRITE (*,'(A,A\)')' Are these input values ready for ',
     &                  'execution (y/n)? '
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF(IANS.EQ.0) GO TO 9000
      WRITE (*,*) ' '
      WRITE (*,'(A,A\)')' Do you want to use the default ',
     &                 'screening threshold (y/n)? '
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (IANS.EQ.1) GO TO 370
      LTDFLT = 0
      WRITE (*,*) ' '
      WRITE (*,'(A,F5.2,A\)') ' Input delta E threshold (default = ',
     &                   THRESH,'): '
      CALL RESPND (3,IDUM,THRESH,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,*) ' '
      WRITE (*,'(A,A,F5.2,A\)') ' Input green contrast threshold ',
     &               '(default = ',CGREEN,' ): '
      CALL RESPND (3,IDUM,CGREEN,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999

370   CONTINUE

C     ----- WRITE OUTPUT HEADER INFO TO SUMMARY FILE AND
C           INPUTS TO FULL RESULTS FILE
C
      IF ((LTDFLT*LMDFLT*LPDFLT).EQ.0) L1DFLT = 0
      CALL SMRPT1 (IERR)
      CALL WINPTS (IERR)
C
C**********************************************************************
C
C       PROGRAM EXECUTION
C
C**********************************************************************
C
C     ----- STORING ARRAYS INITIALIZED
C
      CALL INIT2 (IERR)
C
C       STEP 1: CONVERT ALL EMISSION RATES TO GRAMS/SECOND
C               UNLESS THAT'S HOW THEY ARE ON INPUT
C
      CONVER = CMASS(IMASS)/CTIME(ITIME)
      QPART = QPARTI*CONVER
      QNOX = QNOXI*CONVER
      QNO2 = QNO2I*CONVER
      QSOOT = QSOOTI*CONVER
      QSO4 = QSO4I*CONVER
C
C     --ASSUME 10% OF INITIAL NO IS CONVERTED THERMALLY TO NO2
      QNO2 = QNO2 + 0.1*QNOX
      QNOX = 0.9*QNOX
C
C       STEP 2: DETERMINE PLUME-OBSERVER GEOMETRY FOR EVERY LINE OF SIGHT
C                  CORRESPONDING TO 5 DEGREES OF AZIMUTH SCAN STARTING FROM 
C                  THE SOURCE
C
375   CONTINUE
      GAMMA2 = GAMMA + 11.25
      IMAX = (180.-GAMMA)/5 -2
      DO 400 IVIEW = 1,IMAX
C       ---OBSERVER LINES OF SIGHT ARE DRAWN FOR EACH 5 DEGREES

        PHI(IVIEW) = IVIEW*5.

C       ---ANGLE ALPHA: SINCE SUM OF INTERIOR ANGLES OF TRIANGLE TOTALS 180.

        ALPHA(IVIEW) = 180. - PHI(IVIEW) - GAMMA

C       ---SINCE THE RATIOS OF THE LENGTHS OF THE SIDES OF A TRIANGLE TO THE
C          SINE OF THE OPPOSITE ANGLE ARE EQUAL, .....

        X(IVIEW) = DIST * SIN(RAD*PHI(IVIEW))/SIN(RAD*ALPHA(IVIEW))
        RP(IVIEW) = DIST * SIN(RAD*GAMMA)/SIN(RAD*ALPHA(IVIEW))
        ALPHA2 = ALPHA(IVIEW) - 11.25
        RO(IVIEW) = DIST * SIN(RAD*GAMMA2)/SIN(RAD*ALPHA2)
        IF(ALPHA2.LT.0.) RO(IVIEW) = RP(IVIEW)
        IF(RP(IVIEW).GT.999.) RP(IVIEW) = 999.
        IF(RO(IVIEW).GT.999.) RO(IVIEW) = 999.
400   CONTINUE
C       
C       STEP 2A: DETERMINE PLUME-OBSERVER GEOMETRY FOR THREE ADDITIONAL
C                   LINES OF SIGHT DESIGNED TO PASS THROUGH THE PLUME PARCEL
C                   LOCATED 1 KM DOWNWIND FROM SOURCE, AT THE NEAREST CLASS 
C                   I AREA BOUNDARY, AND AT THE MOST DISTANT CLASS I AREA
C                   BOUNDARY.
C
      X(IMAX+1) = 1.
      X(IMAX+2) = XMIN
      X(IMAX+3) = XMAX
      DO 450 IVIEW = IMAX+1,IMAX+3
        DPRIME = X(IVIEW) * COS(RAD*GAMMA)
        DDOUBL = DIST - DPRIME
        H = X(IVIEW) * SIN(RAD*GAMMA)
        PHI(IVIEW) = (1./RAD) * ATAN(H/DDOUBL)
        IF(PHI(IVIEW).LT.0.) PHI(IVIEW) = PHI(IVIEW) + 180.
        ALPHA(IVIEW) = 180. - GAMMA - PHI(IVIEW)
        RP(IVIEW) = X(IVIEW) * SIN(RAD*GAMMA)/SIN(RAD*PHI(IVIEW))
        ALPHA2 = ALPHA(IVIEW) - 11.25
C...        IF(ALPHA2.LT.11.25)ALPHA2 = 11.25
        RO(IVIEW) = DIST * SIN(RAD*GAMMA2)/SIN(RAD*ALPHA2)
        IF(ALPHA2.LT.0.) RO(IVIEW) = RP(IVIEW)
        IF(RP(IVIEW).GT.999.) RP(IVIEW) = 999.
        IF(RO(IVIEW).GT.999.) RO(IVIEW) = 999.
450   CONTINUE
C
C       STEP 3: CALCULATE THE ATMOSPHERE OPTICAL CHARACTERISTICS
C
C       USING KOSCHMIEDER EQUATION ASSUMING VISUAL RANGE IS RELATED TO
C       THE LIGHT EXTINCTION AT 0.55 UM (ILAMBDA = 2)
C
      BEXT(2) = 3.912/(RV*1000.)
      BRAY = 11.62E-6
      BSP = BEXT(2) - BRAY
      IF(BSP.LT.0.) THEN
        WRITE (*,*) 'Your chosen value of background visual range',
     &              ' is too high.  Please try again!'
        GO TO 190
      END IF
      BSFINE = 0.67 * BSP
      BSCOAR = 0.33 * BSP

C       RAYLEIGH PHASE FUNCTION AT THETA = 10 AND 140 DEGREES; ALL LAMBDA
C       (CALCULATED FROM EQUATION: PRAY = 0.75*(1.+(COS(THETA))**2)

      PRAY(1) = 1.4774
      PRAY(2) = 1.1901
      DO 500 ILAMB = 1,3
        BSF = BSFINE * (LAMBDA(ILAMB)/0.55)**(-NSCAT(IFINE))
        BSC = BSCOAR * (LAMBDA(ILAMB)/0.55)**(-NSCAT(ICOARS))
        BSR = BRAY *   (LAMBDA(ILAMB)/0.55)**(-4.1)
        BEXT(ILAMB) = BSF + BSC + BSR
        DO 500 ITHETA = 1,2
          PBACK(ILAMB,ITHETA) = ( BSF*P(ILAMB,ITHETA,IFINE) +
     &                              BSC*P(ILAMB,ITHETA,ICOARS) +
     &                              BSR*PRAY(ITHETA) ) / BEXT(ILAMB)
          SKY(ILAMB,ITHETA) = FS(ILAMB)/(4.*PI) * PBACK(ILAMB,ITHETA)
500   CONTINUE
C
C       STEP 4: CALCULATE THE PLUME DIMENSIONS, CONCENTRATIONS, AND
C                    OPTICAL THICKNESS FOR EACH OF THE LINES OF SIGHT:
C                    IVIEW = 1, IMAX+3
C
      IHI = IMAX + 3
      DO 550 IVIEW = 1,IHI
C
C       STEP 4A:  CALCULATE THE PLUME NO2 CONCENTRATION RESULTING FROM
C                 TITRATION WITH AMBIENT OZONE, ASSUMING THE PLUME IS
C                 UNIFORMLY SPREAD HORIZONTALLY IN A 22.5 DEGREE SECTOR.
C
        XM = X(IVIEW) * 1000.
        IF(XM.LT.100.) XM = 100.
C
C       IF THE STABILITY IS 5 OR 6 (E OR F), WE ASSUME THAT SUCH 
C       CONDITIONS PERSIST FOR ONLY 12 HOURS.  IF THE TRANSPORT TIME
C       NECESSARY TO TRANSPORT IS LONGER, WE INCREASE WIND SPEED U SUCH
C       THAT THE TRANSPORT TIME TO THE GIVEN DISTANCE IS EXACTLY 12 
C       HOURS.  THIS IS AN APPROXIMATE WAY TO ACCOUNT FOR PERSISTENCE
C       OF EXTREME CONDITIONS AND FOR THE SUBSEQUENT INCREASED DISPERSION
C       AFTER 12 HOURS.
C
        UNEW = U
        IF(ISTAB.GT.4) THEN
          TRANST = X(IVIEW)*1000./U/3600.
          IF(TRANST.GT.12.) THEN
            TRANST = 12.
            UNEW = X(IVIEW)*1000./3600./TRANST
          END IF
        END IF
        SZ = SZPAS(ISTAB,XM)
        XNOX = QNOX*1.E6*5.315E-4/(SQRT2P*SZ*UNEW*TWOTAN*XM)
        XNOX2 = XNOX
        IF(XNOX.GT.O3) XNOX2 = O3
        IF(XNOX.EQ.0.) THEN
          QNOX2 = 0.
        ELSE
          QNOX2 = XNOX2/XNOX*QNOX
        END IF
C
C       STEP 4B:        CALCULATE THE OPTICAL THICKNESS
C
        ALPHAP = ALPHA(IVIEW)
        IF (ALPHAP.LT.5.) ALPHAP = 5.
        DENOM = SQRT2P * SZ * UNEW * SIN(RAD * ALPHAP)
        DO 550  ILAMB = 1,3
          LAMB = LAMBDA(ILAMB)
          SPART = QPART*BSCATV(IPART)/
     &                  DPART*(LAMB/0.55)**(-NSCAT(IPART))
          SSO4 =  QSO4*BSCATV(ISO4)/DSO4*(LAMB/0.55)**(-NSCAT(ISO4))
          SSOOT = QSOOT*BSCATV(ISOOT)/DSOOT*(LAMB/0.55)**(-NSCAT(ISOOT))
          SCAT = SPART + SSO4 + SSOOT
          ABSP  = (QNO2 + QNOX2) * BABSN(ILAMB) + QSOOT * 10.
          PEXT = SCAT + ABSP
          IF(PEXT.EQ.0.) THEN
            OMEGA = 1.
          ELSE
            OMEGA  = SCAT/PEXT
          END IF
          TAUPLU = PEXT / DENOM
C
C       STEP 4C:  CALCULATE THE VERTICAL ANGULAR SUBTENSE OF THE PLUME,
C                 PSI, AS VIEWED FROM THE GIVEN DISTANCE, RP.
C
          PSI(IVIEW) = (1./RAD) * ATAN( 4.3 * SZ / (RP(IVIEW) * 1.E3))
C
C       STEP 4D:  CALCULATE THE THRESHOLD CONTRAST BY INTERPOLATING THE
C                 DATA OF HOWELL AND HESS (1978) FOR THE CALCULATED VALUE
C                 OF PSI.
          J = 13
          DO 520 I = 2,13
            IF (PSI(IVIEW).LT.ANGLE(I)) THEN
              J = I
              GO TO 530
            END IF
520       CONTINUE

530       CONTINUE
C
          PERTHR(IVIEW) = (PSI(IVIEW)-ANGLE(J-1))/(ANGLE(J)-ANGLE(J-1))
     &                        * (CONT(J) - CONT(J-1)) + CONT(J-1)
C
C       
C       STEP 5: CALCULATE PLUME CONTRAST AGAINST SKY AND TERRAIN FOR
C                       BOTH FORWARD AND BACKWARD SCATTER CASES
C
          DO 550 ITHETA = 1,MXANG
            IF(SCAT.EQ.0.) THEN
              PPLUME = 0.
            ELSE
              PPLUME = (SPART * P(ILAMB,ITHETA,IPART) +
     &                    SSO4  * P(ILAMB,ITHETA,ISO4) +
     &                     SSOOT  * P(ILAMB,ITHETA,ISOOT)) /SCAT
            END IF
C
C           CONTRAST AGAINST SKY
C

            DCPLUM = (PPLUME*OMEGA/PBACK(ILAMB,ITHETA)-1.) *
     &         (1. - EXP(-TAUPLU)) * EXP(-BEXT(ILAMB)*RP(IVIEW)*1.E3)
            IF (DCPLUM.GT.-FLCK .AND. DCPLUM.LT.FLCK) DCPLUM = 0.0
C  Float removed from around DCPLUM - pae 7/5/13
            CPLUME(ILAMB,ITHETA,IVIEW) = DCPLUM
C
C            RADIANCE OF PLUME AGAINST SKY
C
            PLUSKY(ILAMB,ITHETA,IVIEW) = (1. +
     &          CPLUME(ILAMB,ITHETA,IVIEW))*PBACK(ILAMB,ITHETA) *
     &          FS(ILAMB)/(4.*PI)
            IF(PLUSKY(ILAMB,ITHETA,IVIEW).LT.0.)
     &            PLUSKY(ILAMB,ITHETA,IVIEW)=0.
C
C           CONTRAST AGAINST TERRAIN -- SET TO MAX OF GREEN CONTRAST
C
C
            DDELCR = EXP(-BEXT(ILAMB)*RO(IVIEW)*1.E3)*
     &          (1. - (EXP(-TAUPLU)/(1. + CPLUME(ILAMB,ITHETA,IVIEW))))
            IF (DDELCR.GT.-FLCK .AND. DDELCR.LT.FLCK) DDELCR = 0.0
C  Float removed from around DCPLUM - pae 7/5/13
            DELCR(ILAMB,ITHETA,IVIEW) = DDELCR
C
C           RADIANCE OF TERRAIN

            TERAIN(ILAMB,ITHETA,IVIEW) = (1.-EXP(-BEXT(ILAMB)*
     &           RO(IVIEW)*1.E3))
     &              * PBACK(ILAMB,ITHETA) * FS(ILAMB)/(4.*PI)
            IF (TERAIN(ILAMB,ITHETA,IVIEW).LT.0.)
     &         TERAIN(ILAMB,ITHETA,IVIEW)=0.
C
C           RADIANCE OF PLUME AGAINST TERRAIN
            PLUTER (ILAMB,ITHETA,IVIEW) = (1.-EXP(-BEXT(ILAMB)*
     &           RO(IVIEW)*1.E3) + DELCR(ILAMB,ITHETA,IVIEW)) *
     &           PBACK(ILAMB,ITHETA) * FS(ILAMB)/(4.* PI)
            IF (PLUTER(ILAMB,ITHETA,IVIEW).LT.0.)
     &           PLUTER(ILAMB,ITHETA,IVIEW)=0.
550   CONTINUE

      DO 570 IVIEW = 1, IHI
        DO 570 ITHETA = 1, MXANG
          DO 560 ILAMB = 1,3
            SPECP(ILAMB) = PLUSKY(ILAMB,ITHETA,IVIEW)
            SPECB(ILAMB) = SKY(ILAMB,ITHETA)
560       CONTINUE
          CALL CHROMA(SPECP,SPECB)
          DELSKY(ITHETA,IVIEW) = DELAB
          DO 561 ILAMB = 1,3
            SPECP(ILAMB) = (1. + PERTHR(IVIEW) ) * SKY(ILAMB,ITHETA)
561       CONTINUE
          CALL CHROMA(SPECP,SPECB)
          THRSKY(ITHETA,IVIEW) = DELAB
          IF (THRSKY(ITHETA,IVIEW).LT.THRESH)
     &        THRSKY(ITHETA,IVIEW) = THRESH
          DO 565 ILAMB = 1,3
            SPECP(ILAMB) = PLUTER(ILAMB,ITHETA,IVIEW)
            SPECB(ILAMB) = TERAIN(ILAMB,ITHETA,IVIEW)
565       CONTINUE
          CALL CHROMA(SPECP,SPECB)
          DELTER(ITHETA,IVIEW) = DELAB
          DO 567 ILAMB = 1,3
            SPECP(ILAMB) = (1.+PERTHR(IVIEW))*TERAIN(ILAMB,ITHETA,IVIEW)
567       CONTINUE
          CALL CHROMA(SPECP,SPECB)
          THRTER(ITHETA,IVIEW) = DELAB
          IF (THRTER(ITHETA,IVIEW).LT.THRESH)
     &              THRTER(ITHETA,IVIEW) = THRESH
C
C         ----- SCREENING CRITERIA FOR GREEN CONTRAST
C
          IF (PERTHR(IVIEW).LT.CGREEN) PERTHR(IVIEW) = CGREEN
C
570   CONTINUE
C***********************************************************************
C
C       EVALUATION AND OUTPUT OF CALCULATED PLUME DELTA E AND CONTRAST
C
C       IN THIS SECTION WE DETERMINE WHETHER ANY OF THE CALCULATED 
C       DELTA E OR CONTRAST VALUES ARE GREATER THAN SCREENING CRITERIA. 
C       RESULTS ARE SEGREGRATED BY WHETHER EFFECTS ARE VIEWED AGAINST
C       A SKY OR A TERRAIN BACKGROUND AND WHETHER EFFECTS ARE CAUSED
C       BY PLUME PARCELS LOCATED INSIDE OR OUTSIDE THE CLASS I AREA.
C
C***********************************************************************
C
C       STEP 1:   DETERMINE WHETHER SCREENING THRESHOLD IS EXCEEDED AND
C                 WHERE.  THIS IS DONE SEPARATELY FOR VIEWS AGAINST SKY
C                 AND TERRAIN AND FOR VIEWS OF PARCELS INSIDE OR OUTSIDE.
C
      DO 580 IVIEW = 1, IHI
      DO 580 ITHETA = 1, MXANG
        OBJSKY(ITHETA,IVIEW) = '  '
        OBJTER(ITHETA,IVIEW) = '  '
C
        IF(DELSKY(ITHETA,IVIEW).GE.THRSKY(ITHETA,IVIEW))
     &          OBJSKY(ITHETA,IVIEW)(1:1) = '*'
        CKGRN = ABS(CPLUME(2,ITHETA,IVIEW))
        IF(CKGRN.GE.PERTHR(IVIEW))
     &          OBJSKY(ITHETA,IVIEW)(2:2) = '*'
C
        IF(DELTER(ITHETA,IVIEW).GE.THRTER(ITHETA,IVIEW))
     &           OBJTER(ITHETA,IVIEW)(1:1) = '*'
        CKGRN = ABS(DELCR(2,ITHETA,IVIEW))
        IF(CKGRN.GE.PERTHR(IVIEW))
     &           OBJTER(ITHETA,IVIEW)(2:2) = '*'
C
580   CONTINUE
      DO 600 IVIEW = 1,IHI
        ICLASS = 2
        IF((X(IVIEW).GE.XMIN).AND.(X(IVIEW).LE.XMAX)) ICLASS = 1
        LSCLAS(IVIEW) = ICLASS
        DO 600 ITHETA = 1, MXANG
C
C      DELTA E CHECKS
          RATIO = DELSKY(ITHETA,IVIEW)/THRSKY(ITHETA,IVIEW)
          IF(DELSKY(ITHETA,IVIEW).GE.SKYMAX(ICLASS))
     &        SKYMAX(ICLASS) = DELSKY(ITHETA,IVIEW)
          IF(RATIO. GE. RATSKY(ICLASS)) THEN
            ISKYMX(ICLASS) = IVIEW
            RATSKY(ICLASS) = RATIO
          END IF
          RATIO = DELTER(ITHETA,IVIEW)/THRTER(ITHETA,IVIEW)
          IF (DELTER(ITHETA,IVIEW).GE.TERMAX(ICLASS))
     &       TERMAX(ICLASS) = DELTER(ITHETA,IVIEW)
          IF(RATIO. GE. RATTER(ICLASS)) THEN
            ITERMX(ICLASS) = IVIEW
            RATTER(ICLASS) = RATIO
          END IF
C
C        --NOW DO CONTRAST CHECKS
          CKGRN = ABS(CPLUME(2,ITHETA,IVIEW))
          RATIO = CKGRN/PERTHR(IVIEW)
          IF(RATIO. GE. RATSCY(ICLASS)) THEN
            ISCYMX(ICLASS) = IVIEW
            RATSCY(ICLASS) = RATIO
          END IF
          CKGRN = ABS(DELCR(2,ITHETA,IVIEW))
          RATIO = CKGRN/PERTHR(IVIEW)
          IF(RATIO. GE. RATTRC(ICLASS)) THEN
            ITRCMX(ICLASS) = IVIEW
            RATTRC(ICLASS) = RATIO
          END IF
C
600   CONTINUE
      WRITE (*,610) SOURCE,RECEPT
610   FORMAT(1X,///,1X,'OVERALL RESULTS OF PLUME VISIBILITY SCREENING',/
     &        /,1X,'SOURCE:  ',A24,/,1X,'CLASS I AREA:  ',A24/1X)
      DO 700 ICLASS = 1,2
        WRITE (*,611) CLASSI(ICLASS)
611     FORMAT (2X,A,' class I area --')
        IF (RATSKY(ICLASS).GE.1.) THEN
          WRITE (*,*) 'Plume delta E EXCEEDS screening criterion for ',
     &               'SKY background'
        ELSE
          WRITE (*,*) 'Plume delta E DOES NOT EXCEED screening ',
     &                'criterion for SKY background'
        ENDIF
        IF (RATTER(ICLASS).GE.1.) THEN
          WRITE (*,*) 'Plume delta E EXCEEDS screening criterion for ',
     &               'TERRAIN background'
        ELSE
          WRITE (*,*) 'Plume delta E DOES NOT EXCEED screening ',
     &                 'criterion for TERRAIN background'
        ENDIF

        IF (RATSCY(ICLASS).GE.1.) THEN
          WRITE (*,*)'Plume contrast EXCEEDS screening criterion for ',
     &                'SKY background'
        ELSE
          WRITE (*,*) 'Plume contrast DOES NOT EXCEED screening ',
     &                 'criterion for SKY background'
        ENDIF
        IF (RATTRC(ICLASS).GE.1.) THEN
             WRITE (*,*)'Plume contrast EXCEEDS screening ',
     &                   'criterion for TERRAIN background'
        ELSE
          WRITE (*,*)'Plume contrast DOES NOT EXCEED screening ',
     &                'criterion for TERRAIN background'
        ENDIF
        WRITE (*,*)' '
700   CONTINUE

      WRITE (*,705) THRESH,CGREEN
705   FORMAT(1X/1X,' SCREENING CRITERIA:        DELTA E = ',
     &          F4.1/3X,'                   GREEN CONTRAST = ',F8.3)
C
C     ----- CALL REPORTING SUBROUTINE TO OUTPUT RESULTS
C
      CALL SMRPT2 (IERR)
      CALL WRESLT (IHI,IERR)
C
C     ------ CONTINUE WITH OUTPUT TO SCREEN
C
      WRITE (*,710)
710   FORMAT (1X/1X,'Do you want to see calculated results for lines ',
     &        'of',/,1X,'sight with maximum delta E (y/n)? '\)
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
         IF (IERR.NE.0) GO TO 999
      IF(IANS.EQ.0) GO TO 805
      WRITE (*,720)
      WRITE (*,721)
720   FORMAT (1X,'VIEW',4X,'ANGLES (DEGREES)',7X,'DIST (KM)',
     &       2X,'PLUME PERCEPTIBILITY  DELTA E(L*A*B*)'/
     &       2X,'no',6X,'phi',2X,'alpha',3X,'psi',5X,'  x  ',3X,'rp',
     &       6X,'   forward   ',2X,'   backward')
721   FORMAT (1X,'----',5X,'---',
     &       2X,'-----',3X,'---',6X,'---',4X,'--',9X,'-------',8X,
     &       '--------')
      DO 800 ICLASS = 1,2
        WRITE (*,730)
730     FORMAT(5X,/,1X,'Line of sight with maximum perceptibility for ',
     &       'plume viewed ')
        WRITE (*,740) CLASSI(ICLASS)
740     FORMAT(10X,'against a SKY background ',A7,' class I area.')
750     FORMAT(10X,'against a TERRAIN background ',A7,' class I area.')
        I = ISKYMX(ICLASS)
        WRITE (*,760) I, PHI(I), ALPHA(I), PSI(I), X(I), RP(I),
     &       DELSKY(1,I),OBJSKY(1,I)(1:1),DELSKY(2,I),OBJSKY(2,I)(1:1)
760     FORMAT(2X,I2,2X,2F7.1,F7.2,2F7.1,6X,F7.1,1X,A1,7X,F7.1,1X,A1)
        WRITE (*,730)
        WRITE (*,750) CLASSI(ICLASS)
        I = ITERMX(ICLASS)
        WRITE (*,760) I, PHI(I), ALPHA(I), PSI(I),X(I), RP(I),
     &     DELTER(1,I),OBJTER(1,I)(1:1),DELTER(2,I),OBJTER(2,I)(1:1)
800   CONTINUE
      WRITE (*,809)
809   FORMAT(1X,'------------------'/2X,'* Exceeds screening criteria')

805   CONTINUE
      WRITE (*,810)
810   FORMAT(1X/1X,'Do you want to see calculations for all',
     &         ' lines of sight (y/n)?  '\ )
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF(IANS.EQ.0) GO TO 900
      ICOUNT = 1
      ILAST = IMAX + 3

815   CONTINUE
      IHIGH = 0

820   CONTINUE
      ILOW = IHIGH + 1
      IHIGH = ILOW + 17
      IF (IHIGH.GT.ILAST) IHIGH = ILAST
      IF(ICOUNT.EQ.1) THEN
        WRITE (*,*) ' '
        WRITE (*,830) 'PLUME DELTA E AGAINST A SKY BACKGROUND'
      ELSE
        WRITE (*,*) ' '
        WRITE (*,830) 'PLUME DELTA E AGAINST A TERRAIN BACKGROUND'
830     FORMAT (1X,A60)
        END IF
      WRITE (*,720)
      DO 850 I = ILOW,IHIGH
        IF(ICOUNT.EQ.1) THEN
          WRITE (*,760) I, PHI(I), ALPHA(I), PSI(I), X(I), RP(I),
     &      DELSKY(1,I),OBJSKY(1,I)(1:1),DELSKY(2,I),OBJSKY(2,I)(1:1)
        ELSE
        WRITE (*,760) I, PHI(I), ALPHA(I), PSI(I),X(I), RP(I),
     &      DELTER(1,I),OBJTER(1,I)(1:1),DELTER(2,I),OBJTER(2,I)(1:1)
        END IF
850   CONTINUE
      WRITE (*,'(A\)')' Please press [ENTER] for more, Q to quit',
     &             '  '
      CALL RESPND (5,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (IANS.EQ.2) GO TO 900
      IF (IHIGH.LT.ILAST) GO TO 820
      ICOUNT = ICOUNT + 1
      IF (ICOUNT.EQ.3) GO TO 900
      GO TO 815

C
900   CONTINUE
C       
C       DISPLAY CONTRAST VALUES THIS TIME RATHER THAN DELTA E
C
      DO 5000 I = 1,2
        SKYMAX(I) = 0.
        TERMAX(I) = 0.
5000  CONTINUE
      DO 6000 IVIEW = 1,IHI
        ICLASS = LSCLAS(IVIEW)
        DO 6000 ITHETA = 1,MXANG
          DO 6000 ILAMB = 1,3
            IF(ABS(CPLUME(ILAMB,ITHETA,IVIEW)).GE.SKYMAX(ICLASS)) THEN
              SKYMAX(ICLASS) = ABS(CPLUME(ILAMB,ITHETA,IVIEW))
              ISKYMX(ICLASS) = IVIEW
            END IF
            IF (ABS(DELCR(ILAMB,ITHETA,IVIEW)).GE.TERMAX(ICLASS)) THEN
              TERMAX(ICLASS) = ABS(DELCR(ILAMB,ITHETA,IVIEW))
             ITERMX(ICLASS) = IVIEW
            END IF
6000  CONTINUE
      WRITE (*,7100)
7100  FORMAT(1X,///,1X,'Do you want to see calculated results for',
     & ' lines of',/,1X,'sight with maximum green contrast? (y/n) '\)
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF(IANS.EQ.0) GO TO 8050
7200  FORMAT(48X,'-GREEN PLUME CONTRAST-'/1X,'VIEW',5X,
     &'ANGLES',8X,'DISTANCES (KM)',11X,'forward',6X,'backward',1X,
     &       'screening'/2X,'no',5X,'phi',2X,'alpha',4X,'  x  ',
     &       4X,'rp',5X,'ro',8X,'contrast',5X,'contrast',1X,'criterion'/
     &       1X,'----',4X,'---',2X,'-----',4x,' --- ',4X,'--',5X,'--',
     &       8X,'--------',5X,'--------',1X,'---------')
C
      WRITE (*,7200)
      DO 8000 ICLASS = 1,2
        WRITE (*,7300)
7300    FORMAT(5X,/,1X,'Line of sight with maximum contrast for plume',
     &            ' viewed')
        WRITE (*,7400) CLASSI(ICLASS)
7400    FORMAT(10X,'against a SKY background ',A7,' class I area.')
7500    FORMAT(10X,'against a TERRAIN background ',A7,' class I area.')
        I = ISCYMX(ICLASS)
        WRITE (*,7600) I, PHI(I), ALPHA(I), X(I), RP(I), RO(I),
     &    CPLUME(2,1,I),OBJSKY(1,I)(2:2),CPLUME(2,2,I),
     &     OBJSKY(2,I)(2:2),PERTHR(I)
7600    FORMAT(2X,I2,2X,5F7.1,6X,F7.3,1X,A1,4X,F7.3,1X,A1,2X,F6.2)
        WRITE (*,7300)
        WRITE (*,7500) CLASSI(ICLASS)
        I = ITRCMX(ICLASS)
        WRITE (*,7600) I, PHI(I), ALPHA(I), X(I), RP(I), RO(I),
     &     DELCR(2,1,I),OBJTER(1,I)(2:2),DELCR(2,2,I),OBJTER(2,I)(2:2),
     &     PERTHR(I)
8000  CONTINUE
      WRITE (*,8090)
8090  FORMAT (1X,'-------------------------------'/2X,'*   Absolute ',
     &         'value exceeds screening criteria')
C
8050  CONTINUE
      WRITE (*,8100)
8100  FORMAT(1X,/1X,' Do you want to see green contrast values for all',
     &          ' lines of sight (y/n)? '\)
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF(IANS.EQ.0) GO TO 9000
      ICOUNT = 1
      ILAST = IMAX + 3

8150  CONTINUE
      IHIGH = 0

8200  CONTINUE
      ILOW = IHIGH + 1
      IHIGH = ILOW + 17
      IF(IHIGH.GT.ILAST) IHIGH = ILAST
      IF(ICOUNT.EQ.1) THEN
        WRITE (*,8300) 'PLUME CONTRAST AGAINST A SKY BACKGROUND'
      ELSE
        WRITE (*,8300) 'PLUME CONTRAST AGAINST A TERRAIN BACKGROUND'
8300    FORMAT(1X//1X,A60)
      END IF
      WRITE (*,7200)
      DO 8500 I = ILOW,IHIGH
        IF(ICOUNT.EQ.1) THEN
        WRITE (*,7600) I, PHI(I), ALPHA(I), X(I), RP(I), RO(I),
     &    CPLUME(2,1,I),OBJSKY(1,I)(2:2),CPLUME(2,2,I),
     &     OBJSKY(2,I)(2:2),PERTHR(I)
        ELSE
          WRITE (*,7600) I, PHI(I), ALPHA(I), X(I), RP(I), RO(I),
     &       DELCR(2,1,I),OBJTER(1,I)(2:2),DELCR(2,2,I),
     &       OBJTER(2,I)(2:2),PERTHR(I)
        END IF
8500  CONTINUE
      WRITE (*,'(A\)')' When you''re ready, please press [ENTER] for ',
     &             ' more lines of sight (Q to quit)'
      CALL RESPND (5,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF (IANS.EQ.2) GO TO 9000
      IF(IHIGH.LT.ILAST) GO TO 8200
      ICOUNT = ICOUNT + 1
      IF(ICOUNT.EQ.3) GO TO 9000
      GO TO 8150
C
9000  CONTINUE
      WRITE (*,910)
910   FORMAT(1X,//,1X,'Do you want to quit (y/n)? '\)
      CALL RESPND (1,IANS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      IF(IANS.EQ.1) GO TO 1000
      LFIRST = 0
      DO 915 I = 1,2
        SKYMAX(I) = 0.
        TERMAX(I) = 0.
        RATSKY(I) = 0.
        RATTER(I) = 0.
915   CONTINUE
920   FORMAT(1X,//,1X,'Do you want to change emissions? (y/n)'\)
930   FORMAT(1X,//,1X,'Do you want to change distances? (y/n)'\)
940   FORMAT(1X,//,1X,'Do you want to change particle sizes or ',
     &              'densities? (y/n)'\)
950   FORMAT(1X,//,1X,'Do you want to change meteorology? (y/n)'\)
      WRITE (*,920)
      CALL RESPND (1,IEMISS,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,930)
      CALL RESPND (1,IDIST,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,940)
      CALL RESPND (1,IPAR,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      WRITE (*,950)
      CALL RESPND (1,IMET,RDUM,CSTR,IERR)
        IF (IERR.NE.0) GO TO 999
      GO TO 10

1000  CONTINUE
      WRITE (*,*) ' *******************'
      WRITE (*,*)' '
      WRITE (*,*) ' VISCREEN summary report file is: ',IPSMRY
      WRITE (*,*) ' VISCREEN results file is: ',IPLTUS
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) ' *******************'
999   STOP
      END
CDECK SZPAS
C
      FUNCTION SZPAS(I,X)
      SAVE
C
C   PASQUILL-GIFFORD VERTICAL DISPERSION COEFFICIENT (SIGMA Z).
C
      REAL A(7),B(7),C(7),D(7),LOGX
      DATA A/1.157,-.031027,-.0045741,.011157,-.0005092,.0037608,0./
      DATA B/2.815,.050674,.0040771,-.093465,-.10332,-.12889,0./
      DATA C/3.316,1.0827,.92084,.72583,.67969,.65602,0./
      DATA D/2.804,2.0327,1.7824,1.4901,1.3284,1.1391,0./

      IF(I.EQ.7) GO TO 1
      LOGX=ALOG10(X/1000.)
      SZPAS=10.**(A(I)*LOGX*LOGX*LOGX+B(I)*LOGX*LOGX+C(I)*LOGX+D(I))
      RETURN

 1    LOGX=ALOG10(X)
      SZPAS=10.**(-.0086351*LOGX*LOGX*LOGX-.036447*LOGX*LOGX+1.1243*LOGX
     1          -1.8981)                                                          PVT55040
      RETURN
      END
CDECK CHROMA
C
      SUBROUTINE CHROMA(SPECB,SPECR)
      SAVE
C*****
C***** CALCULATES VARIOUS COLORATION PARAMETERS SUCH AS CHROMA-
C***** TICITY COORDINATES,LUMINANCE,VALUE, AND DELTA E.
C*****
      COMMON/COLOR/DELAB,           
     1XBAR(3),YBAR(3),ZBAR(3)
      COMMON/REF/XCAP0,YCAP0,ZCAP0
      DIMENSION SPECB(3),SPECR(3)                                     

      XCAP=0.
      YCAP=0.
      ZCAP=0.
      XCAPR=0.
      YCAPR=0.
      ZCAPR=0.
      DO 10 I=1,3                                                     
        XCAP=XCAP+SPECB(I)*XBAR(I)
        YCAP=YCAP+SPECB(I)*YBAR(I)
        ZCAP=ZCAP+SPECB(I)*ZBAR(I)
        XCAPR=XCAPR+SPECR(I)*XBAR(I)
        YCAPR=YCAPR+SPECR(I)*YBAR(I)
        ZCAPR=ZCAPR+SPECR(I)*ZBAR(I)
 10   CONTINUE        
      VAL=116.*(YCAP/YCAP0)**.333-16.
      VALR=116.*(YCAPR/YCAP0)**.333-16.
      ASTAR=500.*((XCAP/XCAP0)**.333-(YCAP/YCAP0)**.333)
      ASTARR=500.*((XCAPR/XCAP0)**.333-(YCAPR/YCAP0)**.333)
      BSTAR=200.*((YCAP/YCAP0)**.333-(ZCAP/ZCAP0)**.333)
      BSTARR=200.*((YCAPR/YCAP0)**.333-(ZCAPR/ZCAP0)**.333)
      VALD=VAL-VALR
      ASTARD=ASTAR-ASTARR
      BSTARD=BSTAR-BSTARR
      DELAB=SQRT(VALD*VALD+ASTARD*ASTARD+BSTARD*BSTARD)
      RETURN
      END
CDECK BLOCK DATA
C
      BLOCK DATA
      COMMON/COLOR/DELAB,           
     1       XBAR(3),YBAR(3),ZBAR(3)
      COMMON/REF/XCAP0,YCAP0,ZCAP0

      DATA XBAR/0.1196, 0.6317, 0.1838/
      DATA YBAR/0.0935, 0.8229, 0.0753/
      DATA ZBAR/0.7012, 0.0159, 0.0000/
      DATA XCAP0,YCAP0,ZCAP0/3*0./
      END
CDECK INIT
C
      SUBROUTINE INIT (IERR)
      SAVE
C
C
C     SET VALUES FOR SOME COMMON BLOCK VARIABLES
C
C----------------------------------------------------------------------
C
      COMMON /IO/ ITERM,ISMRY,ILOTUS
      COMMON /COMR/ ALPHA(39), BABS,BEXT(3),
     &        CPLUME(3,2,39), DELCR(3,2,39), DIST, GAMMA,
     &        O3, P(3,2,9), PHI(39), RP(39), RV, TAU, X(39),
     &        U,OMEGA,XMIN,XMAX,DFINE,DCOARS,DPART,DSOOT,DSO4,
     &        SKYMAX(2),TERMAX(2),THRESH,CGREEN,
     &        QPARTI,QPART,QNOXI,QNOX,QNO2I,QNO2,QSOOTI,QSOOT,QSO4I,
     &        QSO4,RO(39),PRAY(2),PBACK(3,2),SCTANG(2),SKY(3,2),
     &        TERAIN(3,2,39),PLUSKY(3,2,39),PLUTER(3,2,39),DELSKY(2,39),
     &        DELTER(2,39),SPECB(3),SPECP(3),
     &        PSI(39),PERTHR(39),THRSKY(2,39),THRTER(2,39),RATIO,
     &        RATSKY(2),RATTER(2)
      COMMON /COMI/ ISIZE,ISTAB,ITHETA,IFINE,ICOARS,IPART,ISOOT,
     &         ISO4,ISKYMX(2),ITERMX(2),IANS,IEMISS,IDIST,IPAR,IMET,
     &         L1DFLT,LSCLAS(39),MXANG,MXLOS,IMASS,ITIME,
     &         LMDFLT,LPDFLT,LTDFLT
      COMMON /COMC/ MASS,TIME,SOURCE,RECEPT,CLASSI,OBJSKY,OBJTER
      CHARACTER*2 OBJSKY(2,39),OBJTER(2,39)
      CHARACTER*3 MASS(5),TIME(5)
      CHARACTER*7 CLASSI(2)
      CHARACTER*24 SOURCE,RECEPT
C
C----------------------------------------------------------------------------
C
      IERR = -1
C     ----- THESE SHOULD REALLY BE PARAMETERS
C
      ITERM = 1
      ISMRY = 7
      ILOTUS = 8
      MXANG = 2
      MXLOS = 39
C
      IEMISS = 1
      IDIST = 1
      IPAR = 1
      IMET = 1
C
C     ----- SET DEFAULT PARTICLES CHARACTERISTICS
C
      DFINE = 1.5
      IFINE = 3
      DCOARS = 2.5
      ICOARS = 8
      DPART = 2.5
      IPART = 6
      DSOOT = 2.0
      ISOOT = 1
      DSO4 = 1.5
      ISO4 = 4
C
C     ----- SET DEFAULT MET AND BACKGROUND OZONE
C
      O3 = 0.04
      U = 1.
      ISTAB = 6
      GAMMA = 11.25
C
      DO 10 I = 1, 2
         SKYMAX(I) = 0.
         TERMAX(I) = 0.
         RATSKY(I) = 0.
         RATTER(I) = 0.
10    CONTINUE
C
C     ----- SET DEFAULT THRESHOLDS AND SCATTER ANGLES
C
      THRESH = 2.0
      CGREEN = 0.05
      SCTANG(1) = 10.
      SCTANG(2) = 140.
C
C     ----- SET LABELS
C
      MASS(1) = 'G'
      MASS(2) = 'KG'
      MASS(3) = 'MT'
      MASS(4) = 'LB'
      MASS(5) = 'TON'
      TIME(1) = 'S'
      TIME(2) = 'MIN'
      TIME(3) = 'HR'
      TIME(4) = 'DAY'
      TIME(5) = 'YR'
C
      CLASSI(1) = 'INSIDE'
      CLASSI(2) = 'OUTSIDE'
C
      IERR = 0
C
C
C
999   RETURN
      END
CDECK INIT2
C
      SUBROUTINE INIT2 (IERR)
      SAVE
C
C
C     REINIT SOME IMPORTANT RESULTS ARRAYS
C
C----------------------------------------------------------------------
C
      COMMON /COMR/ ALPHA(39), BABS,BEXT(3),
     &        CPLUME(3,2,39), DELCR(3,2,39), DIST, GAMMA,
     &        O3, P(3,2,9), PHI(39), RP(39), RV, TAU, X(39),
     &        U,OMEGA,XMIN,XMAX,DFINE,DCOARS,DPART,DSOOT,DSO4,
     &        SKYMAX(2),TERMAX(2),THRESH,CGREEN,
     &        QPARTI,QPART,QNOXI,QNOX,QNO2I,QNO2,QSOOTI,QSOOT,QSO4I,
     &        QSO4,RO(39),PRAY(2),PBACK(3,2),SCTANG(2),SKY(3,2),
     &        TERAIN(3,2,39),PLUSKY(3,2,39),PLUTER(3,2,39),DELSKY(2,39),
     &        DELTER(2,39),SPECB(3),SPECP(3),
     &        PSI(39),PERTHR(39),THRSKY(2,39),THRTER(2,39),RATIO,
     &        RATSKY(2),RATTER(2)
      COMMON /CRGI/ LFIRST,ISCYMX(2),RATSCY(2),ITRCMX(2),RATTRC(2)
C
C----------------------------------------------------------------------------
C
      IERR = -1
C
      DO 10 I = 1, 2
         SKYMAX(I) = -1.
         TERMAX(I) = -1.
         RATSKY(I) = -1.
         RATTER(I) = -1.
         RATSCY(I) = -1.
         RATTRC(I) = -1.
10    CONTINUE
C
      IERR = 0
C
C
999   RETURN
      END
CDECK OPENA
C
      SUBROUTINE OPENA (IERR)
      SAVE
C
C  OPEN OUTPUT FILES FOR VISCREEN PROGRAM
C
C************************************************************************
C
      COMMON /IFL/ IPSMRY,IPLTUS
      COMMON /IO/ ITERM,ISMRY,ILOTUS
      LOGICAL LFLAG
      CHARACTER*40 IPSMRY,IPLTUS
C   IVAL never referenced - pae 7/5/13
C      CHARACTER*2 IVAL
      CHARACTER*40 CSTR
C
C**************************************************************************
C
      IERR = -1
C
C     ----- SUMMARY REPORT FILE
C
100   CONTINUE
      WRITE (*,*) 'Path & file name for Summary Report'
      WRITE (*,'(A,A\)') ' (max 40 characters including file name &',
     &                    ' extension): '
      READ (*,1000) IPSMRY
      INQUIRE (FILE=IPSMRY,EXIST=LFLAG)
      IF (LFLAG) THEN
         WRITE (*,'(A\)') ' File Exists, do you want to overwrite it? '
         CALL RESPND (1,IYES,RNUM,CSTR,IERR)
         IF (IYES.EQ.0) GO TO 100
         OPEN (ISMRY,FILE=IPSMRY,STATUS='OLD')
         ENDFILE ISMRY
         REWIND ISMRY
      ELSE
         OPEN (ISMRY,FILE=IPSMRY,STATUS='NEW')
      ENDIF
C
C     ----- INPUT AND LOTUS FORMAT RESULTS FILE
C
200   CONTINUE
      WRITE (*,*) ' '
      WRITE (*,*) 'Path & file name for Results Output'
      WRITE (*,'(A,A\)') ' (max 40 characters including file name & ',
     &                       'extension): '
      READ (*,1000) IPLTUS
      INQUIRE (FILE=IPLTUS,EXIST=LFLAG)
      IF (LFLAG) THEN
         WRITE (*,'(A\)') ' File Exists, do you want to overwrite it? '
         CALL RESPND (1,IYES,RNUM,CSTR,IERR)
         IF (IYES.EQ.0) GO TO 200
         OPEN (ILOTUS,FILE=IPLTUS,STATUS='OLD')
         ENDFILE ILOTUS
         REWIND ILOTUS
      ELSE
         OPEN (ILOTUS,FILE=IPLTUS,STATUS='NEW')
      ENDIF
C
      IERR = 0
C
C
C
      RETURN
1000  FORMAT (40A)
      END
CDECK RESPND
C
      SUBROUTINE RESPND (IFLAG,INUM,RNUM,CSTR,IERR)
      SAVE
C
C
C     ----- GET USER RESPONSE FROM THE TERMINAL
C           TRAPPING ERRORS AS WE GO
C
C     ARGUMENTS:
C        IFLAG  I  RESPONSE TYPE FLAG
C                  = 1  YES/NO
C                  = 2  INTEGER
C                  = 3  REAL
C                  = 4  CHARACTER STRING
C                  = 5  QUITTING ?
C        INUM   I  INTEGER RETURNED ARG FOR IFLAG = 0,1
C                  NUMERIC INTEGER OR
C                  = 0  NO  FOR IFLAG = 0
C                  = 1  YES
C                  = 2  QUIT, YES I WANT TO
C        RNUM   R  REAL RETURNED ARG
C        CSTR   C  CHARACTER RETURNED ARG
C        IERR   I  ERROR FLAG RETURNED
C                  = 0 OKEY-DOKEY
C                  < 0 OPPS OF SOME KIND
C
C**********************************************************************
C
      COMMON /IO/ ITERM,ISMRY,ILOTUS
C
      REAL RNUM
      INTEGER INUM,IFLAG,IERR,ICNT,MXERR
      CHARACTER CSTR*40,YESNO*3
C
      DATA MXERR / 3 /
C
C**********************************************************************
C
C     ------ BRANCH ON RESPONSE TYPE WANTED
C
      ICNT = 0
      IERR = -1
      IF (IFLAG.EQ.1) GO TO 100
      IF (IFLAG.EQ.2) GO TO 200
      IF (IFLAG.EQ.3) GO TO 300
      IF (IFLAG.EQ.4) GO TO 400
      IF (IFLAG.EQ.5) GO TO 500
      GO TO 999
C
C      _____ YESNO ANSWER
C
100   CONTINUE
      INUM = 0
      READ (*,'(3A)',ERR=910) YESNO
      IF (YESNO(1:1).EQ.'Y' .OR. YESNO(1:1).EQ.'y') THEN
         INUM = 1
         IERR = 0
         GO TO 999
      ELSE
         IF (YESNO(1:1).EQ.'N' .OR. YESNO(1:1).EQ.'n') THEN
            INUM = 0
            IERR = 0
            GO TO 999
         ENDIF
      ENDIF
      GO TO 910
C
C     ----- INTEGER RESPONSE
C
200   CONTINUE
      READ (*,*,ERR=920) INUM
      IERR = 0
      GO TO 999
C
C     ----- REAL RESPONSE
C
300   CONTINUE
      READ (*,*,ERR=930) RNUM
      IERR = 0
      GO TO 999
C
C     ----- CHARACTER STRING INPUT
C
400   CONTINUE
      READ (*,'(40A)',ERR=940) CSTR
      IERR = 0
      GO TO 999
C
C      _____ IS HE A QUITER
C
500   CONTINUE
      INUM = 0
      READ (*,'(3A)',ERR=910) YESNO
      IF (YESNO(1:1).EQ.'Q' .OR. YESNO(1:1).EQ.'q') THEN
         INUM = 2
         IERR = 0
         GO TO 999
      ENDIF
      IERR = 0
      GO TO 999
C
C     ----- ERROR MESSAGES
C
910   ICNT = ICNT + 1
      IF (ICNT.GT.MXERR) GO TO 999
      WRITE (*,'(A\)') ' Please answer Y or N: '
      GO TO 100
C
920   ICNT = ICNT + 1
      IF (ICNT.GT.MXERR) GO TO 999
      WRITE (*,'(A\)') ' Error...Please re-enter number: '
      GO TO 200
C
930   ICNT = ICNT + 1
      IF (ICNT.GT.MXERR) GO TO 999
      WRITE (*,'(A\)') ' Error...Please re-enter number: '
      GO TO 300
C
940   ICNT = ICNT + 1
      IF (ICNT.GT.MXERR) GO TO 999
      WRITE (*,'(A\)') ' Illegal character string, Try again: '
      GO TO 400
C
950   ICNT = ICNT + 1
      IF (ICNT.GT.MXERR) GO TO 999
      WRITE (*,'(A\)') ' Enter Q to quit: '
      GO TO 400
999   RETURN
      END
CDECK SMRPT1
C
      SUBROUTINE SMRPT1 (IERR)
      SAVE

C     ----- SUBROUTINE OUTPUTS HEADER & INPUT RECORDS FOR
C            THE SUMMARY REPORT
C
C********************************************************************
C
      COMMON /IO/ ITERM,ISMRY,ILOTUS
      COMMON /COMR/ ALPHA(39), BABS,BEXT(3),
     &        CPLUME(3,2,39), DELCR(3,2,39), DIST, GAMMA,
     &        O3, P(3,2,9), PHI(39), RP(39), RV, TAU, X(39),
     &        U,OMEGA,XMIN,XMAX,DFINE,DCOARS,DPART,DSOOT,DSO4,
     &        SKYMAX(2),TERMAX(2),THRESH,CGREEN,
     &        QPARTI,QPART,QNOXI,QNOX,QNO2I,QNO2,QSOOTI,QSOOT,QSO4I,
     &        QSO4,RO(39),PRAY(2),PBACK(3,2),SCTANG(2),SKY(3,2),
     &        TERAIN(3,2,39),PLUSKY(3,2,39),PLUTER(3,2,39),DELSKY(2,39),
     &        DELTER(2,39),SPECB(3),SPECP(3),
     &        PSI(39),PERTHR(39),THRSKY(2,39),THRTER(2,39),RATIO,
     &        RATSKY(2),RATTER(2)
      COMMON /COMI/ ISIZE,ISTAB,ITHETA,IFINE,ICOARS,IPART,ISOOT,
     &         ISO4,ISKYMX(2),ITERMX(2),IANS,IEMISS,IDIST,IPAR,IMET,
     &         L1DFLT,LSCLAS(39),MXANG,MXLOS,IMASS,ITIME,
     &         LMDFLT,LPDFLT,LTDFLT
      COMMON /COMC/ MASS,TIME,SOURCE,RECEPT,CLASSI,OBJSKY,OBJTER
      CHARACTER*2 OBJSKY(2,39),OBJTER(2,39)
      CHARACTER*3 MASS(5),TIME(5)
      CHARACTER*7 CLASSI(2)
      CHARACTER*24 SOURCE,RECEPT
C
C****************************************************************************
C
C      ----- TABLE A --------
C
      WRITE (ISMRY,1000) SOURCE,RECEPT
1000  FORMAT (/15X,'Visual Effects Screening Analysis for'/
     &        15X,'  Source: ',A/
     &        15X,'  Class I Area: ',A)

      IF (L1DFLT.EQ.1) THEN
         WRITE (ISMRY,1001)
      ELSE
         WRITE (ISMRY,1002)
      ENDIF
1001  FORMAT (//15X,'  ***   Level-1 Screening   ***')
1002  FORMAT (//15X,'*** User-selected Screening Scenario Results ***')
C
C     ----- TABLE B -------------
C
      WRITE (ISMRY,*)'Input Emissions for '
      WRITE (ISMRY,1010) QPARTI,MASS(IMASS),TIME(ITIME),
     &                   QNOXI,MASS(IMASS),TIME(ITIME),
     &                   QNO2I,MASS(IMASS),TIME(ITIME),
     &                   QSOOTI,MASS(IMASS),TIME(ITIME),
     &                   QSO4I,MASS(IMASS),TIME(ITIME)
1010  FORMAT (/'    Particulates ',F8.2,2X,A3,'/',A3/
     &        '    NOx (as NO2) ',F8.2,2X,A3,'/',A3/
     &        '    Primary NO2  ',F8.2,2X,A3,'/',A3/
     &        '    Soot         ',F8.2,2X,A3,'/',A3/
     &        '    Primary SO4  ',F8.2,2X,A3,'/',A3)
      WRITE (ISMRY,*)' '
C
      IF (LPDFLT.EQ.1) THEN
         WRITE (ISMRY,1003)
1003     FORMAT (/5X, '**** Default Particle Characteristics Assumed')
      ELSE
         WRITE (ISMRY,1004)
1004     FORMAT (/15X,'PARTICLE CHARACTERISTICS'/
     &           15X,'Density',7X,'Diameter'/
     &           15X,'=======',7X,'========')
         WRITE (ISMRY,1005) DPART,IPART,DSOOT,ISOOT,DSO4,ISO4
1005     FORMAT (1X,'Primary Part.  ',F6.1,7X,I6/
     &           1X,'Soot           ',F6.1,7X,I6/
     &           1X,'Sulfate        ',F6.1,7X,I6)
      ENDIF
C
C     ---------- TABLE C ---------------------
C
      WRITE (ISMRY,1006)
1006  FORMAT (/15X,'Transport Scenario Specifications:'/)
      WRITE (ISMRY,1007) O3,RV,DIST,XMIN,XMAX,GAMMA
1007  FORMAT (5X,'Background Ozone:             ',F8.2,' ppm'/
     &        5X,'Background Visual Range:      ',F8.2,' km'/
     &        5X,'Source-Observer Distance:     ',F8.2,' km'/
     &        5X,'Min. Source-Class I Distance: ',F8.2,' km'/
     &        5X,'Max. Source-Class I Distance: ',F8.2,' km'/
     &        5X,'Plume-Source-Observer Angle:  ',F8.2,' degrees')
C
      WRITE (ISMRY,1008) ISTAB,U
1008  FORMAT (5X,'Stability: ',I3/
     &        5X,'Wind Speed: ',F6.2,' m/s')

C
      IERR = 0
999   RETURN
      END
CDECK SMRPT2
C
      SUBROUTINE SMRPT2 (IERR)
      SAVE

C     ----- SUBROUTINE OUTPUTS RESULT SUMMARY TO
C            THE SUMMARY REPORT
C
C********************************************************************
C
      COMMON /IO/ ITERM,ISMRY,ILOTUS
      COMMON /COMR/ ALPHA(39), BABS,BEXT(3),
     &        CPLUME(3,2,39), DELCR(3,2,39), DIST, GAMMA,
     &        O3, P(3,2,9), PHI(39), RP(39), RV, TAU, X(39),
     &        U,OMEGA,XMIN,XMAX,DFINE,DCOARS,DPART,DSOOT,DSO4,
     &        SKYMAX(2),TERMAX(2),THRESH,CGREEN,
     &        QPARTI,QPART,QNOXI,QNOX,QNO2I,QNO2,QSOOTI,QSOOT,QSO4I,
     &        QSO4,RO(39),PRAY(2),PBACK(3,2),SCTANG(2),SKY(3,2),
     &        TERAIN(3,2,39),PLUSKY(3,2,39),PLUTER(3,2,39),DELSKY(2,39),
     &        DELTER(2,39),SPECB(3),SPECP(3),
     &        PSI(39),PERTHR(39),THRSKY(2,39),THRTER(2,39),RATIO,
     &        RATSKY(2),RATTER(2)
      COMMON /COMI/ ISIZE,ISTAB,ITHETA,IFINE,ICOARS,IPART,ISOOT,
     &         ISO4,ISKYMX(2),ITERMX(2),IANS,IEMISS,IDIST,IPAR,IMET,
     &         L1DFLT,LSCLAS(39),MXANG,MXLOS,IMASS,ITIME,
     &         LMDFLT,LPDFLT,LTDFLT
      COMMON /COMC/ MASS,TIME,SOURCE,RECEPT,CLASSI,OBJSKY,OBJTER
      CHARACTER*2 OBJSKY(2,39),OBJTER(2,39)
      CHARACTER*3 MASS(5),TIME(5)
      CHARACTER*7 CLASSI(2),IBACK(2)
      CHARACTER*24 SOURCE,RECEPT
C
      DATA IBACK / 'SKY    ','TERRAIN'/
C****************************************************************************
C
      IERR = -1
C
       WRITE (ISMRY,1000)
1000   FORMAT (/28X,'R E S U L T S'//1X,'Asterisks (*) indicate',
     &         ' plume impacts that exceed screening criteria')
c
C      ----- TABLE D RESULTS WITHIN CLASS I AREA --------
C
      ICLASS = 1
      ISMAX = ISKYMX(ICLASS)
      WRITE (ISMRY,1001) CLASSI(ICLASS)
1001  FORMAT (/10X,'Maximum Visual Impacts ',A,' Class I Area')
C
C     ----- PASS OR FAIL ?
C
      IF ((SKYMAX(ICLASS).GT.THRESH).AND.(RATSKY(ICLASS).GT.1.)) THEN
        WRITE (ISMRY,*) '            Screening Criteria ARE Exceeded'
      ELSE
        IF ((TERMAX(ICLASS).GT.THRESH).AND.(RATTER(ICLASS).GT.1.)) THEN
         WRITE (ISMRY,*) '             Screening Criteria ARE Exceeded'
        ELSE
         WRITE (ISMRY,*) '          Screening Criteria ARE NOT Exceeded'
        ENDIF
      ENDIF
      WRITE (ISMRY,1002)
1002  FORMAT (37X,'Delta E',7X,'Contrast'/35X,'===========',
     &        3X,'============'/1X,'Backgrnd',1X,'Theta',1X,
     &        'Azi',1X,'Distance',1X,'Alpha',1X,'Crit',2X,'Plume',3X,
     &        'Crit',2X,'Plume')
      WRITE (ISMRY,1005)
1005  FORMAT (1X,'========',1X,'=====',1X,
     &        '===',1X,'========',1X,'=====',1X,'====',2X,'=====',3X,
     &        '====',2X,'=====')
C
C    ------------ SKY  ------------
C
      DO 100 IS = 1, 2
         WRITE (ISMRY,1003) IBACK(1),SCTANG(IS),PHI(ISMAX),X(ISMAX),
     &        ALPHA(ISMAX),THRSKY(IS,ISMAX),DELSKY(IS,ISMAX),
     &        OBJSKY(IS,ISMAX)(1:1),PERTHR(ISMAX),CPLUME(2,IS,ISMAX),
     &        OBJSKY(IS,ISMAX)(2:2)
1003    FORMAT (2X,A7,1X,F4.0,1X,F4.0,2X,F5.1,3X,F4.0,1X,F5.2,1X,F6.3,
     &          A1,1X,F5.2,1X,F6.3,A1)
100   CONTINUE
C
C     ---------- TERRAIN  --------------------
C
      ITMAX = ITERMX(ICLASS)
      DO 101 IS = 1, 2
         WRITE (ISMRY,1003) IBACK(2),SCTANG(IS),PHI(ITMAX),X(ITMAX),
     &        ALPHA(ITMAX),THRTER(IS,ITMAX),DELTER(IS,ITMAX),
     &        OBJTER(IS,ITMAX)(1:1),PERTHR(ITMAX),DELCR(2,IS,ITMAX),
     &        OBJTER(IS,ITMAX)(2:2)
101   CONTINUE
C
      WRITE (ISMRY,*) ' '
C
C      ----- TABLE E RESULTS OUTSIDE CLASS I AREA --------
C
      ICLASS = 2
      ISMAX = ISKYMX(ICLASS)
      WRITE (ISMRY,1001) CLASSI(ICLASS)
C
C     ----- PASS OR FAIL ?
C
      IF ((SKYMAX(ICLASS).GT.THRESH).AND.(RATSKY(ICLASS).GT.1.)) THEN
        WRITE (ISMRY,*) '            Screening Criteria ARE Exceeded'
      ELSE
        IF ((TERMAX(ICLASS).GT.THRESH).AND.(RATTER(ICLASS).GT.1.)) THEN
         WRITE (ISMRY,*) '             Screening Criteria ARE Exceeded'
        ELSE
         WRITE (ISMRY,*) '          Screening Criteria ARE NOT Exceeded'
        ENDIF
      ENDIF
      WRITE (ISMRY,1002)
      WRITE (ISMRY,1005)
C
C    ------------ SKY  ------------
C
      DO 200 IS = 1, 2
         WRITE (ISMRY,1003) IBACK(1),SCTANG(IS),PHI(ISMAX),X(ISMAX),
     &        ALPHA(ISMAX),THRSKY(IS,ISMAX),DELSKY(IS,ISMAX),
     &        OBJSKY(IS,ISMAX)(1:1),PERTHR(ISMAX),CPLUME(2,IS,ISMAX),
     &        OBJSKY(IS,ISMAX)(2:2)
200   CONTINUE
C
C     ---------- TERRAIN  --------------------
C
      ITMAX = ITERMX(ICLASS)
      DO 201 IS = 1, 2
         WRITE (ISMRY,1003) IBACK(2),SCTANG(IS),PHI(ITMAX),X(ITMAX),
     &        ALPHA(ITMAX),THRTER(IS,ITMAX),DELTER(IS,ITMAX),
     &        OBJTER(IS,ITMAX)(1:1),PERTHR(ITMAX),DELCR(2,IS,ITMAX),
     &        OBJTER(IS,ITMAX)(2:2)
201   CONTINUE
C
C
      IERR = 0
999   RETURN
      END
CDECK WINPTS
C
      SUBROUTINE WINPTS (IERR)
      SAVE
C
C     ----- SUBROUTINE WRITES INPUTS TO HEADER OF
C            THE FULL RESULTS FILE
C
C********************************************************************
C
      COMMON /IO/ ITERM,ISMRY,ILOTUS
      COMMON /COMR/ ALPHA(39), BABS,BEXT(3),
     &        CPLUME(3,2,39), DELCR(3,2,39), DIST, GAMMA,
     &        O3, P(3,2,9), PHI(39), RP(39), RV, TAU, X(39),
     &        U,OMEGA,XMIN,XMAX,DFINE,DCOARS,DPART,DSOOT,DSO4,
     &        SKYMAX(2),TERMAX(2),THRESH,CGREEN,
     &        QPARTI,QPART,QNOXI,QNOX,QNO2I,QNO2,QSOOTI,QSOOT,QSO4I,
     &        QSO4,RO(39),PRAY(2),PBACK(3,2),SCTANG(2),SKY(3,2),
     &        TERAIN(3,2,39),PLUSKY(3,2,39),PLUTER(3,2,39),DELSKY(2,39),
     &        DELTER(2,39),SPECB(3),SPECP(3),
     &        PSI(39),PERTHR(39),THRSKY(2,39),THRTER(2,39),RATIO,
     &        RATSKY(2),RATTER(2)
      COMMON /COMI/ ISIZE,ISTAB,ITHETA,IFINE,ICOARS,IPART,ISOOT,
     &         ISO4,ISKYMX(2),ITERMX(2),IANS,IEMISS,IDIST,IPAR,IMET,
     &         L1DFLT,LSCLAS(39),MXANG,MXLOS,IMASS,ITIME,
     &         LMDFLT,LPDFLT,LTDFLT
      COMMON /COMC/ MASS,TIME,SOURCE,RECEPT,CLASSI,OBJSKY,OBJTER
      CHARACTER*2 OBJSKY(2,39),OBJTER(2,39)
      CHARACTER*3 MASS(5),TIME(5)
      CHARACTER*7 CLASSI(2)
      CHARACTER*24 SOURCE,RECEPT
      CHARACTER*1 IQUOT

      DATA IQUOT / '"' /
C
C****************************************************************************
C
      IERR = -1
C
      WRITE (ILOTUS,1000) IQUOT,SOURCE,IQUOT,IQUOT,RECEPT,IQUOT,
     &                     IMASS,ITIME
1000  FORMAT (A,A,A/A,A,A/2I5)
C
C     ----- EMISSIONS
C
      WRITE (ILOTUS,1001) QPARTI,QNOXI,QNO2I,QSOOTI,QSO4I
1001  FORMAT (8F10.3)
C
C     ----- GEOMETRY 1
C
      WRITE (ILOTUS,1001) DIST,XMIN,XMAX,RV
C
C     ----- PARTICLE CHARACTERISTICS
C
      WRITE (ILOTUS,1002) L1DFLT,DFINE,IFINE,
     &                    L1DFLT,DCOARS,ICOARS,
     &                    L1DFLT,DPART,IPART,
     &                    L1DFLT,DSOOT,ISOOT,
     &                    L1DFLT,DSO4,ISO4
1002  FORMAT (I5,F10.3,I5)
C
C     ----- BACKGROUND OZONE AND MET
C
      WRITE (ILOTUS,1003) L1DFLT,O3,U,ISTAB
1003  FORMAT (I5,2F10.3,I5)
C
C     ----- PLUME OFFSET ANGLE
C
      WRITE (ILOTUS,1003) L1DFLT,GAMMA
C
C
      IERR = 0
999   RETURN
      END
CDECK WRESLT
C
      SUBROUTINE WRESLT (NLOS,IERR)
      SAVE
C
C     ----- SUBROUTINE WRITES RESULTS TO
C            THE FULL RESULTS FILE
C
C********************************************************************
C
      COMMON /IO/ ITERM,ISMRY,ILOTUS
      COMMON /COMR/ ALPHA(39), BABS,BEXT(3),
     &        CPLUME(3,2,39), DELCR(3,2,39), DIST, GAMMA,
     &        O3, P(3,2,9), PHI(39), RP(39), RV, TAU, X(39),
     &        U,OMEGA,XMIN,XMAX,DFINE,DCOARS,DPART,DSOOT,DSO4,
     &        SKYMAX(2),TERMAX(2),THRESH,CGREEN,
     &        QPARTI,QPART,QNOXI,QNOX,QNO2I,QNO2,QSOOTI,QSOOT,QSO4I,
     &        QSO4,RO(39),PRAY(2),PBACK(3,2),SCTANG(2),SKY(3,2),
     &        TERAIN(3,2,39),PLUSKY(3,2,39),PLUTER(3,2,39),DELSKY(2,39),
     &        DELTER(2,39),SPECB(3),SPECP(3),
     &        PSI(39),PERTHR(39),THRSKY(2,39),THRTER(2,39),RATIO,
     &        RATSKY(2),RATTER(2)
      COMMON /COMI/ ISIZE,ISTAB,ITHETA,IFINE,ICOARS,IPART,ISOOT,
     &         ISO4,ISKYMX(2),ITERMX(2),IANS,IEMISS,IDIST,IPAR,IMET,
     &         L1DFLT,LSCLAS(39),MXANG,MXLOS,IMASS,ITIME,
     &         LMDFLT,LPDFLT,LTDFLT
      COMMON /COMC/ MASS,TIME,SOURCE,RECEPT,CLASSI,OBJSKY,OBJTER
      CHARACTER*2 OBJSKY(2,39),OBJTER(2,39)
      CHARACTER*3 MASS(5),TIME(5)
      CHARACTER*7 CLASSI(2)
      CHARACTER*24 SOURCE,RECEPT
C
      INTEGER IC
C
C****************************************************************************
C
      IERR = -1
      WRITE (ILOTUS,1000) NLOS
      DO 100 LOS = 1, NLOS
        IC = LSCLAS(LOS)
        IF (IC.EQ.2) IC = 0
        WRITE (ILOTUS,1000) LOS,IC,PHI(LOS),ALPHA(LOS),X(LOS),
     &        RP(LOS),RO(LOS),PSI(LOS),PERTHR(LOS),
     &        (THRSKY(I,LOS),DELSKY(I,LOS),I=1,MXANG),
     &        (THRTER(I,LOS),DELTER(I,LOS),I=1,MXANG)
1000    FORMAT (1X,I2,I2,F8.1,4F7.1,F5.2,F7.3,10F7.2)
100   CONTINUE
C
C     ----- WRITE OUT CONTRAST ORDERED GREEN,BLUE,RED
C
      WRITE (ILOTUS,1000) NLOS
      DO 200 LOS = 1, NLOS
        IC = LSCLAS(LOS)
        IF (IC.EQ.2) IC = 0
        WRITE (ILOTUS,1001) LOS,IC,PHI(LOS),PERTHR(LOS),
     &                      (CPLUME(2,J,LOS),DELCR(2,J,LOS),J=1,MXANG),
     &                      (CPLUME(1,J,LOS),DELCR(1,J,LOS),J=1,MXANG),
     &                      (CPLUME(3,J,LOS),DELCR(3,J,LOS),J=1,MXANG)
1001    FORMAT (1X,I2,I2,F8.3,15F7.3)
200   CONTINUE

C
C
      IERR = 0
999   RETURN
      END
