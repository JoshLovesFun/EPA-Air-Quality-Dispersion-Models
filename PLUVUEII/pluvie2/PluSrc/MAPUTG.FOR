CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMUT00010
      SUBROUTINE MAPUTG (UTMX,UTMY,IZONE,ALON,ALAT)                     MUT00020
C                                                                       MUT00030
C   MAPUTG.FOR                                                          MUT00040
C                                                                       MUT00050
C   CONVERTS UTM COORDINATES TO LATITUDE AND LONGITUDE                  MUT00060
C   THIS IS A MODIFICATION OF THE US GEOLOGICAL SURVEY                  MUT00070
C   PROGRAM NO J380  (TOPOGRAPHIC DIVISION)                             MUT00080
C   GW LUNDBERG/SAI  APR 79                                             MUT00090
C                                                                       MUT00100
C         UTMX    EASTING IN KILOMETERS                                 MUT00110
C         UTMY    NORTHING IN KILOMETERS                                MUT00120
C         IZONE   UTM ZONE NUMBER                                       MUT00130
C         ALON    LONGITUDE IN DEGREES                                  MUT00140
C         ALAT    LATITUDE IN DEGREES                                   MUT00150
C                                                                       MUT00160
C  PLUVUE II                                                            MUT00170
C  Version 92243   Level 1.0                                            MUT00180
C                                                                       MUT00190
C  U.S. Environmental Protection Agency                                 MUT00200
C  Office of Air Quality Planning and Standards                         MUT00210
C  Research Triangle Park, North Carolina                               MUT00220
C                                                                       MUT00230
C  Modifications:                                                       MUT00240
C                                                                       MUT00250
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMUT00260
      SAVE                                                              MUT00270
C                                                                       MUT00280
C                                                                       MUT00290
      DIMENSION B(12)                                                   MUT00300
      COMMON /VVUTM/ A(16)                                              MUT00310
C                                                                       MUT00320
      X = UTMX*1000.                                                    MUT00330
      Y = UTMY*1000.                                                    MUT00340
C                                                                       MUT00350
C     *** COMPUTE CONVERSION COEFFICIENTS IF NEEDED                     MUT00360
      CALL VVGTU1                                                       MUT00370
C                                                                       MUT00380
C     *** COMPUTE CENTRAL MERIDIAN IN SECONDS FROM IZONE INPUT          MUT00390
   10 UTZ = 30.0 - FLOAT(IABS (IZONE))                                  MUT00400
      A(9) = ((UTZ*6.0) + 3.0) * 3600.0                                 MUT00410
C                                                                       MUT00420
C     *** CONVERT UTM COORDINATES TO GEODETIC                           MUT00430
      B(9)=((A(5)-X)*1.0E-6)/A(8)                                       MUT00440
      IF (ABS(B(9))-1.0E-7*A(15)*2.0)30,30,20                           MUT00450
   20 SLAT=0.0                                                          MUT00460
      SLON=0.0                                                          MUT00470
      GO TO 99                                                          MUT00480
   30 B(10)=((Y-A(6))/A(8)+A(7))/A(10)                                  MUT00490
      IF(ABS(B(10))-1.47E0)40,40,20                                     MUT00500
   40 SINW=SIN(B(10))                                                   MUT00510
      COSW=COS(B(10))                                                   MUT00520
      B(12)=COSW*COSW                                                   MUT00530
      B(11)=(((A(14)*B(12)+A(13))*B(12)+A(12))*B(12)+A(11))*SINW*COSW   MUT00540
     1 +B(10)                                                           MUT00550
      SINW=SIN(B(11))                                                   MUT00560
      COSW=COS(B(11))                                                   MUT00570
      RN=SQRT(1.0-A(16)*SINW*SINW)*1.0E6/A(15)                          MUT00580
      T=SINW/COSW                                                       MUT00590
      TS=T*T                                                            MUT00600
      B(12)=COSW*COSW                                                   MUT00610
      ETAS=A(16)*B(12)/(1.0-A(16))                                      MUT00620
      B(1)=RN/COSW                                                      MUT00630
      B(2)=-T*(1.0+ETAS)*RN*RN/2.0                                      MUT00640
      B(3)=-(1.0+2.0*TS+ETAS)*B(1)*RN*RN/6.0                            MUT00650
      B(4)=(((-6.0-ETAS*9.0)*ETAS+3.0)*TS+(6.0-ETAS*3.0)*ETAS           MUT00660
     1 +5.0)*T*RN**4/24.0                                               MUT00670
      B(5)=((TS*24.0+ETAS*8.0+28.0)*TS+ETAS*6.0+5.0)*B(1)*              MUT00680
     1 RN**4/120.0                                                      MUT00690
      B(6)=(((ETAS*45.0-45.0)*TS+ETAS*162.0-90.0)*TS                    MUT00700
     1 -ETAS*107.0-61.0)*T*RN**6/720.0                                  MUT00710
      B(7)=-(((TS*720.0+1320.0)*TS+662.0)*TS+61.0)*B(1)*RN**6/          MUT00720
     1 5040.0                                                           MUT00730
      B(8)=(((TS*1575.0+4095.0)*TS+3633.0)*TS+1385.0)*T*RN**8/          MUT00740
     1 40320.0                                                          MUT00750
      B(10)=B(9)*B(9)                                                   MUT00760
      SLAT=((((B(8)*B(10)+B(6))*B(10)+B(4))*B(10)+B(2))*B(10)+B(11))*   MUT00770
     1 206264.80624709                                                  MUT00780
      SLON= (((B(7)*B(10)+B(5))*B(10)+B(3))*B(10)+B(1))*B(9)*           MUT00790
     1 206264.80624709 + A(9)                                           MUT00800
C                                                                       MUT00810
C     *** CONVERT SECONDS OF ARC TO DEGREES AND RETURN                  MUT00820
   99 ALAT = SLAT/3600.                                                 MUT00830
      ALON = -SLON/3600.                                                MUT00840
C                                                                       MUT00850
      RETURN                                                            MUT00860
      END                                                               MUT00870
