CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCVVG00010
      SUBROUTINE VVGTU1                                                 VVG00020
C                                                                       VVG00030
C  VVGTU1.FOR                                                           VVG00040
C                                                                       VVG00050
C  SETS UP COEFFICIENTS FOR CONVERTING GEODETIC TO                      VVG00060
C  RECTIFYING LATITUDE AND CONVERSELY                                   VVG00070
C                                                                       VVG00080
C         A1-A4 COEFFICIENTS FOR CONVERTING GEODITIC TO RECTIFYING LATITVVG00090
C         A5    FALSE EASTING                                           VVG00100
C         A6    FALSE NORTHING                                          VVG00110
C         A8    SCALE FACTOR AT CENTRAL MERIDIAN                        VVG00120
C         A9    CENTRAL MERIDIAN IN SECONDS                             VVG00130
C         A10   RADIUS OF SPHERE HAVING GREAT CIRCLE LENGTH EQUAL TO    VVG00140
C               SPHEROID MERIDIAN LENGTH                                VVG00150
C         A11-A14  COEFFICIENTS FOR CONVERTING RECTIFYING LATITUDE TO   VVG00160
C                  GEODETIC LATITUDE                                    VVG00170
C         A15   SEMIMAJOR AXIS OF SPHERIOD                              VVG00180
C         A16   ECCENTRICITY SQUARED                                    VVG00190
C                                                                       VVG00200
C  PLUVUE II                                                            VVG00210
C  Version 92243   Level 1.0                                            VVG00220
C                                                                       VVG00230
C  U.S. Environmental Protection Agency                                 VVG00240
C  Office of Air Quality Planning and Standards                         VVG00250
C  Research Triangle Park, North Carolina                               VVG00260
C                                                                       VVG00270
C  Modifications:                                                       VVG00280
C                                                                       VVG00290
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCVVG00300
      SAVE                                                              VVG00310
C                                                                       VVG00320
      COMMON /VVUTM/ A(16)                                              VVG00330
      DATA IFRST/1/                                                     VVG00340
C                                                                       VVG00350
C     *** CONPUTE COEFFICIENTS ON FIRST PASS ONLY                       VVG00360
      IF (IFRST .NE. 1) RETURN                                          VVG00370
      IFRST = 0                                                         VVG00380
C                                                                       VVG00390
      A(5)=5.0E5                                                        VVG00400
      A(6)=0.0                                                          VVG00410
      A(7)=0.0                                                          VVG00420
      A(8)=0.9996                                                       VVG00430
C                                                                       VVG00440
C     *** CLARKE 1866 ELLIPSOID AXES LENGTHS                            VVG00450
      A(15)=6378206.4                                                   VVG00460
      B = 6356583.80                                                    VVG00470
C                                                                       VVG00480
      A(16)= ((A(15)-B)/A(15))*((A(15)+B)/A(15))                        VVG00490
      A(10)=(((A(16)*(7.0/3.2E1)+(5.0/1.6E1))*A(16)+0.5)*A(16)          VVG00500
     1 +1.0)*A(16)*0.25                                                 VVG00510
      A(1)=-(((A(10)*(1.95E2/6.4E1)+3.25)*A(10)+3.75)*A(10)+3.0)*       VVG00520
     1 A(10)                                                            VVG00530
      A(2)=(((1.455E3/3.2E1)*A(10)+(7.0E1/3.0))*A(10)+7.5)*A(10)**2     VVG00540
      A(3)=-((7.0E1/3.0)+A(10)*(9.45E2/8.0))*A(10)**3                   VVG00550
      A(4)=(3.15E2/4.0)*A(10)**4                                        VVG00560
      A(11)=(((7.75-(6.57E2/6.4E1)*A(10))*A(10)-5.25)*A(10)+3.0)*       VVG00570
     1 A(10)                                                            VVG00580
      A(12)=(((5.045E3/3.2E1)*A(10)-(1.51E2/3.0))*A(10)+10.5)*          VVG00590
     1 A(10)**2                                                         VVG00600
      A(13)=((1.51E2/3.0)-(3.291E3/8.0)*A(10))*A(10)**3                 VVG00610
      A(14)=(1.097E3/4.0)*A(10)**4                                      VVG00620
      FAC=A(10)*A(10)                                                   VVG00630
      A(10)=(((2.25E2/6.4E1)*FAC+2.25)*FAC+1.0)*(1.0-FAC)*              VVG00640
     1 (1.0-A(10))*A(15)                                                VVG00650
      RETURN                                                            VVG00660
      END                                                               VVG00670
