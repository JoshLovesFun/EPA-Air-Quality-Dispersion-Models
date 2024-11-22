C***********************************************************************PRE00005
C                                                                       PRE00006
C                        PRERTDM3.2 (DATED 89226)                       PRE00007
C                                                                       PRE00008
C***********************************************************************PRE00009
C                    PRERTDM - VERSION 3.0 (DATED 87306)
C******************************************************                 PRE00010
C       PROGRAM PRERTDM                                                 PRE00020
C                                                                       PRE00030
C       THIS PROGRAM READS IN A FILE PREPARED BY RAMMET AND EXTRACTS    PRE00040
C       WIND DIRECTION, WIND SPEED, STABILITY CLASS, MIXING HEIGHTS,    PRE00050
C       AND TEMPERATURE.  THE WIND DIRECTION IS OBTAINED BY ADDING      PRE00060
C       180 DEGREES TO THE RAMMET INPUT FLOW VECTOR.  INPUT DATA        PRE00070
C       IS READ FROM CHANNEL 2 AND OUTPUT DATA IS WRITTEN TO CHANNEL 3. PRE00080
C       INFORMATION ON THE NUMBER OF DAYS TO SKIP AND THE NUMBER OF     PRE00090
C       DAYS TO EXTRACT FROM THE BINARY MET FILE ARE READ IN FROM       PRE00100
C       CHANNEL 5.                                                      PRE00110
C                                                                       PRE00120
C******************************************************                 PRE00130
C                                                                       PRE00140
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          PRE00150
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          PRE00160
C                                                                       PRE00170
C       PRERTDM   VERSION 3.00    LEVEL 821010                          PRE00180
C                                                                       PRE00190
C******************************************************                 PRE00200
                                                                        PRE00210
                                                                        PRE00220
      INTEGER  IST(24),IYEAR,JYEAR,JMONTH,IDD                           PRE00230
      REAL U(24),FLOW(24),FLOWR(24)                                     PRE00240
      REAL TEMP(24),DMX(2,24),RDAY,STAB(24)                             PRE00250
C                                                                       PRE00260
C     PRINT OUT HEADER LINE
C
      WRITE(6,10)
10    FORMAT('1', 21X, 'PRERTDM3.2            (DATED 89226)',/)
C
C     READ FROM CHANNEL 5 TO OBTAIN SKIP AND EXTRACT COUNTS             PRE00270
C                                                                       PRE00280
      READ(5,1) ISKIP,IEXTR                                             PRE00290
    1 FORMAT(I5,I5)                                                     PRE00300
C                                                                       PRE00310
C     READ(2,END=9000) IDD,IYEAR,IDD,IYEAR  @ READ AND DISCARD HEADER   IBM00320
      READ(2,END=9000) IDD,IYEAR,IDD,IYEAR                              IBM00001
      IF(ISKIP.LT.1) GO TO 1500                                         PRE00330
      DO 1000 KSKIP=1,ISKIP                                             PRE00340
      READ(2,END=9000)JYEAR,JMONTH,RDAY,IST,U,TEMP,FLOW,FLOWR,DMX       PRE00350
 1000 CONTINUE                                                          PRE00360
 1500 DO 5000 KDAYS=1,IEXTR                                             PRE00370
      READ(2,END=9000)JYEAR,JMONTH,RDAY,IST,U,TEMP,FLOW,FLOWR,DMX       PRE00380
      IDAY=INT(RDAY+.001)                                               PRE00390
      DO 4000 K=1,24                                                    PRE00400
C     U(K)=U(K)/0.44707     @ SPEED IN MILES PER HOUR                   IBM00410
      U(K)=U(K)/0.44707                                                 IBM00002
C     TEMP(K)=1.8*TEMP(K)-459.67    @ TEMPERATURE IN DEG F              IBM00420
      TEMP(K)=1.8*TEMP(K)-459.67                                        IBM00003
      STAB(K)=FLOAT(IST(K))                                             PRE00430
C     FLOWR(K)=FLOWR(K)+180.0  @ METEOROLOGICAL CONVENTION FOR WIND DIR IBM00440
      FLOWR(K)=FLOWR(K)+180.0                                           IBM00004
      IF(FLOWR(K).LE.360.0) GO TO 4000                                  PRE00450
      FLOWR(K)=FLOWR(K)-360.0                                           PRE00460
 4000 CONTINUE                                                          PRE00470
      WRITE(3,2)(JYEAR,IDAY,I,FLOWR(I),U(I),DMX(1,I),STAB(I),TEMP(I),   PRE00480
     X I=1,24)                                                          PRE00490
    2 FORMAT(I2,I3,I2,1X,F6.0,F6.1,F6.0,F6.0,F6.1)                      PRE00500
 5000 CONTINUE                                                          PRE00510
C                                                                       PRE00520
 9000 CONTINUE                                                          PRE00530
C                                                                       PRE00540
      STOP                                                              PRE00550
      END                                                               PRE00560
