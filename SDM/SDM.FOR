C******************************************************************************
C                                                                             *
C                               SDM (DATED 90320)                             *
C                                                                             *
C                  *** SEE SDM MODEL CHANGE BULLETIN MCB#1 ***                *
C                                                                             *
C         ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD      *
C                                                                             *
C                                  919-541-5742                               *
C                                                                             *
C******************************************************************************
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSDM0001
C            SHORELINE DISPERSION MODEL (SDM)                           SDM0002
C                  --VERSION 1--                                        SDM0003
C                                                                       SDM0004
C  SDM PROGRAM ABSTRACT                                                 SDM0005
C     THE SHORELINE DISPERSION MODEL (SDM) IS A MULTIPOINT GAUSSIAN     SDM0006
C     DISPERSION MODEL THAT CAN BE USED TO DETERMINE GROUND-LEVEL       SDM0007
C     CONCENTRATIONS FROM TALL STATIONARY POINT SOURCES THAT ARE        SDM0008
C     INFLUENCED BY THE UNIQUE METEOROLOGICAL PHENOMENON IN A           SDM0009
C     SHORELINE ENVIRONMENT.  SDM IS A HYBRID MODEL THAT UTILIZES       SDM0010
C     THE SHORELINE FUMIGATION MODEL (SFM) TO DETERMINE THE HOURS       SDM0011
C     DURING THE YEAR WHEN FUMIGATION EVENTS ARE EXPECTED AND THAT      SDM0012
C     USES THE MPTER (VERSION 6) MODEL FOR THE REMAINING HOURS.         SDM0013
C                                                                       SDM0014
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            SDM0015
C       RESEARCH TRIANGLE PARK, NC                                      SDM0016
C                                                                       SDM0017
C SDM 1.0         REVISION HISTORY:                                     SDM0018
C                                                                       SDM0019
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSDM0020
C->->->-> COMMON, DIMENSION, AND DATA STATEMENTS.                       SDM0021
C                                                                       SDM0022
C         /EXPOS/  BETWEEN MAIN PROGRAM AND BLOCK DATA                  SDM0023
      COMMON /EXPOS/ PXUCOF(6,9),PXUEXP(6,9),HC1(10),BXUCOF(6,9),BXUEXP(SDM0024
     *6,9)                                                              SDM0025
C         /MPOR/ BETWEEN MAIN, PTR, OUTHR, AND RCP                      SDM0026
      COMMON /MPOR/ IOPT(26)                                            SDM0027
C         /MPO/ BETWEEN MAIN, PTR, AND OUTHR                            SDM0028
      COMMON /MPO/ NRECEP,NAVG,NB,LH,NPT,IDATE(2),RREC(180),SREC(180),ZRSDM0029
     1(180),ELR(180),PHCHI(180),PHSIGS(180,26),HSAV(250),DSAV(250),PCHI(SDM0030
     2180),PSIGS(180,26),IPOL                                           SDM0031
C         /MPR/  BETWEEN MAIN, PTR, AND RCP                             SDM0032
      COMMON /MPR/ UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC,MUOR                SDM0033
C         /MP/  BETWEEN MAIN PROGRAM AND PTR                            SDM0034
      COMMON /MP/ SOURCE(9,250),CONTWO,PSAV(250),IPSIGS(250),U,TEMP,SINTSDM0035
     1,COST,PL(6),ELP(250),ELHN,HANE,TLOS,CELM,CTER                     SDM0036
C         /MO/  BETWEEN MAIN PROGRAM AND OUTHR                          SDM0037
      COMMON /MO/ QTHETA(24),QU(24),IKST(24),QHL(24),QTEMP(24),MPS(25),NSDM0038
     1SIGP,IO,LINE1(20),LINE2(20),LINE3(20),IRANK(180)                  SDM0039
      COMMON /CHARS/ RNAME(2,180),STAR(5,180),SNAME(3)                  SDM0040
C         /MR/  BETWEEN MAIN PROGRAM AND RANK                           SDM0041
      COMMON /MR/ HMAXA(5,180,5),NDAY(5,180,5),IHR(5,180,5),CONC(180,5),SDM0042
     1JDAY,NR                                                           SDM0043
C                                                                       SDM0044
C     SHORELINE COMMON - ADDED BY JEFF WINGET (6-88) TO HANDLE SHORE    SDM0045
C     LINE DEFINITION CARDS                                             SDM0046
C                                                                       SDM0047
      COMMON /SHORE/XSL(250),YSL(250),BA(250),EA(250),FETCH(250),       SDM0048
     & INDEX(250),THETA                                                 SDM0049
      CHARACTER*4 SNAME,ENDS                                            SDM0050
                                                                        SDM0051
      COMMON /MSFM/ MSFMFL                                              SDM0052
      INTEGER MSFMFL(50,24)                                             SDM0053
      CHARACTER*5 CRADIL                                                SDM0054
      CHARACTER*4 RNAME,PNAME,ANAME,BLNK,ENDP,ENDR,DUM,STAR,STR,CF,C,   SDM0055
     & FUME                                                             SDM0056
      DIMENSION PNAME(3,250), IFREQ(7), DUMR(24), HLH(2,24), IMPS(25), TSDM0057
     1ITLE(2), TABLE(2,21), CONTER(6), RADIL(5), ANAME(36),PLL(6,2)     SDM0058
      DIMENSION SUM(180), ELRDUM(5), NTIME(5), ATIME(5), MODEL(2,2)     SDM0059
      DIMENSION CF(5),IDUMR(24)                                         SDM0060
C  *** Use next line for Lahey FORTRAN 77 Compiler                      SDM0061
      LOGICAL FLAG                                                      SDM0062
      CHARACTER*4 TITLE,MODEL                                           SDM0063
C                                                                       SDM0064
C                                                                       SDM0065
      DATA IFREQ /7*0/ ,BLNK /' '/                                      SDM0066
      DATA TITLE /'S02 ','PART'/                                        SDM0067
      DATA MODEL /'URBA','N','RURA','L'/                                SDM0068
      DATA ENDP /'ENDP'/ ,ENDR /'ENDR'/,ENDS /'ENDS'/                   SDM0069
      DATA MAXP /250/ ,STR /'*'/                                        SDM0070
C        MAXP EQUALS SECOND DIMENSION OF THE ARRAY NAMED: SOURCE.       SDM0071
      DATA ANAME /' 10,',' 20,',' 30,',' 40,',' 50,',' 60,',' 70,',' 80,SDM0072
     1',' 90,','100,','110,','120,','130,','140,','150,','160,','170,','SDM0073
     2180,','190,','200,','210,','220,','230,','240,','250,','260,','270SDM0074
     3,','280,','290,','300,','310,','320,','330,','340,','350,','360,'/SDM0075
     4                                                                  SDM0076
C                                                                       SDM0077
      DATA NTIME /1,3,8,24,0/ ,ATIME /1.,3.,8.,24.,0./                  SDM0078
      DATA ITMIN1 /9999/,IDIV8 /0/, IDIV24 /0/, ICALM /0/               SDM0079
      DATA C/'C'/,ICFL3/0/,ICFL8/0/,ICFL24/0/,CF/5*' '/                 SDM0080
      DATA FUME/'F'/                                                    SDM0081
      DATA L1/1/,L2/2/,L3/3/,L4/4/,L5/5/                                SDM0082
C                                                                       SDM0083
C      DEFAULT POWER LAW EXPONENTS AND TERRAIN ADJUSTMENT FACTORS.      SDM0084
C                                                                       SDM0085
      DATA PLL/.15,.15,.20,.25,.30,.30,.07,.07,.10,.15,.35,.55/         SDM0086
      DATA CONTER/0.,0.,0.,0.,0.,0./                                    SDM0087
C                                                                       SDM0088
C     CALL WSTCLK                                                       SDM0089
C                                                                       SDM0090
ccc   Heading for output moved to after file is opened --RCM            SDM0091
C                                                                       SDM0092
C->->->->SECTION E - RUN SET-UP AND READ FIRST 6 INPUT CARDS.           SDM0093
C                                                                       SDM0094
C        INITIALIZATIONS............                                    SDM0095
C        THE FOLLOWING 18 STATEMENTS MAY BE DELETED FOR USE ON          SDM0096
C        COMPUTERS THAT ZERO CORE LOCATIONS USED BY A PROBLEM           SDM0097
C        PRIOR TO EXECUTION.                                            SDM0098
C  *** Use next two lines for Lahey FORTRAN 77 Compiler                 SDM0099
      FLAG = .TRUE.                                                     SDM0100
C      call under0( flag)                                               SDM0101
      NRECEP=0                                                          SDM0102
      NP=0                                                              SDM0103
      NHR=0                                                             SDM0104
      NP3=0                                                             SDM0105
      NP8=0                                                             SDM0106
      NP24=0                                                            SDM0107
      NPX=0                                                             SDM0108
      DO 10 I=1,21                                                      SDM0109
      TABLE(1,I)=0.                                                     SDM0110
10    TABLE(2,I)=0.                                                     SDM0111
      DO 40 I=1,180                                                     SDM0112
      SUM(I)=0.                                                         SDM0113
      DO 30 J=1,5                                                       SDM0114
      CONC(I,J)=0.                                                      SDM0115
      DO 20 K=1,5                                                       SDM0116
20    HMAXA(J,I,K)=0.                                                   SDM0117
30    CONTINUE                                                          SDM0118
40    CONTINUE                                                          SDM0119
C        I/O DEVICE INITIALIZATIONS                                     SDM0120
      IN=5                                                              SDM0121
      IO=6                                                              SDM0122
                                                                        SDM0123
c     Input/Output files...... RCM 8/24/90  SRC                         SDM0124
      OPEN(5,FILE='INPUT',STATUS='OLD')                                 SDM0125
      OPEN(6,FILE='SDM.OUT',STATUS='UNKNOWN')                           SDM0126
      OPEN(20,FILE='SDM.FUM',STATUS='UNKNOWN')                          SDM0127
                                                                        SDM0128
      WRITE (6,5432)                                                    SDM0129
 5432 FORMAT ('1',34X,'SDM (DATED 90320)'/                              SDM0130
     1 29X,'AN AIR QUALITY DISPERSION MODEL '/                          SDM0131
     1 32X,'COMBINING MPTER AND SHORELINE FUMIGATION'/                  SDM0132
     4 22X,'SOURCE: UNAMAP FILE ON EPA''S IBM 3090, RTP. NC.')          SDM0133
                                                                        SDM0134
                                                                        SDM0135
C        UNIT 11 - DISK INPUT OF MET DATA--USED WHEN IOPT(5)=1.         SDM0136
C        UNIT 10 - DISK OUPUT OF PARTIAL CONCENTRATIONS                 SDM0137
C         AT EACH RECEPTOR--USED WHEN IOPT(21) = 1.                     SDM0138
C        UNIT 12 TAPE/DISK OUTPUT OF HRLY CONCENTRATIONS-IF IOPT(22)=1. SDM0139
C        UNIT 13 TAPE/DISK OUTPUT OF CONCENTRATIONS FOR AVERAGING PERIODSDM0140
C           USED IF IOPT(23) = 1.                                       SDM0141
C        UNIT 14 TAPE/DISK STORAGE FOR SUMMARY INFO, USED IF IOPT(20)=1.SDM0142
C        UNIT 15 - TAPE/DISK INPUT OF HOURLY POINT SOURCE EMISSIONS     SDM0143
C          -- USED IF IOPT(6) = 1.                                      SDM0144
C                                                                       SDM0145
C        READ CARDS 1-3  (SEE DESCRIPTION, SECTION B).                  SDM0146
C                                                                       SDM0147
      READ (IN,1180) LINE1,LINE2,LINE3                                  SDM0148
C                                                                       SDM0149
C        READ CARD TYPE 4  (SEE DESCRIPTION, SECTION B).                SDM0150
C                                                                       SDM0151
      READ (IN,*) IDATE(1),IDATE(2),IHSTRT,NPER,NAVG,IPOL,MUOR,NSIGP,   SDM0152
     1NAV5,CONONE,CELM,HAFL                                             SDM0153
C        THE ABOVE FORMAT IS IBM'S FREE FIELD INPUT.                    SDM0154
C        VARIABLES MUST BE SEPARATED BY COMMAS.                         SDM0155
C        THIS IS SIMILAR TO IBM'S LIST DIRECTED IO.                     SDM0156
      WRITE (IO,1395)(MODEL(K,MUOR),K=1,2),LINE1,LINE2,LINE3            SDM0157
      IF (NSIGP.LE.25) GO TO 50                                         SDM0158
      WRITE (IO,1250) NSIGP                                             SDM0159
C     CALL WAUDIT                                                       SDM0160
      STOP                                                              SDM0161
50    IP=IPOL-2                                                         SDM0162
      CONTWO=CONONE                                                     SDM0163
C        READ CARD TYPE 5  (SEE DESCRIPTION, SECTION B).                SDM0164
C                                                                       SDM0165
C***MODIFICATION  DCD  10/29/90                                         SDM0166
C    ADD SWITCH 26 - TOWER INPUT DATA FORMAT                            SDM0167
C        IOPT(26) = 0   TOWER.BIN IN BINARY                             SDM0168
C        IOPT(26) = 1   TOWER.BIN IN ASCII                              SDM0169
      READ (IN,*) (IOPT(I),I=1,26)                                      SDM0170
      IF(IOPT(26) .EQ. 0) THEN                                          SDM0171
         OPEN(19,FILE='TOWER.BIN',STATUS='OLD',FORM='UNFORMATTED')      SDM0172
      ELSE                                                              SDM0173
         OPEN(19,FILE='TOWER.ASC',STATUS='OLD')                         SDM0174
      ENDIF                                                             SDM0175
C                                                                       SDM0176
      IF(IOPT(25).NE.1) GO TO 55                                        SDM0177
C                                                                       SDM0178
C        DEFAULT SELECTION RESULTS IN THE FOLLOWING: USE STACK DOWNWASH SDM0179
C        (2); USE FINAL PLUME RISE (3); USE BUOYANCY-INDUCED DISPERSION SDM0180
C        (4); WRITE HIGH-5 TABLES (19) BUT DELETE ALL OTHER OUTPUT (10, SDM0181
C        11,12, 13, 14, 15, 16, 17, 18, 21, 22, 23, AND 24).            SDM0182
C                                                                       SDM0183
         IOPT(2)=0                                                      SDM0184
         IOPT(3)=1                                                      SDM0185
         IOPT(4)=1                                                      SDM0186
         IOPT(5)=0                                                      SDM0187
         IOPT(7)=0                                                      SDM0188
         IOPT(10)=1                                                     SDM0189
         IOPT(11)=1                                                     SDM0190
         IOPT(12)=1                                                     SDM0191
         IOPT(13)=1                                                     SDM0192
         IOPT(14)=1                                                     SDM0193
         IOPT(15)=1                                                     SDM0194
         IOPT(16)=1                                                     SDM0195
         IOPT(17)=1                                                     SDM0196
         IOPT(18)=1                                                     SDM0197
         IOPT(19)=0                                                     SDM0198
         IOPT(20)=0                                                     SDM0199
         IOPT(21)=0                                                     SDM0200
         IOPT(22)=0                                                     SDM0201
         IOPT(23)=0                                                     SDM0202
         IOPT(24)=0                                                     SDM0203
C                                                                       SDM0204
C        SET HALF-LIFE FOR DEFAULT OPTION                               SDM0205
C                                                                       SDM0206
         IF(IPOL.EQ.3.AND.MUOR.EQ.1)HAFL=14400.                         SDM0207
         IF(IPOL.NE.3.OR.MUOR.NE.1)HAFL=0.                              SDM0208
C                                                                       SDM0209
C          SET START HOUR AND AVERAGING PERIOD;                         SDM0210
C          SET THE NUMBER OF SIGNIFICANT POINT AND                      SDM0211
C          AREA SOURCES.                                                SDM0212
C                                                                       SDM0213
         IHSTRT=1                                                       SDM0214
         NAVG=24                                                        SDM0215
         NSIGP=0                                                        SDM0216
C                                                                       SDM0217
55    CONTINUE                                                          SDM0218
C                                                                       SDM0219
C***MODIFICATION  DCD  9/18/90                                          SDM0220
C   Input/output file specifications based on options                   SDM0221
      IF(IOPT(22).EQ.1) OPEN(12,FILE='HRCONCS',FORM='UNFORMATTED',      SDM0222
     1  STATUS='UNKNOWN')                                               SDM0223
      IF(IOPT(21).EQ.1) OPEN(10,FILE='PARTIAL',STATUS='UNKNOWN',        SDM0224
     1  FORM='UNFORMATTED')                                             SDM0225
      IF(IOPT(5).EQ.0) OPEN(11,FILE='MET',STATUS='OLD',                 SDM0226
     1  FORM='UNFORMATTED')                                             SDM0227
      IF(IOPT(23).EQ.1) OPEN(13,FILE='AVGCONCS',STATUS='UNKNOWN',       SDM0228
     1  FORM='UNFORMATTED')                                             SDM0229
      IF(IOPT(6).EQ.1) OPEN(15,FILE='EMISSIONS',STATUS='OLD',           SDM0230
     1  FORM='UNFORMATTED')                                             SDM0231
      IF(IOPT(20).EQ.1) OPEN(14,FILE='SUMMARY',STATUS='UNKNOWN',        SDM0232
     1  FORM='UNFORMATTED')                                             SDM0233
      IF(IOPT(24).EQ.1) OPEN(1,FILE='AVCONCS',STATUS='UNKNOWN')         SDM0234
C *** END OF MODIFICATION                                               SDM0235
C           WRITE GENERAL INPUT INFORMATION                             SDM0236
      WRITE (IO,1410) TITLE(IP),NPER,NAVG,IHSTRT,IDATE(2),IDATE(1),CONTWSDM0237
     1O,NSIGP                                                           SDM0238
      DAY1A=IDATE(2)                                                    SDM0239
      HR1=IHSTRT                                                        SDM0240
      IF (HAFL.GT.0.0) GO TO 60                                         SDM0241
      TLOS=0.                                                           SDM0242
      WRITE (IO,1420)                                                   SDM0243
      GO TO 70                                                          SDM0244
60    WRITE (IO,1430) HAFL                                              SDM0245
      TLOS=693./HAFL                                                    SDM0246
70    IF (IOPT(19).EQ.1) GO TO 80                                       SDM0247
      NAVT=5                                                            SDM0248
C        FOR DEFAULT OPTION                                             SDM0249
C        ADDITIONAL AVERAGING PERIOD SET TO ZERO.                       SDM0250
      IF(IOPT(25).EQ.1) NAV5=0                                          SDM0251
      IF (NAV5.EQ.1.OR.NAV5.EQ.3.OR.NAV5.EQ.8.OR.NAV5.EQ.24.OR.NAV5.EQ.0SDM0252
     1) NAVT=4                                                          SDM0253
      NTIME(5)=NAV5                                                     SDM0254
      ATIME(5)=NAV5                                                     SDM0255
      WRITE (IO,1440) NAVT                                              SDM0256
80    IF (IOPT(1).EQ.0) GO TO 90                                        SDM0257
      WRITE (IO,1450) CELM                                              SDM0258
      ELHN=99999.                                                       SDM0259
      ELOW=99999.                                                       SDM0260
90    IF (NSIGP.GT.0) GO TO 100                                         SDM0261
      IOPT(11)=1                                                        SDM0262
      IOPT(17)=1                                                        SDM0263
100   WRITE (IO,1460) (I,IOPT(I),I=1,13)                                SDM0264
      WRITE (IO,1470) (I,IOPT(I),I=14,26)                               SDM0265
C                                                                       SDM0266
C        READ CARD TYPE 6 (SEE DESCRIPTION, SECTION B).                 SDM0267
C                                                                       SDM0268
C      SWITCH TO SELECT DEFAULT POWER LAW EXPONENTS,                    SDM0269
C      TERS.RAIN ADJUSTMENT FACTOR                                      SDM0270
C                                                                       SDM0271
      IF(IOPT(25).NE.0)READ(IN,*)HANE                                   SDM0272
      IF(IOPT(25).EQ.0)READ(IN,*)HANE,PL,CONTER                         SDM0273
      IF(IOPT(25).EQ.0) GO TO 105                                       SDM0274
      DO 104 I1=1,6                                                     SDM0275
      PL(I1)=PLL(I1,MUOR)                                               SDM0276
104   CONTINUE                                                          SDM0277
105   CONTINUE                                                          SDM0278
C                                                                       SDM0279
      IF (IOPT(1).EQ.1) GO TO 110                                       SDM0280
      WRITE (IO,1480) HANE,PL                                           SDM0281
      GO TO 140                                                         SDM0282
110   WRITE (IO,1490) HANE,PL,CONTER                                    SDM0283
      DO 120 I=1,6                                                      SDM0284
      IF (CONTER(I).LT.0..OR.CONTER(I).GT.1.) GO TO 130                 SDM0285
120   CONTINUE                                                          SDM0286
      GO TO 140                                                         SDM0287
130   WRITE (IO,1260)                                                   SDM0288
C     CALL WAUDIT                                                       SDM0289
      STOP                                                              SDM0290
C                                                                       SDM0291
C        MUCH OF THE FOLLOWING PROGRAM SECTION IS BASED UPON            SDM0292
C        RAMQ IN THE RAM SYSTEM. THIS SECTION IS RESPONSIBLE            SDM0293
C        FOR MAKING THE NECESSARY DATA CONVERSIONS ON THE RAW           SDM0294
C        EMISSIONS DATA IN ORDER TO ESTABLISH A STANDARD                SDM0295
C        DATA BANK WHICH WILL BE ACCEPTABLE. A CONVERSION FACTOR        SDM0296
C        FROM USER UNITS TO KILOMETERS IS APPLIED WHEN NECESSARY.       SDM0297
C                                                                       SDM0298
C->->->->SECTION F - INPUT AND PROCESS EMISSION INFORMATION.            SDM0299
C                                                                       SDM0300
140   WRITE (IO,1500)                                                   SDM0301
      NPT=0                                                             SDM0302
C        BEGIN LOOP TO READ THE POINT SOURCE INFORMATION                SDM0303
150   NPT=NPT+1                                                         SDM0304
      IF (NPT.LE.MAXP) GO TO 160                                        SDM0305
      READ (IN,1200) DUM                                                SDM0306
      IF (DUM.EQ.ENDP) GO TO 230                                        SDM0307
      WRITE (IO,1270) MAXP                                              SDM0308
C     CALL WAUDIT                                                       SDM0309
      STOP                                                              SDM0310
C                                                                       SDM0311
C        READ CARD TYPE 7  (SEE DESCRIPTION, SECTION B).                SDM0312
C                                                                       SDM0313
160   READ (IN,1210) (PNAME(I,NPT),I=1,3),(SOURCE(I,NPT),I=1,8),ELP(NPT)SDM0314
C        CARD WITH     'ENDP'   IN COL 1-10 IS USED TO SIGNIFY END OF   SDM0315
C        POINT SOURCES.                                                 SDM0316
      IF (PNAME(1,NPT).EQ.ENDP) GO TO 230                               SDM0317
C        ELHN, ELEVATION OF LOWEST STACK TOP IN INVENTORY, IS DETERMINEDSDM0318
C        IN USER HEIGHT UNITS                                           SDM0319
      IF (IOPT(1).EQ.0) GO TO 170                                       SDM0320
      TOM=SOURCE(5,NPT)/CELM+ELP(NPT)                                   SDM0321
      IF (TOM.LT.ELHN) ELHN=TOM                                         SDM0322
C        LOWPT, ELEVATION OF LOWEST SOURCE GROUND-LEVEL                 SDM0323
C         IN INVENTORY, IN USER HEIGHT UNITS.                           SDM0324
      IF (ELP(NPT).LT.ELOW) ELOW=ELP(NPT)                               SDM0325
C        CALCULATE BUOYANCY FACTOR                                      SDM0326
170   D=SOURCE(7,NPT)                                                   SDM0327
C         FOLLOWING VARIABLE IS BRIGGS' F WITHOUT TEMPERATURE FACTOR.   SDM0328
      SOURCE(9,NPT)=2.45153*SOURCE(8,NPT)*D*D                           SDM0329
C        2.45153 IS GRAVITY OVER FOUR.                                  SDM0330
      TS=SOURCE(6,NPT)                                                  SDM0331
      IF (TS.GT.293.) GO TO 180                                         SDM0332
      HF=SOURCE(5,NPT)                                                  SDM0333
      GO TO 200                                                         SDM0334
180   F=SOURCE(9,NPT)*(TS-293.)/TS                                      SDM0335
      IF (F.GE.55.) GO TO 190                                           SDM0336
C        ONLY BUOYANCY PLUME RISE IS CONSIDERED HERE.                   SDM0337
      HF=SOURCE(5,NPT)+21.425*F**0.75/3.                                SDM0338
      GO TO 200                                                         SDM0339
190   HF=SOURCE(5,NPT)+38.71*F**0.6/3.                                  SDM0340
C        HSAV, DSAV, AND PSAV ARE USED FOR TEMPORARY STORAGE            SDM0341
C        (OR AS DUMMIES) FOR THE NEXT 60 STATEMENTS.                    SDM0342
200   HSAV(NPT)=HF                                                      SDM0343
C        DETERMINE HEIGHT INDEX.                                        SDM0344
      DO 210 IH=2,9                                                     SDM0345
      IF (HF.LT.(HC1(IH)-.01)) GO TO 220                                SDM0346
210   CONTINUE                                                          SDM0347
      IH=10                                                             SDM0348
220   IS=IH-1                                                           SDM0349
      IF(MUOR.EQ.1)GO TO 221                                            SDM0350
      A=PXUCOF(2,IS)                                                    SDM0351
      B=PXUEXP(2,IS)                                                    SDM0352
      GO TO 222                                                         SDM0353
221   A=BXUCOF(2,IS)                                                    SDM0354
      B=BXUEXP(2,IS)                                                    SDM0355
222   DSAV(NPT)=(A*HF**B)*SOURCE(IPOL,NPT)/3.                           SDM0356
C        AN ESTIMATE OF THE POTENTIAL IMPACT OF EACH SOURCE IS          SDM0357
C         DETERMINED AND STORED IN DSAV. MAX CONCENTRATION IS           SDM0358
C         DETERMINED BY CHI(MAX)=(A*H**B)*Q/U WHERE                     SDM0359
C         A IS THE COEFFICIENT AND B IS THE EXPONENT, OF                SDM0360
C         MAXIMUM CHI*U/Q VALUES PREDETERMINED FOR B STABILITY          SDM0361
C         AND A SPECIFIC EFFECTIVE HEIGHT RANGE. PLUME RISE             SDM0362
C         IS CALCULATED FOR B STABILITY AND 3 M/SEC WIND SPEED.         SDM0363
C                                                                       SDM0364
C        GO BACK AND READ DATA FOR ANOTHER POINT SOURCE.                SDM0365
      IPSIGS(NPT)=0                                                     SDM0366
C        LIST POINT SOURCE INFORMATION.                                 SDM0367
      WRITE (IO,1510) NPT,(PNAME(J,NPT),J=1,3),(SOURCE(K,NPT),K=1,8),DSASDM0368
     1V(NPT),HSAV(NPT),ELP(NPT),F                                       SDM0369
      GO TO 150                                                         SDM0370
230   NPT=NPT-1                                                         SDM0371
C         CHECK FOR NPT < OR = 0                                        SDM0372
      IF (NPT.GT.0) GO TO 240                                           SDM0373
      WRITE (IO,1280) NPT                                               SDM0374
C     CALL WAUDIT                                                       SDM0375
      STOP                                                              SDM0376
C                                                                       SDM0377
C->->->->SECTION G - RANK SIGNIFICANT SOURCES.                          SDM0378
C                                                                       SDM0379
240   IF (NSIGP.EQ.0) GO TO 280                                         SDM0380
C        RANK NSIGP HIGHEST POINT SOURCES.                              SDM0381
      IF (NPT.LT.NSIGP) NSIGP=NPT                                       SDM0382
      DO 260 I=1,NSIGP                                                  SDM0383
      SIGMAX=-1.0                                                       SDM0384
      DO 250 J=1,NPT                                                    SDM0385
      IF (DSAV(J).LE.SIGMAX) GO TO 250                                  SDM0386
      SIGMAX=DSAV(J)                                                    SDM0387
      LMAX=J                                                            SDM0388
250   CONTINUE                                                          SDM0389
C        IMPS IS THE SOURCE NUMBER IN ORDER OF SIGNIFICANCE.            SDM0390
      IMPS(I)=LMAX                                                      SDM0391
C        PSAV IS THE CALC. CONC. IN ORDER OF SIGNIFICANCE.              SDM0392
      PSAV(I)=SIGMAX                                                    SDM0393
260   DSAV(LMAX)=-1.0                                                   SDM0394
C        OUTPUT TABLE OF RANKED SOURCES.                                SDM0395
      WRITE (IO,1520) TITLE(IP)                                         SDM0396
      DO 270 I=1,NSIGP                                                  SDM0397
      WRITE (IO,1530) I,PSAV(I),IMPS(I)                                 SDM0398
270   CONTINUE                                                          SDM0399
C                                                                       SDM0400
C->->->->SECTION H - EMISSIONS WITH HEIGHT TABLE.                       SDM0401
C                                                                       SDM0402
280   IF (IOPT(9).EQ.1) GO TO 340                                       SDM0403
      DO 320 I=1,NPT                                                    SDM0404
      DO 290 J=1,20                                                     SDM0405
      HC=J*5.                                                           SDM0406
      IF (SOURCE(5,I).LE.HC) GO TO 300                                  SDM0407
290   CONTINUE                                                          SDM0408
C        POINT SOURCES WITH PHYSICAL HEIGHTS GT 100 METERS ARE LISTED   SDM0409
C        SEPARATELY.                                                    SDM0410
      WRITE (IO,1540) I,SOURCE(5,I),SOURCE(IPOL,I)                      SDM0411
      GO TO 310                                                         SDM0412
C        ADD EMISSION RATE INTO TABLE AND TOTAL.                        SDM0413
300   TABLE(1,J)=TABLE(1,J)+SOURCE(IPOL,I)                              SDM0414
310   TABLE(1,21)=TABLE(1,21)+SOURCE(IPOL,I)                            SDM0415
320   CONTINUE                                                          SDM0416
C        OUTPUT SOURCE-STRENGTH-HEIGHT TABLE                            SDM0417
C        THIS TABLE DISPLAYS THE TOTAL EMISSIONS FOR POINT              SDM0418
C        SOURCES AND THE CUMULATIVE FREQUENCY ACCORDING TO              SDM0419
C        HEIGHT CLASS                                                   SDM0420
      WRITE (IO,1550) TITLE(IP)                                         SDM0421
C        HEIGHT CLASS EMISSIONS ARE IN 1                                SDM0422
C        DETERMINE CUMULATIVE PERCENT IN 2                              SDM0423
      IH1=0                                                             SDM0424
      IH2=5                                                             SDM0425
      IM1=1                                                             SDM0426
      TABLE(2,1)=TABLE(1,1)/TABLE(1,21)                                 SDM0427
      WRITE (IO,1560) IH1,IH2,(TABLE(J,1),J=1,2)                        SDM0428
      DO 330 I=2,20                                                     SDM0429
      IH2=I*5                                                           SDM0430
      IH1=IH2-4                                                         SDM0431
      IM1=I-1                                                           SDM0432
      TABLE(2,I)=TABLE(1,I)/TABLE(1,21)+TABLE(2,IM1)                    SDM0433
      WRITE (IO,1560) IH1,IH2,(TABLE(J,I),J=1,2)                        SDM0434
330   CONTINUE                                                          SDM0435
      WRITE (IO,1570) TABLE(1,21)                                       SDM0436
 340  NSL=1                                                             SDM0437
3010  READ (IN,3000) (SNAME(I),I=1,3),XSL(NSL),YSL(NSL),                SDM0438
     & BA(NSL),EA(NSL),FETCH(NSL)                                       SDM0439
C   MODIFICATION  DCD 10/30/90                                          SDM0440
C   ECHO SHORELINE INPUTS TO OUTPUT FILE                                SDM0441
      IF(SNAME(1) .NE. ENDS) WRITE(IO,3011) (SNAME(I),I=1,3),XSL(NSL),  SDM0442
     &   YSL(NSL),BA(NSL),EA(NSL),FETCH(NSL)                            SDM0443
      IF (SNAME(1).EQ.ENDS) THEN                                        SDM0444
         WRITE(IO,3012)                                                 SDM0445
         GO TO 3020                                                     SDM0446
      ENDIF                                                             SDM0447
C   END OF MODIFICATION                                                 SDM0448
3000  FORMAT (3A4,5F8.0)                                                SDM0449
      NSL=NSL+1                                                         SDM0450
      GO TO 3010                                                        SDM0451
3020  CONTINUE                                                          SDM0452
C                                                                       SDM0453
C->->->->SECTION I - EXECUTE FOR INPUT OF SIGNIFICANT SOURCE NUMBERS.   SDM0454
C                                                                       SDM0455
      WRITE (IO,1580)                                                   SDM0456
      IF (IOPT(7).EQ.0) GO TO 370                                       SDM0457
C                                                                       SDM0458
C        READ CARD TYPE 8 (SEE DESCRIPTION, SECTION B).                 SDM0459
C                                                                       SDM0460
      READ (IN,1220) INPT,(MPS(I),I=1,INPT)                             SDM0461
      WRITE (IO,1590) INPT,(MPS(I),I=1,INPT)                            SDM0462
      IF (INPT.LE.NSIGP) GO TO 350                                      SDM0463
      WRITE (IO,1290) INPT,NSIGP                                        SDM0464
C     CALL WAUDIT                                                       SDM0465
      STOP                                                              SDM0466
350   IF (INPT.EQ.0) GO TO 370                                          SDM0467
      IF (MPS(INPT).EQ.0) WRITE (IO,1300)                               SDM0468
      J=INPT+1                                                          SDM0469
      K=1                                                               SDM0470
C        ADD SIGNIFICANT SOURCES DETERMINED FROM RANKED SOURCE LIST     SDM0471
C        IF NSIGP GREATER THAN INPT.                                    SDM0472
      IF (J.GT.NSIGP) GO TO 390                                         SDM0473
      DO 360 I=J,NSIGP                                                  SDM0474
      MPS(I)=IMPS(K)                                                    SDM0475
360   K=K+1                                                             SDM0476
      GO TO 390                                                         SDM0477
370   DO 380 I=1,NSIGP                                                  SDM0478
380   MPS(I)=IMPS(I)                                                    SDM0479
390   WRITE (IO,1600) NPT,NSIGP,(MPS(I),I=1,NSIGP)                      SDM0480
      IF (IOPT(6).EQ.0) GO TO 410                                       SDM0481
C        SAVE AVERAGE EMISSION RATE                                     SDM0482
      DO 400 I=1,NPT                                                    SDM0483
400   PSAV(I)=SOURCE(IPOL,I)                                            SDM0484
C        FILL IN SIGNIFICANT POINT SOURCE ARRAY                         SDM0485
410   DO 420 I=1,NSIGP                                                  SDM0486
      J=MPS(I)                                                          SDM0487
420   IPSIGS(J)=I                                                       SDM0488
C                                                                       SDM0489
C->->->->SECTION J - CHECK MET DATA IF FROM FILE OF ONE YEAR'S DATA.    SDM0490
C                                                                       SDM0491
      IF (IOPT(5).EQ.1) GO TO 450                                       SDM0492
C                                                                       SDM0493
C        READ CARD TYPE 9 (SEE DESCRIPTION, SECTION B).                 SDM0494
C                                                                       SDM0495
      READ (IN,*) ISFCD,ISFCYR,IMXD,IMXYR                               SDM0496
C        READ ID RECORD FROM PREPROCESSED MET DISK OR TAPE FILE.        SDM0497
      READ (11) ID,IYEAR,IDM,IYM                                        SDM0498
      IF (ISFCD.EQ.ID.AND.ISFCYR.EQ.IYEAR) GO TO 430                    SDM0499
      WRITE (IO,1310) ISFCD,ISFCYR,ID,IYEAR                             SDM0500
C     CALL WAUDIT                                                       SDM0501
      STOP                                                              SDM0502
430   IF (IMXD.EQ.IDM.AND.IMXYR.EQ.IYM) GO TO 440                       SDM0503
      WRITE (IO,1320) IMXD,IMXYR,IDM,IYM                                SDM0504
C     CALL WAUDIT                                                       SDM0505
      STOP                                                              SDM0506
440   WRITE (IO,1610) ISFCD,ISFCYR,IMXD,IMXYR                           SDM0507
C                                                                       SDM0508
C->->->->SECTION K - GENERATE POLAR COORDINATE RECEPTORS.               SDM0509
C                                                                       SDM0510
450   NRECEP=0                                                          SDM0511
      WRITE (IO,1620)                                                   SDM0512
      IF (IOPT(8).NE.1) GO TO 520                                       SDM0513
C                                                                       SDM0514
C        READ CARD TYPE 10 (SEE DESCRIPTION, SECTION B).                SDM0515
C                                                                       SDM0516
      READ (IN,*) RADIL,CENTX,CENTY                                     SDM0517
      JA=0                                                              SDM0518
      DO 460 J=1,5                                                      SDM0519
      IF (RADIL(J).EQ.0) GO TO 460                                      SDM0520
      JA=JA+1                                                           SDM0521
460   CONTINUE                                                          SDM0522
      WRITE (IO,1630) CENTX,CENTY,RADIL                                 SDM0523
      DO 480 I=1,36                                                     SDM0524
C          CALCULATE THE ANGLE IN RADIANS                               SDM0525
      RADIK=FLOAT(I)*0.1745329                                          SDM0526
C        0.1745329 IS 2*PI/36                                           SDM0527
      SINRAD=SIN(RADIK)                                                 SDM0528
      COSRAD=COS(RADIK)                                                 SDM0529
      DO 470 J=1,JA                                                     SDM0530
      NRECEP=I+36*(J-1)                                                 SDM0531
      RREC(NRECEP)=(RADIL(J)*SINRAD)+CENTX                              SDM0532
C          CALCULATE THE EAST-COORDINATE                                SDM0533
      SREC(NRECEP)=(RADIL(J)*COSRAD)+CENTY                              SDM0534
C          CALCULATE THE NORTH-COORDINATE                               SDM0535
      RNAME(1,NRECEP)=ANAME(I)                                          SDM0536
C          ALPHANUMERIC INFORMATION WHICH INDICATES DEGREES AZIMUTH     SDM0537
C     ENCODE (4,1640,RNAME(2,NRECEP)) RADIL(J)                          SDM0538
C          ENCODE THE FLOATING POINT VARIABLE OF RADIAL DISTANCE        SDM0539
C          TO ALPHANUMERIC REPRESENTATION SO INFO CAN BE PRINTED        SDM0540
C                                                                       SDM0541
C  THE IBM3090 DOES NOT SUPPORT ENCODE SO WRITE THE RADIL DISTANCE      SDM0542
C  TO AN INTERNAL FILE TO CONVERT IT TO A CHARACTER VALUE               SDM0543
C                                                                       SDM0544
      ASSIGN 2531 TO NUMF                                               SDM0545
      IF (RADIL(J) .LT. 100.) ASSIGN 2532 TO NUMF                       SDM0546
      IF (RADIL(J) .LT. 0.1) ASSIGN 2533 TO NUMF                        SDM0547
      WRITE(CRADIL,NUMF) RADIL(J)                                       SDM0548
      RNAME(2,NRECEP)=CRADIL                                            SDM0549
C                                                                       SDM0550
      ZR(NRECEP)=0.                                                     SDM0551
      ELR(NRECEP)=0.                                                    SDM0552
470   CONTINUE                                                          SDM0553
480   CONTINUE                                                          SDM0554
      NRECEP=36*JA                                                      SDM0555
C                                                                       SDM0556
C->->->->SECTION L - READ POLAR COORDINATE ELEVATIONS.                  SDM0557
C                                                                       SDM0558
      IF (IOPT(1).EQ.0) GO TO 520                                       SDM0559
C                                                                       SDM0560
C        READ 36 CARDS, TYPE 11 (SEE DESCRIPTION, SECTION 8).           SDM0561
C                                                                       SDM0562
      DO 510 I=1,36                                                     SDM0563
      READ (IN,1230) IDUM,(ELRDUM(J),J=1,JA)                            SDM0564
      IF (IDUM.EQ.I) GO TO 490                                          SDM0565
      WRITE (IO,1330) I,IDUM                                            SDM0566
C     CALL WAUDIT                                                       SDM0567
      STOP                                                              SDM0568
490   DO 500 J=1,JA                                                     SDM0569
      K=J-1                                                             SDM0570
      L=K*36+I                                                          SDM0571
500   ELR(L)=ELRDUM(J)                                                  SDM0572
510   CONTINUE                                                          SDM0573
C                                                                       SDM0574
C->->->->SECTION M - READ AND PROCESS RECEPTOR INFORMATION.             SDM0575
C                                                                       SDM0576
C          NOW READ CARD TYPE 12 IF NECESSARY. MUST HAVE A CARD WITH    SDM0577
C          'ENDR'IN COLS 1-4 TO INDICATE END OF RECEPTOR CARDS.         SDM0578
C          NO MORE THAN 180 RECEPTORS CAN BE INPUT TO MPTER.            SDM0579
C        START LOOP TO ENTER RECEPTORS.                                 SDM0580
520   NRECEP=NRECEP+1                                                   SDM0581
      IF (NRECEP.LE.180) GO TO 540                                      SDM0582
      READ (IN,1200,END=530) DUM                                        SDM0583
      IF (DUM.EQ.ENDR) GO TO 550                                        SDM0584
530   WRITE (IO,1340)                                                   SDM0585
C     CALL WAUDIT                                                       SDM0586
      STOP                                                              SDM0587
C                                                                       SDM0588
C        READ CARD TYPE 12 (SEE DESCRIPTION, SECTION B).                SDM0589
C                                                                       SDM0590
540   READ (IN,1240) (RNAME(J,NRECEP),J=1,2),RREC(NRECEP),SREC(NRECEP),ZSDM0591
     1R(NRECEP),ELR(NRECEP)                                             SDM0592
C        PLACE 'ENDR' IN COLS 1 TO 4 ON CARD FOLLOWING LAST RECEPTOR    SDM0593
C        TO END READING TYPE 12 CARDS.                                  SDM0594
      IF (RNAME(1,NRECEP).EQ.ENDR) GO TO 550                            SDM0595
      GO TO 520                                                         SDM0596
550   NRECEP=NRECEP-1                                                   SDM0597
      IF (IOPT(1).EQ.0) GO TO 570                                       SDM0598
C        IF TERRAIN OPTION IS EMPLOYED, DETERMINE IF                    SDM0599
C         RECEPTOR ELEVATIONS REQUIRE LABELING WITH ASTERISKS           SDM0600
C         FOR ADDITIONAL REMARKS.                                       SDM0601
      DO 560 J=1,NRECEP                                                 SDM0602
      IF (ELR(J).GT.ELHN.OR.ELR(J).LT.ELOW) STAR(2,J)=STR               SDM0603
      IF (ELR(J).GT.ELHN) STAR(1,J)=STR                                 SDM0604
560   CONTINUE                                                          SDM0605
570   IF (NRECEP.GT.0) GO TO 580                                        SDM0606
      WRITE (IO,1350) NRECEP                                            SDM0607
C     CALL WAUDIT                                                       SDM0608
      STOP                                                              SDM0609
C        PRINT OUT TABLE OF RECEPTORS. ***                              SDM0610
580   WRITE (IO,1650)                                                   SDM0611
      DO 590 K=1,NRECEP                                                 SDM0612
590   WRITE (IO,1660) K,STAR(1,K),STAR(2,K),(RNAME(J,K),J=1,2),RREC(K),SSDM0613
     1REC(K),ZR(K),ELR(K)                                               SDM0614
      IF (IOPT(1).EQ.0) GO TO 600                                       SDM0615
      WRITE (IO,1670)                                                   SDM0616
C                                                                       SDM0617
C->->->->SECTION N - POSITION FILES AS REQUIRED.                        SDM0618
C                                                                       SDM0619
600   IF (IOPT(20).EQ.0) GO TO 610                                      SDM0620
C                                                                       SDM0621
C        READ CARD TYPE 13 (SEE DESCRIPTION, SECTION B).                SDM0622
C                                                                       SDM0623
      READ (IN,*) IDAY,LDRUN                                            SDM0624
      WRITE (IO,1680) IDAY,LDRUN                                        SDM0625
      IF (IDAY.EQ.0) GO TO 610                                          SDM0626
C        READ INFO FOR HIGH-FIVE TABLE FROM LAST SEGMENT.               SDM0627
      READ (14) IDAYS,SUM,NHR,DAY1A,HR1,HMAXA,NDAY,IHR                  SDM0628
      REWIND 14                                                         SDM0629
      IF (IDAY.EQ.IDAYS) GO TO 610                                      SDM0630
      WRITE (IO,1360) IDAY,IDAYS                                        SDM0631
C     CALL WAUDIT                                                       SDM0632
      STOP                                                              SDM0633
610   NP=IDAY*(24/NAVG)                                                 SDM0634
C        IF OPTION 21 = 1, WRITE INITIAL INFO TO UNIT 10                SDM0635
      IF (IOPT(21).EQ.1) WRITE (10) NPER,NAVG,LINE1,LINE2,LINE3         SDM0636
      IF (IOPT(22).EQ.0) GO TO 640                                      SDM0637
      IF (IDAY.LE.0) GO TO 630                                          SDM0638
C        SKIP PREVIOUSLY GENERATED HOURLY RECORDS.                      SDM0639
      ISKIP=IDAY*24+2                                                   SDM0640
      DO 620 I=1,ISKIP                                                  SDM0641
620   READ (12)                                                         SDM0642
      GO TO 640                                                         SDM0643
C        WRITE LEAD TWO RECORDS ON HOURLY FILE.                         SDM0644
630   WRITE (12) NPER,NAVG,LINE1,LINE2,LINE3                            SDM0645
      WRITE (12) NRECEP,(RREC(I),I=1,NRECEP),(SREC(J),J=1,NRECEP)       SDM0646
640   IF (IOPT(23).EQ.0) GO TO 670                                      SDM0647
      IF (IDAY.LE.0) GO TO 660                                          SDM0648
C        SKIP PREVIOUSLY GENERATED AVERAGING-PERIOD FILE.               SDM0649
      ISKIP=NP+2                                                        SDM0650
      DO 650 I=1,ISKIP                                                  SDM0651
650   READ (13)                                                         SDM0652
      GO TO 670                                                         SDM0653
C        WRITE LEAD TWO RECORDS ON AVERAGING PERIOD FILE.               SDM0654
660   WRITE (13) NPER,NAVG,LINE1,LINE2,LINE3                            SDM0655
      WRITE (13) NRECEP,(RREC(I),I=1,NRECEP),(SREC(J),J=1,NRECEP)       SDM0656
670   IF (IOPT(6).EQ.0) GO TO 690                                       SDM0657
      IF (IDAY.LE.0) GO TO 690                                          SDM0658
      ISKIP=IDAY*24                                                     SDM0659
      DO 680 I=1,ISKIP                                                  SDM0660
680   READ (15)                                                         SDM0661
690   IDAY=IDATE(2)-1                                                   SDM0662
      IF (IDAY.LE.0.OR.IOPT(5).EQ.1) GO TO 710                          SDM0663
C        SKIP PREVIOUSLY USED HOURLY EMISSION RECORDS.                  SDM0664
      DO 700 I=1,IDAY                                                   SDM0665
700   READ (11)                                                         SDM0666
710   CONTINUE                                                          SDM0667
C                                                                       SDM0668
C->->->->SECTION O - START LOOPS FOR DAY AND AVG TIME; READ MET DATA.   SDM0669
C                                                                       SDM0670
720   IDAY=IDAY+1                                                       SDM0671
      DAY=IDAY                                                          SDM0672
      WRITE(*,*) 'Processing Day: ',IDAY                                SDM0673
      NHRS=0                                                            SDM0674
      IF (IOPT(5).EQ.1) GO TO 760                                       SDM0675
C        IF OPTION 5 EQUALS ZERO, INPUT MET DATA OFF DISK (UNIT 11)     SDM0676
      READ (11) JYR,IMO,DAY1,IKST,QU,QTEMP,DUMR,QTHETA,HLH              SDM0677
      DO 781 JM1=1,24                                                   SDM0678
      IDUMR(JM1)=DUMR(JM1)+0.5                                          SDM0679
 781  CONTINUE                                                          SDM0680
      IF (JYR.NE.IDATE(1)) GO TO 730                                    SDM0681
      IF (DAY1.EQ.DAY) GO TO 740                                        SDM0682
C        DATE ON MET TAPE DOES NOT MATCH INTERNAL DATE                  SDM0683
730   WRITE (IO,1370) JYR,IDATE(2),IDATE(1),IDAY                        SDM0684
C     CALL WAUDIT                                                       SDM0685
      STOP                                                              SDM0686
C        MODIFY WIND VECTOR BY 180 DEGREES. SINCE FLOW VECTORS WERE     SDM0687
C        OUTPUT FROM RAMMET. THIS CONVERTS BACK TO WIND DIRECTIONS.     SDM0688
740   IDATE(2)=DAY1                                                     SDM0689
      DO 750 IQ=1,24                                                    SDM0690
      IF (IKST(IQ).EQ.7) IKST(IQ)=6                                     SDM0691
      QTHETA(IQ)=QTHETA(IQ)+180.                                        SDM0692
      IF (QTHETA(IQ).GT.360.) QTHETA(IQ)=QTHETA(IQ)-360.                SDM0693
C       SELECT URBAN OR RURAL MIXING HEIGHTS AS APPROPRIATE.            SDM0694
      IF(MUOR.EQ.1)IMX=2                                                SDM0695
      IF(MUOR.EQ.2)IMX=1                                                SDM0696
750   QHL(IQ)=HLH(IMX,IQ)                                               SDM0697
760   NB=IHSTRT                                                         SDM0698
      NE=NB+NAVG-1                                                      SDM0699
      IF (NB.GT.0) GO TO 770                                            SDM0700
      WRITE (IO,1380) IHSTRT                                            SDM0701
C     CALL WAUDIT                                                       SDM0702
      STOP                                                              SDM0703
C        START LOOP FOR AVERAGING PERIOD.                               SDM0704
770   U=0.0                                                             SDM0705
      TEMP=0.0                                                          SDM0706
      DELN=0.0                                                          SDM0707
      DELM=0.0                                                          SDM0708
      DO 780 I=1,7                                                      SDM0709
780   IFREQ(I)=0.0                                                      SDM0710
      DO 800 I=NB,NE                                                    SDM0711
      JHR=I                                                             SDM0712
      DAY2=IDATE(2)                                                     SDM0713
      IF (IOPT(5).EQ.0) GO TO 790                                       SDM0714
C                                                                       SDM0715
C        READ CARD TYPE 14 IF IOPT                                      SDM0716
C         (SEE DESCRIPTION, SECTION B).                                 SDM0717
C                                                                       SDM0718
      READ (IN,*) JYR,DAY1,JHR,IKST(JHR),QU(JHR),QTEMP(JHR),QTHETA(JH   SDM0719
     1R),QHL(JHR)                                                       SDM0720
      IF (I.NE.NB) GO TO 790                                            SDM0721
C        REDEFINE START HOURS AND DATES AT FIRST HOUR OF EACH           SDM0722
C         AVERAGING PERIOD IF READING HOURLY MET DATA.                  SDM0723
      IDATE(1)=JYR                                                      SDM0724
      IHSTRT=JHR                                                        SDM0725
      ISTDAY=DAY1                                                       SDM0726
      IDATE(2)=ISTDAY                                                   SDM0727
      DAY2=IDATE(2)                                                     SDM0728
790   IF (IKST(JHR).EQ.7) IKST(JHR)=6                                   SDM0729
      IF (IOPT(10).EQ.1) GO TO 800                                      SDM0730
C                                                                       SDM0731
C->->->->SECTION P - CALCULATE AND STORE FOR HIGH-FIVE TABLE.           SDM0732
C                                                                       SDM0733
      IF (I.EQ.NB) WRITE (IO,1690) IDATE                                SDM0734
      TRAD=QTHETA(JHR)*0.01745329                                       SDM0735
      WRITE (IO,1700) JHR,QTHETA(JHR),QU(JHR),QHL(JHR),QTEMP(JHR),IKST(JSDM0736
     1HR)                                                               SDM0737
      SINT=SIN(TRAD)                                                    SDM0738
      COST=COS(TRAD)                                                    SDM0739
C        CALCULATE WIND COMPONENTS                                      SDM0740
      URES=QU(JHR)                                                      SDM0741
      UR=URES*SINT                                                      SDM0742
      VR=URES*COST                                                      SDM0743
      DELM=DELM+UR                                                      SDM0744
      DELN=DELN+VR                                                      SDM0745
      TEMP=TEMP+QTEMP(JHR)                                              SDM0746
      U=U+URES                                                          SDM0747
      KST=IKST(JHR)                                                     SDM0748
      IFREQ(KST)=IFREQ(KST)+1                                           SDM0749
C        END LOOP TO READ ALL MET DATA FOR AVERAGING PERIOD.            SDM0750
800   CONTINUE                                                          SDM0751
      IF (IOPT(10).EQ.1) GO TO 860                                      SDM0752
C        CALCULATE RESULTANT  WIND DIRECTION THETA                      SDM0753
      DELN=DELN/NAVG                                                    SDM0754
      DELM=DELM/NAVG                                                    SDM0755
      THETA=ANGARC(DELM,DELN)                                           SDM0756
C        CALCULATE AVERAGE AND RESULTANT SPEED AND PERSISTENCE.         SDM0757
      U=U/NAVG                                                          SDM0758
      TEMP=TEMP/NAVG                                                    SDM0759
      URES=SQRT(DELN*DELN+DELM*DELM)                                    SDM0760
      PERSIS=URES/U                                                     SDM0761
C        DETERMINE MODEL AND AVERAGE STABILITY                          SDM0762
      LSMAX=0                                                           SDM0763
      DO 810 I=1,7                                                      SDM0764
      LST=IFREQ(I)                                                      SDM0765
      IF (LST.LE.LSMAX) GO TO 810                                       SDM0766
      LSMAX=LST                                                         SDM0767
      LSTAB=I                                                           SDM0768
810   CONTINUE                                                          SDM0769
      IP1=LSTAB+1                                                       SDM0770
      KST=LSTAB                                                         SDM0771
      DO 820 I=IP1,7                                                    SDM0772
      IF (LSMAX.EQ.IFREQ(I)) GO TO 830                                  SDM0773
820   CONTINUE                                                          SDM0774
      GO TO 850                                                         SDM0775
C        IF TIE FOR MAX MODEL STABILITY CALCULATE AVERAGE STABILITY     SDM0776
830   KSUM=0                                                            SDM0777
      DO 840 J=1,7                                                      SDM0778
840   KSUM=KSUM+IFREQ(J)*J                                              SDM0779
      KST=FLOAT(KSUM)/FLOAT(NAVG)+0.5                                   SDM0780
C        PRINT RESULTANT MET DATA SUMMARY FOR AVERAGING PERIOD.         SDM0781
850   WRITE (IO,1710)                                                   SDM0782
      WRITE (IO,1720) THETA,URES,U,TEMP,PERSIS,KST                      SDM0783
C        REDEFINE NB AND NE IN CASE NON-CONSECUTIVE DAYS ARE BEING RUN  SDM0784
860   IF (IOPT(5).EQ.0) GO TO 870                                       SDM0785
      NB=IHSTRT                                                         SDM0786
      NE=IHSTRT+NAVG-1                                                  SDM0787
C                                                                       SDM0788
C->->->->SECTION Q - INITIALIZE FOR HOURLY LOOP.                        SDM0789
C                                                                       SDM0790
C        INITIALIZE SUMS FOR CONC AND PARTIAL CONC FOR AVG PERIOD.      SDM0791
870   DO 890 K=1,NRECEP                                                 SDM0792
      PCHI(K)=0.0                                                       SDM0793
      DO 880 I=1,26                                                     SDM0794
880   PSIGS(K,I)=0.0                                                    SDM0795
890   CONTINUE                                                          SDM0796
C        IF SAVING PARTIAL CONCENTRATIONS, WRITE INITIAL RECEPTOR INFO. SDM0797
      IF (IOPT(21).EQ.0) GO TO 900                                      SDM0798
      WRITE (10) NRECEP,NPT,(RREC(I),I=1,NRECEP),(SREC(I),I=1,NRECEP)   SDM0799
C                                                                       SDM0800
C->->->->SECTION R - BEGIN HOURLY LOOP.                                 SDM0801
C                                                                       SDM0802
900   DO 1020 ILH=NB,NE                                                 SDM0803
      LH=ILH                                                            SDM0804
      IF (LH.LE.24) GO TO 910                                           SDM0805
      LH=MOD(ILH,24)                                                    SDM0806
      IF (LH.EQ.1) IDATE(2)=DAY1                                        SDM0807
C        INITIALIZE SUMS FOR CONC AND PARTIAL CONC FOR HOURLY PERIODS.  SDM0808
910   DO 930 K=1,NRECEP                                                 SDM0809
      PHCHI(K)=0.0                                                      SDM0810
      DO 920 I=1,26                                                     SDM0811
920   PHSIGS(K,I)=0.0                                                   SDM0812
930   CONTINUE                                                          SDM0813
C        SET MET CONDITIONS FOR THIS HOUR                               SDM0814
      THETA=QTHETA(LH)                                                  SDM0815
      U=QU(LH)                                                          SDM0816
      HL=QHL(LH)                                                        SDM0817
      TEMP=QTEMP(LH)                                                    SDM0818
      KST=IKST(LH)                                                      SDM0819
      TRAD=THETA*0.01745329                                             SDM0820
      SINT=SIN(TRAD)                                                    SDM0821
      COST=COS(TRAD)                                                    SDM0822
      CTER=CONTER(KST)                                                  SDM0823
C        IF OPTION 6 IS 1, READ HOURLY EMISSIONS.                       SDM0824
      IF (IOPT(6).EQ.0) GO TO 940                                       SDM0825
      IDCK=IDATE(1)*100000+IDATE(2)*100+LH                              SDM0826
      READ (15) IDATP,(SOURCE(IPOL,I),I=1,NPT)                          SDM0827
C        CHECK DATE                                                     SDM0828
      IF (IDCK.EQ.IDATP) GO TO 940                                      SDM0829
      WRITE (IO,1390) IDCK,IDATP                                        SDM0830
C     CALL WAUDIT                                                       SDM0831
      STOP                                                              SDM0832
C        CALCULATE POINT SOURCE CONTRIBUTIONS                           SDM0833
940   CALL PTR(IDAY,PNAME)                                              SDM0834
      IF (IOPT(22).EQ.0) GO TO 950                                      SDM0835
C        WRITE HOURLY CONCENTRATIONS TO TAPE                            SDM0836
      WRITE (12) IDATE(2),LH,(PHCHI(I),I=1,NRECEP)                      SDM0837
C                                                                       SDM0838
C->->->->SECTION S - CALCULATE AND STORE FOR HIGH-FIVE TABLE.           SDM0839
C                                                                       SDM0840
950   NHR=NHR+1                                                         SDM0841
C        IF OPTION 19 IS 1, DELETE COMPUTATIONS FOR AVG CONC.           SDM0842
C         FOR LENGTH OF RECORD AND HIGH-FIVE TABLE.                     SDM0843
      IF (IOPT(19).EQ.1) GO TO 1010                                     SDM0844
C        CUMULATE CONCENTRATIONS FOR AVG TIMES AND LENGTH OF RECORD.    SDM0845
C                                                                       SDM0846
C         FOR DEFAULT OPTION DETERMINE CALM HOURS.                      SDM0847
C         FOR CALM HOURS, CONCENTRATIONS AT EACH RECEPTOR ARE           SDM0848
C          SET EQUAL TO ZERO.                                           SDM0849
C         --- A CALM HOUR IS AN HOUR WITH A WIND SPEED                  SDM0850
C             OF 1.00 M/S AND A WIND DIRECTION THE SAME                 SDM0851
C              AS THE PREVIOUS HOUR.                                    SDM0852
      IF(IOPT(25).EQ.1.AND.QU(LH).LT.1.009.AND.ITMIN1.EQ.               SDM0853
     *IDUMR(LH))THEN                                                    SDM0854
      ICALM=ICALM+1                                                     SDM0855
      DO 955 K=1,NRECEP                                                 SDM0856
      PHCHI(K)=0.0                                                      SDM0857
955   CONTINUE                                                          SDM0858
      GO TO 971                                                         SDM0859
      END IF                                                            SDM0860
      DO 970 K=1,NRECEP                                                 SDM0861
      DO 960 L=1,NAVT                                                   SDM0862
960   CONC(K,L)=CONC(K,L)+PHCHI(K)                                      SDM0863
970   SUM(K)=SUM(K)+PHCHI(K)                                            SDM0864
C          STORE DATE FOR WHICH CONCS. HAVE BEEN CALCULATED.            SDM0865
                                                                        SDM0866
  971 JDAY=IDATE(2)                                                     SDM0867
C        SUBROUTINE RANK IS CALLED WHENEVER A COUNTER                   SDM0868
C        INDICATES THAT ENOUGH END TO END HOURLY CONCENTRATIONS         SDM0869
C        HAVE BEEN STORED OFF TO COMPLETE AN AVG TIME.                  SDM0870
C        NP3, NP8, NP24, NPX ARE USED AS COUNTERS FOR EACH              SDM0871
C        AVG TIME AND ARE ZEROED AFTER EACH CALL TO RANK.               SDM0872
C                                                                       SDM0873
C        FOR THE DEFAULT OPTION CALCULATE AVERAGE                       SDM0874
C        CONCENTRATION FOR APPROPRIATE AVERAGING PERIOD.                SDM0875
C        SET UP CALM FLAG FOR ENTRY INTO SUBROUTINE RANK.               SDM0876
C                                                                       SDM0877
C  DCD Modifications  11/15/90                                          SDM0878
C  Comment out references to MSFMHR, fumigation flag                    SDM0879
      IF(IOPT(25).EQ.0) GOTO 979                                        SDM0880
      LL1=1                                                             SDM0881
C     IF (MSFMHR(JDAY*24+LH).EQ.1) LL1=111                              SDM0882
      CALL RANK(LL1)                                                    SDM0883
      LL1=1                                                             SDM0884
      NP3=NP3+1                                                         SDM0885
      IF(QU(LH).LT.1.009.AND.IDUMR(LH).EQ.ITMIN1)ICFL3=1                SDM0886
c     IF (MSFMHR(JDAY*24+LH).EQ.1) IMFL3=1                              SDM0887
      IF(NP3.NE.3) GO TO 974                                            SDM0888
C        FOR 3 HOUR AVERAGING PERIOD DIVIDE SUM BY 3.0.                 SDM0889
      DO 972 LQ=1,NRECEP                                                SDM0890
972   CONC(LQ,2)=CONC(LQ,2)/3.0                                         SDM0891
      LL2=2                                                             SDM0892
      IF(ICFL3.EQ.1)LL2=22                                              SDM0893
C     IF (IMFL3.EQ.1)LL2=222                                            SDM0894
      CALL RANK(LL2)                                                    SDM0895
      NP3=0                                                             SDM0896
      ICFL3=0                                                           SDM0897
C     IMFL3=0                                                           SDM0898
974   NP8=NP8+1                                                         SDM0899
      IDIV8=IDIV8+1                                                     SDM0900
      IF(QU(LH).LT.1.009.AND.IDUMR(LH).EQ.ITMIN1) THEN                  SDM0901
      IDIV8=IDIV8-1                                                     SDM0902
      ICFL8=1                                                           SDM0903
      END IF                                                            SDM0904
C     IF (MSFMHR(JDAY*24+LH).EQ.1) IMFL8=1                              SDM0905
      IF(NP8.NE.8)GO TO 976                                             SDM0906
      IF(IDIV8.LT.6)IDIV8=6                                             SDM0907
      DIV8=IDIV8                                                        SDM0908
C        FOR 8 HOUR AVERAGING PERIOD DIVIDE THE SUM OF THE HOURLY       SDM0909
C        CONCENTRATIONS BY THE NUMBER OF NON-CALM HOURS OR 6.0          SDM0910
C        WHICHEVER IS GREATER.                                          SDM0911
      DO 975 LQ=1,NRECEP                                                SDM0912
975   CONC(LQ,3)=CONC(LQ,3)/DIV8                                        SDM0913
      LL3=3                                                             SDM0914
      IF(ICFL8.EQ.1)LL3=33                                              SDM0915
C     IF (IMFL8.EQ.1)LL3=333                                            SDM0916
      CALL RANK(LL3)                                                    SDM0917
      NP8=0                                                             SDM0918
      IDIV8=0                                                           SDM0919
      ICFL8=0                                                           SDM0920
C     IMFL8=0                                                           SDM0921
976   NP24=NP24+1                                                       SDM0922
      IDIV24=IDIV24+1                                                   SDM0923
      IF(QU(LH).LT.1.009.AND.IDUMR(LH).EQ.ITMIN1)THEN                   SDM0924
      IDIV24=IDIV24-1                                                   SDM0925
      ICFL24=1                                                          SDM0926
      END IF                                                            SDM0927
C     IF (MSFMHR(JDAY*24+LH).EQ.1) IMFL24=1                             SDM0928
      IF(NP24.NE.24)GO TO 1011                                          SDM0929
      IF(IDIV24.LT.18)IDIV24=18                                         SDM0930
      DIV24=IDIV24                                                      SDM0931
C        FOR 24 HOUR AVERAGING PERIOD DIVIDE THE SUM OF THE HOURLY      SDM0932
C        CONCENTRATIONS BY THE NUMBER OF NON-CALM HOURS OR 18.          SDM0933
C        WHICHEVER IS GREATER.                                          SDM0934
      DO 977 LQ=1,NRECEP                                                SDM0935
977   CONC(LQ,4)=CONC(LQ,4)/DIV24                                       SDM0936
      LL4=4                                                             SDM0937
      IF(ICFL24.EQ.1)LL4=44                                             SDM0938
C     IF (IMFL24.EQ.1) LL4=444                                          SDM0939
      CALL RANK(LL4)                                                    SDM0940
      NP24=0                                                            SDM0941
      IDIV24=0                                                          SDM0942
      ICFL24=0                                                          SDM0943
C     IMFL24=0                                                          SDM0944
1011  ITMIN1=IDUMR(LH)                                                  SDM0945
      GO TO 1010                                                        SDM0946
C                                                                       SDM0947
C        WHEN DEFAULT OPTION IS NOT USED, DETERMINE ENTRY INTO          SDM0948
C        SUBROUTINE RANK FOR APPROPRIATE AVERAGING PERIOD.              SDM0949
C        RANKING BASED ON HIGH AVERAGING PERIOD SUM.                    SDM0950
C                                                                       SDM0951
C979   IF (MSFMHR(JDAY*24+LH).EQ.1) L1=111                              SDM0952
979   CALL RANK (L1)                                                    SDM0953
      L1=1                                                              SDM0954
      NP3=NP3+1                                                         SDM0955
C     IF (MSFMHR(JDAY*24+LH).EQ.1) L2=222                               SDM0956
      IF (NP3.NE.3) GO TO 980                                           SDM0957
      CALL RANK (L2)                                                    SDM0958
      L2=2                                                              SDM0959
      NP3=0                                                             SDM0960
C980   IF (MSFMHR(JDAY*24+LH).EQ.1) L3=333                              SDM0961
980   NP8=NP8+1                                                         SDM0962
      IF (NP8.NE.8) GO TO 990                                           SDM0963
      CALL RANK (L3)                                                    SDM0964
      L3=3                                                              SDM0965
      NP8=0                                                             SDM0966
C990   IF (MSFMHR(JDAY*24+LH).EQ.1) L4=444                              SDM0967
990   NP24=NP24+1                                                       SDM0968
      IF (NP24.NE.24) GO TO 1000                                        SDM0969
      CALL RANK (L4)                                                    SDM0970
      L4=4                                                              SDM0971
      NP24=0                                                            SDM0972
1000  IF (NAVT.EQ.4) GO TO 1010                                         SDM0973
C     IF (MSFMHR(JDAY*24+LH).EQ.1) L5=555                               SDM0974
      NPX=NPX+1                                                         SDM0975
      IF (NPX.NE.NAV5) GO TO 1010                                       SDM0976
      CALL RANK (L5)                                                    SDM0977
      L5=5                                                              SDM0978
      NPX=0                                                             SDM0979
C                                                                       SDM0980
C->->->->SECTION T - END HOURLY, AVERAGING TIME, AND DAILY LOOPS.       SDM0981
C                                                                       SDM0982
1010  IF (IOPT(11).EQ.1.AND.IOPT(14).EQ.1) GO TO 1020                   SDM0983
C        IF BOTH OPTIONS 11 AND 14 CALL FOR OUTPUT DELETIONS,           SDM0984
C         SKIP HOURLY PRINTOUT.                                         SDM0985
      CALL OUTHR                                                        SDM0986
1020  CONTINUE                                                          SDM0987
C                                                                       SDM0988
C        END OF HOURLY LOOP                                             SDM0989
C                                                                       SDM0990
      IF (NE.GT.24) IDATE(2)=ISTDAY                                     SDM0991
C        OUTPUT FINAL RESULTS                                           SDM0992
      CALL OUTAVG                                                       SDM0993
      NP=NP+1                                                           SDM0994
      NHRS=NHRS+NAVG                                                    SDM0995
C        NEXT STATEMENT IS BRANCH FOR END OF RUN.                       SDM0996
      IF (NP.GE.NPER) GO TO 1050                                        SDM0997
      IF (NHRS.LT.24) GO TO 1030                                        SDM0998
C                                                                       SDM0999
C     ADDED FOR SHORELINE DISPERSION MODEL                              SDM1000
C                                                                       SDM1001
      DO 1021 I=1,NPT                                                   SDM1002
         PTEST=0.                                                       SDM1003
         DO 1022 J=1,24                                                 SDM1004
            PTEST=PTEST+MSFMFL(I,J)                                     SDM1005
 1022    CONTINUE                                                       SDM1006
         IF (PTEST.GT.0.) WRITE (IO,1023) (PNAME(J,I),J=1,3),IDAY,      SDM1007
     &   (MSFMFL(I,J),J=1,24)                                           SDM1008
         DO 1024 J=1,24                                                 SDM1009
            MSFMFL(I,J)=0                                               SDM1010
 1024    CONTINUE                                                       SDM1011
 1021 CONTINUE                                                          SDM1012
 1023 FORMAT ('  SOURCE ',3A4,' DAY',I3,'SHORELINE FUMIGATION HOURS',   SDM1013
     &  24(2X,I1))                                                      SDM1014
C                                                                       SDM1015
C                                                                       SDM1016
C                                                                       SDM1017
      IF (IOPT(20).EQ.0) GO TO 720                                      SDM1018
C        NEXT STATEMENT CHECKS FOR END OF SEGMENTED RUN.                SDM1019
      IF (IDAY.GE.LDRUN) GO TO 1040                                     SDM1020
      GO TO 720                                                         SDM1021
C                                                                       SDM1022
C          END OF LOOP FOR CALENDAR DAYS                                SDM1023
C                                                                       SDM1024
1030  NB=NB+NAVG                                                        SDM1025
      NE=NE+NAVG                                                        SDM1026
      IF (NB.LE.24) GO TO 770                                           SDM1027
      NB=MOD(NB,24)                                                     SDM1028
      NE=NB+NAVG-1                                                      SDM1029
      GO TO 770                                                         SDM1030
C                                                                       SDM1031
C        END OF LOOP FOR AVERAGING PERIOD.                              SDM1032
C                                                                       SDM1033
C        IF SEGMENTED RUN, TEMPORARILY STORE                            SDM1034
C         HIGH-FIVE INFO ON UNIT 14 FILE.                               SDM1035
1040  WRITE (14) IDAY,SUM,NHR,DAY1A,HR1,HMAXA,NDAY,IHR                  SDM1036
      WRITE (IO,1730) IDAY                                              SDM1037
      GO TO 1140                                                        SDM1038
1050  IF (IOPT(19).EQ.1) GO TO 1140                                     SDM1039
C                                                                       SDM1040
C->->->->SECTION U - WRITE AVERAGE CONC. AND HIGH-FIVE TABLES.          SDM1041
C                                                                       SDM1042
C        IF OPTION 19 = 0, WRITE AVERAGE CONCENTRATION.                 SDM1043
C         FOR LENGTH OF RECORD AND HIGH-FIVE TABLE.                     SDM1044
      DO 1060 J=1,NRECEP                                                SDM1045
      STAR(1,J)=BLNK                                                    SDM1046
      STAR(2,J)=BLNK                                                    SDM1047
1060  CONTINUE                                                          SDM1048
      WRITE (IO,1400)(MODEL(K,MUOR),K=1,2), LINE1,LINE2,LINE3           SDM1049
      HR2=NE                                                            SDM1050
C        FOR DEFAULT OPTION CALCULATE AND REPORT THE                    SDM1051
C        NUMBER OF CALMS FOR AVERAGING PERIOD.                          SDM1052
      IF(IOPT(25).EQ.1)THEN                                             SDM1053
      NHR=NHR-ICALM                                                     SDM1054
      WRITE(6,1061)ICALM                                                SDM1055
      END IF                                                            SDM1056
      SUM(1)=SUM(1)/NHR                                                 SDM1057
      HIMAX=SUM(1)                                                      SDM1058
      KMX=1                                                             SDM1059
C        INITIALIZE PERIODIC CONC TO BEGIN RANKING FOR PERIODIC MAX     SDM1060
      DO 1070 K=2,NRECEP                                                SDM1061
      SUM(K)=SUM(K)/NHR                                                 SDM1062
      IF (SUM(K).LE.HIMAX) GO TO 1070                                   SDM1063
      KMX=K                                                             SDM1064
      HIMAX=SUM(K)                                                      SDM1065
1070  CONTINUE                                                          SDM1066
      STAR(1,KMX)=STR                                                   SDM1067
C        FIND HIGHEST AVERAGE CONC. AMONG RECEPTORS.                    SDM1068
      WRITE (IO,1740) DAY1A,HR1,DAY2,HR2                                SDM1069
      DO 1080 K=1,NRECEP                                                SDM1070
1080  WRITE (IO,1750) K,(RNAME(J,K),J=1,2),RREC(K),SREC(K),ZR(K),ELR(K),SDM1071
     1STAR(1,K),SUM(K)                                                  SDM1072
      STAR(1,KMX)=BLNK                                                  SDM1073
C        LOOP TO WRITE HIGH-FIVE TABLE FOR 4 OR 5 AVG TIMES.            SDM1074
      DO 1130 L=1,NAVT                                                  SDM1075
C        ASTERISKS DEPICT RECEPTORS WITH HIGHEST AND                    SDM1076
C         SECOND HIGHEST CONCENTRATIONS.                                SDM1077
      K1=1                                                              SDM1078
      K2=1                                                              SDM1079
      HI1=HMAXA(1,1,L)                                                  SDM1080
      HI2=HMAXA(2,1,L)                                                  SDM1081
      DO 1100 K=2,NRECEP                                                SDM1082
      IF (HMAXA(1,K,L).LE.HI1) GO TO 1090                               SDM1083
      HI1=HMAXA(1,K,L)                                                  SDM1084
      K1=K                                                              SDM1085
1090  IF (HMAXA(2,K,L).LE.HI2) GO TO 1100                               SDM1086
      HI2=HMAXA(2,K,L)                                                  SDM1087
      K2=K                                                              SDM1088
1100  CONTINUE                                                          SDM1089
      STAR(1,K1)=STR                                                    SDM1090
      STAR(2,K2)=STR                                                    SDM1091
      IF((IOPT(25).EQ.1.AND.L.EQ.1).OR.(IOPT(25).NE.1))THEN             SDM1092
      WRITE (IO,1760) NTIME(L),TITLE(IP),(I,I=1,5)                      SDM1093
      END IF                                                            SDM1094
      IF(IOPT(25).EQ.1.AND.L.NE.1)THEN                                  SDM1095
      WRITE (IO,1761) NTIME(L),TITLE(IP),(I,I=1,5)                      SDM1096
      END IF                                                            SDM1097
      ZDUM=ATIME(L)                                                     SDM1098
      DO 1120 K=1,NRECEP                                                SDM1099
C        SET CALM FLAG FOR PRINTING.                                    SDM1100
C        RESET HOUR VARIABLE FOR CALM HOURS.                            SDM1101
      IF(IOPT(25).EQ.1)THEN                                             SDM1102
         DO 1112 J=1,5                                                  SDM1103
            IF(IHR(J,K,L).GT.24)THEN                                    SDM1104
               IHR(J,K,L)=IHR(J,K,L)-100                                SDM1105
               CF(J)=C                                                  SDM1106
            END IF                                                      SDM1107
1112     CONTINUE                                                       SDM1108
      END IF                                                            SDM1109
      DO 10000 J=1,5                                                    SDM1110
            CF(J)=BLNK                                                  SDM1111
            IF (IHR(J,K,L).GT.124) THEN                                 SDM1112
               IHR(J,K,L)=IHR(J,K,L)-200                                SDM1113
               CF(J)=FUME                                               SDM1114
            ENDIF                                                       SDM1115
10000 CONTINUE                                                          SDM1116
      IF(IOPT(25).EQ.1)GO TO 1111                                       SDM1117
C        CALCULATE AVERAGE CONCENTRATIONS WHEN                          SDM1118
C        DEFAULT OPTION IS NOT ON.                                      SDM1119
      DO 1110 J=1,5                                                     SDM1120
1110  HMAXA(J,K,L)=HMAXA(J,K,L)/ZDUM                                    SDM1121
 1111 WRITE (IO,1770) K,RREC(K),SREC(K),(STAR(J,K),HMAXA(J,K,L),CF(J),  SDM1122
     1NDAY(J,K,L),IHR(J,K,L),J=1,2),(HMAXA(J,K,L),CF(J),NDAY(J,K,L),    SDM1123
     2IHR(J,K,L),J=3,5)                                                 SDM1124
1120  CONTINUE                                                          SDM1125
C        INITIALIZE ASTERISK STORAGE TO BLANKS.                         SDM1126
      STAR(1,K1)=BLNK                                                   SDM1127
      STAR(2,K2)=BLNK                                                   SDM1128
1130  CONTINUE                                                          SDM1129
C                                                                       SDM1130
C->->->->SECTION V - CLOSE OUT FILES.                                   SDM1131
C                                                                       SDM1132
1140  IF (IOPT(21).EQ.0) GO TO 1150                                     SDM1133
      END FILE 10                                                       SDM1134
C     END FILE 10                                                       SDM1135
1150  IF (IOPT(22).EQ.0) GO TO 1160                                     SDM1136
      END FILE 12                                                       SDM1137
C     END FILE 12                                                       SDM1138
1160  IF (IOPT(23).EQ.0) GO TO 1170                                     SDM1139
      END FILE 13                                                       SDM1140
C     END FILE 13                                                       SDM1141
C     CALL WAUDIT                                                       SDM1142
1170  STOP                                                              SDM1143
C                                                                       SDM1144
C->->->->SECTION X - OUTLINE OF PROGRAM SECTIONS                        SDM1145
C                                                                       SDM1146
C        SECTION A - GENERAL REMARKS                                    SDM1147
C        SECTION B - DATA INPUT LISTS.                                  SDM1148
C        SECTION C - COMMON, DIMENSION, AND DATA STATEMENTS.            SDM1149
C        SECTION D - FLOW DIAGRAM.                                      SDM1150
C        SECTION E - RUN SET-UP AND READ FIRST 6 INPUT CARDS.           SDM1151
C        SECTION F - INPUT AND PROCESS EMISSION INFORMATION.            SDM1152
C        SECTION G - RANK SIGNIFICANT SOURCES.                          SDM1153
C        SECTION H - EMISSIONS WITH HEIGHT TABLE.                       SDM1154
C        SECTION I - EXECUTE FOR INPUT OF SIGNIFICANT SOURCE NUMBERS.   SDM1155
C        SECTION J - CHECK MET. DATA IF FROM FILE OF ONE YEARS'S DATA.  SDM1156
C        SECTION K - GENERATE POLAR COORDINATE RECEPTORS.               SDM1157
C        SECTION L - READ POLAR COORDINATE ELEVATIONS.                  SDM1158
C        SECTION M - READ AND PROCESS RECEPTOR INFORMATION.             SDM1159
C        SECTION N - POSITION FILES AS REQUIRED.                        SDM1160
C        SECTION O - START LOOPS FOR DAY AND AVERAGING TIME; READ       SDM1161
C                       MET. DATA.                                      SDM1162
C        SECTION P - CALCULATE AND WRITE MET. SUMMARY INFORMATION.      SDM1163
C        SECTION Q - INITIALIZE FOR HOURLY LOOP.                        SDM1164
C        SECTION R - BEGIN HOURLY LOOP.                                 SDM1165
C        SECTION S - CALCULATE AND STORE FOR HIGH-FIVE TABLE.           SDM1166
C        SECTION T - END HOURLY, AVERAGING TIME, AND DAILY LOOPS.       SDM1167
C        SECTION U - WRITE AVERAGE CONC. AND HIGH-FIVE TABLES.          SDM1168
C        SECTION V - CLOSE OUT FILES.                                   SDM1169
C        SECTION W - FORMAT STATEMENTS.                                 SDM1170
C        SECTION X - OUTLINE OF PROGRAM SECTIONS.                       SDM1171
C        SECTION Y - INPUT AND OUTPUT FILE DESCRIPTIONS.                SDM1172
C        SECTION Z - INDEX AND GLOSSARY.                                SDM1173
C                                                                       SDM1174
C                                                                       SDM1175
C->->->-> SECTION Y -  INPUT AND OUTPUT FILE DESCRIPTIONS.              SDM1176
C                                                                       SDM1177
C***     INPUT AND OUTPUT FILE DESCRIPTIONS.                            SDM1178
C                                                                       SDM1179
C***  INPUT FILE (UNIT 11) METEOROLOGICAL DATA (USED IF IOPT(5)=0)      SDM1180
C                                                                       SDM1181
C      RECORD 1                                                         SDM1182
C                                                                       SDM1183
C        ID          SFC STATION IDENTIFIER, 5 DIGITS                   SDM1184
C        IYEAR       YEAR OF SURFACE DATA, 2 DIGITS                     SDM1185
C        IDM         MIX HT STATION IDENTIFIER, 5 DIGITS                SDM1186
C        IYR         YEAR OF MIX HT DATA, 2 DIGITS                      SDM1187
C                                                                       SDM1188
C      RECORD TYPE 2 (ONE FOR EACH DAY OF YEAR)                         SDM1189
C                                                                       SDM1190
C        JYR         YEAR                                               SDM1191
C        IMO         MONTH                                              SDM1192
C        DAY1        JULIAN DAY                                         SDM1193
C        IKST(24)    STABILITY CLASS                                    SDM1194
C        QU(24)      WIND SPEED, METERS PER SECOND                      SDM1195
C        QTEMP(24)   AMBIENT AIR TEMPERATURE, KELVIN                    SDM1196
C        DUMR(24)    FLOW VECTOR TO 10 DEG, DEGREES AZIMUTH             SDM1197
C        QTHETA(24)  RANDOMIZED FLOW VECTOR, DEGREES AZIMUTH            SDM1198
C        HLH(2,24)   MIXING HEIGHT, METERS                              SDM1199
C                                                                       SDM1200
C***  INPUT FILE(UNIT 15) EMISSION DATA (USED IF IOPT(6)=1)             SDM1201
C                                                                       SDM1202
C      RECORD TYPE 1 (ONE FOR EACH HOUR OF SIMULATION)                  SDM1203
C                                                                       SDM1204
C        IDATP       DATE-TIME INDICATOR CONSISTING OF YEAR, JULIAN DAY,SDM1205
C                     AND HOUR: YYDDDHH.                                SDM1206
C        SOURCE(IPOL,I),I=1,NPT  EMISSION RATE FOR THE POLLUTANT IPOL   SDM1207
C                                 FOR EACH SOURCE, GRAMS PER SECOND.    SDM1208
C                                                                       SDM1209
C***  OUTPUT PUNCHED CARDS (UNIT 1) AVERAGE CONCENTRATIONS (PUNCHED IF  SDM1210
C                                    IOPT(24)=1)                        SDM1211
C                                                                       SDM1212
C      CARD TYPE 1 (ONE FOR EACH RECEPTOR FOR EACH AVERAGING TIME)      SDM1213
C                                                                       SDM1214
C        CC:1-4      WORD'CNTL' PUNCHED                                 SDM1215
C        CC:5        BLANK                                              SDM1216
C        CC:6-15     RREC  EAST COORDINATE OF RECEPTOR, USER UNITS      SDM1217
C        CC:16-25    SREC  NORTH COORDINATE OF RECEPTOR, USER UNITS     SDM1218
C        CC:26-35    GWU   CONCENTRATION FOR AVERAGING TIME, MICROG/M**3SDM1219
C        CC:36-55    BLANK                                              SDM1220
C        CC:56-59    K     RECEPTOR NUMBER                              SDM1221
C        CC:60-69    ZR    RECEPTOR HEIGHT ABOVE GROUND, METERS         SDM1222
C        CC:70-79    ELR  RECEPTOR GROUND-LEVEL ELEVATION, USER HT UNITSSDM1223
C                                                                       SDM1224
C***  OUTPUT FILE (UNIT 10) PARTIAL CONCENTRATIONS (USED IF IOPT(21)=1) SDM1225
C                                                                       SDM1226
C     RECORD TYPE 1                                                     SDM1227
C                                                                       SDM1228
C        NPER        NUMBER OF PERIODS                                  SDM1229
C        NAVG        NUMBER OF HOURS IN AVERAGING PERIOD.               SDM1230
C        LINE1(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              SDM1231
C        LINE2(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              SDM1232
C        LINE3(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              SDM1233
C                                                                       SDM1234
C     RECORD TYPE 2 (FROM MPTER) (ONE FOR EACH AVERAGING PERIOD)        SDM1235
C                                                                       SDM1236
C        NRECEP      NUMBER OF RECEPTORS                                SDM1237
C        NPT         NUMBER OF SOURCES                                  SDM1238
C        RREC(I),I=1,NRECEP  EAST COORDINATE OF RECEPTOR, USER UNITS    SDM1239
C        SREC(I),I=1,NRECEP  NORTH COORDINATE OF RECEPTOR, USER UNITS   SDM1240
C                                                                       SDM1241
C      RECORD TYPE 3 (ONE FOR EACH RECEPTOR FOR EACH SIMULATED HOUR,    SDM1242
C                      FROM PTR)                                        SDM1243
C                                                                       SDM1244
C        IDATE       YEAR AND JULIAN DAY                                SDM1245
C        LH          HOUR                                               SDM1246
C        K           RECEPTOR NUMBER                                    SDM1247
C        PARTC(J),J=1,NPT  CONCENTRATION AT RECEPTOR K FROM SOURCE J,   SDM1248
C                           G/M**3.                                     SDM1249
C                                                                       SDM1250
C***  OUTPUT FILE (UNIT 12) HOURLY CONCENTRATIONS (USED IF IOPT(22)=1)  SDM1251
C                                                                       SDM1252
C     RECORD 1                                                          SDM1253
C                                                                       SDM1254
C        NPER        NUMBER OF PERIODS                                  SDM1255
C        NAVG        NUMBER OF HOURS IN AVERAGING PERIOD.               SDM1256
C        LINE1(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              SDM1257
C        LINE2(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              SDM1258
C        LINE3(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              SDM1259
C                                                                       SDM1260
C      RECORD 2                                                         SDM1261
C                                                                       SDM1262
C        NRECEP      NUMBER OF RECEPTORS.                               SDM1263
C        RREC(I),I=1,NRECEP  EAST COORDINATE OF RECEPTOR, USER UNITS    SDM1264
C        SREC(I),I=1,NRECEP  NORTH COORDINATE OF RECEPTOR, USER UNITS   SDM1265
C                                                                       SDM1266
C      RECORD TYPE 3 (ONE FOR EACH SIMULATED HOUR)                      SDM1267
C                                                                       SDM1268
C        IDATE(2)    JULIAN DAY                                         SDM1269
C        LH          HOUR                                               SDM1270
C        PHCHI(I),I=1,NRECEP  HOURLY CONCENTRATION FOR EACH RECEPTOR,   SDM1271
C                              G/M**3.                                  SDM1272
C                                                                       SDM1273
C***  OUTPUT FILE (UNIT 13) AVERAGING-PERIOD CONCENTRATIONS (USED IF    SDM1274
C                            IOPT(23)=1)                                SDM1275
C                                                                       SDM1276
C      RECORD 1                                                         SDM1277
C                                                                       SDM1278
C        NPER        NUMBER OF PERIODS                                  SDM1279
C        NAVG        NUMBER OF HOURS IN AVERAGING PERIOD.               SDM1280
C        LINE1(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              SDM1281
C        LINE2(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              SDM1282
C        LINE3(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              SDM1283
C                                                                       SDM1284
C      RECORD 2                                                         SDM1285
C                                                                       SDM1286
C        NRECEP      NUMBER OF RECEPTORS.                               SDM1287
C        RREC(I),I=1,NRECEP  EAST COORDINATE OF RECEPTOR, USER UNITS    SDM1288
C        SREC(I),I=1,NRECEP  NORTH COORDINATE OF RECEPTOR, USER UNITS   SDM1289
C                                                                       SDM1290
C      RECORD TYPE 3 (ONE FOR EACH SIMULATED AVERAGING PERIOD)          SDM1291
C                                                                       SDM1292
C        IDATE(2)    JULIAN DAY                                         SDM1293
C        NB          ENDING HOUR OF PERIOD                              SDM1294
C        PCHI(K),K=1,NRECEP  AVERAGING PERIOD CONCENTRATION FOR EACH    SDM1295
C                             RECEPTOR, G/M**3.                         SDM1296
C                                                                       SDM1297
C***  TEMPORARY FILE (UNIT 14) VALUES FOR HIGH-FIVE TABLES (USED IF     SDM1298
C                                IOPT(20)=1)                            SDM1299
C                                                                       SDM1300
C      ONLY RECORD                                                      SDM1301
C                                                                       SDM1302
C        NDAY(ON WRITE)    NUMBER OF DAYS PROCESSED                     SDM1303
C        IDAYS(ON READ     NUMBER OF DAYS PREVIOUSLY PROCESSED          SDM1304
C        SUM(180)          CUMULATION OF LONG-TERM CONCENTRATION,(G/M**3SDM1305
C        NHR               NUMBER OF HOURS PROCESSED                    SDM1306
C        DAY1A             JULIAN DAY OF START OF LENGTH OF RECORD.     SDM1307
C        HR1               START HOUR OF LENGTH OF RECORD               SDM1308
C        HMAXA(3,5,180,5)  HIGHEST FIVE CONCENTRATIONS (G/M**3), AND    SDM1309
C                           ASSOCIATED DAY AND HOUR, FOR EACH RECEPTOR, SDM1310
C                           FOR FIVE DIFFERENT AVERAGING TIMES.         SDM1311
C                                                                       SDM1312
C->->->->SECTION W - FORMAT STATEMENTS.                                 SDM1313
C                                                                       SDM1314
C        INPUT FORMATS                                                  SDM1315
C                                                                       SDM1316
1061  FORMAT(5X,T98,'# CALMS FOR PERIOD: ',I4)                          SDM1317
1180  FORMAT (20A4/20A4/20A4)                                           SDM1318
1200  FORMAT (A4)                                                       SDM1319
1210  FORMAT (3A4,8F8.2,F4.0)                                           SDM1320
1220  FORMAT (26I3)                                                     SDM1321
1230  FORMAT (I2,8X,5F10.0)                                             SDM1322
1240  FORMAT (2A4,2F10.3,2F10.0)                                        SDM1323
C                                                                       SDM1324
C        ERROR STATEMENT FORMATS                                        SDM1325
C                                                                       SDM1326
1250  FORMAT (1X,' NSIGP (THE NO. OF SIGNF POINT SOURCES) WAS FOUND',' TSDM1327
     1O EXCEED THE LIMIT (25).  USER TRIED TO INPUT ',I3,' SOURCES'/'   SDM1328
     2 *********EXECUTION TERMINATED**********')                        SDM1329
1260  FORMAT (1H0,'CONTER VALUE IS OUTSIDE OF RANGE: ','ZERO TO ONE. EXESDM1330
     1CUTION TERMINATED.')                                              SDM1331
1270  FORMAT (' USER TRIED TO INPUT MORE THAN ',I4,' POINT SOURCES. THISSDM1332
     1 GOES BEYOND THE CURRENT PROGRAM DIMENSIONS.')                    SDM1333
1280  FORMAT (1X,'NPT =  ',I3,'I.E., EQUAL OR LESS THAN ZERO'/' RUN TERMSDM1334
     1INATED----CHECK INPUT DATA')                                      SDM1335
1290  FORMAT (1H1,'***ERROR---USER TRIED TO SPECIFY ',I4,' SIGNIFICANT SSDM1336
     1OURCES, BUT IS ONLY ALLOWING ',I3,' TOTAL SIGNIFICANT SOURCES IN TSDM1337
     2HIS RUN.',/2X,'***RUN TERMINATED-CHECK INPUT DATA!***')           SDM1338
1300  FORMAT (' (MPS) THE INPUT SIGNIFICANT SOURCE NUMBER ','WAS FOUND TSDM1339
     1O EQUAL ZERO - USER CHECK INPUT DATA.')                           SDM1340
1310  FORMAT (' SURFACE DATA IDENTIFIERS READ INTO MODEL (STATION=',I5,'SDM1341
     1 ,YEAR=',I2,') DO NOT AGREE WITH THE PREPROCESSOR OUTPUT FILE',/1XSDM1342
     2,' (STATION=',I5,' ,YEAR=',I2)                                    SDM1343
1320  FORMAT (' MIXING HEIGHT IDENTIFIERS READ INTO MODEL (STATION=',I5,SDM1344
     1' ,YEAR=',I2,') DO NOT AGREE WITH THE PREPROCESSOR OUTPUT FILE',/1SDM1345
     2X,' (STATION=',I5,' ,YEAR=',I2)                                   SDM1346
1330  FORMAT (1H0,' WRONG RECEPTOR ELEVATION CARD READ.','READ CARD FOR SDM1347
     1AZIMUTH ',I3,' SHOULD HAVE BEEN ',I3,'.')                         SDM1348
1340  FORMAT (1X,'****USER EITHER TRIED TO INPUT MORE THAN 180 ','RECEPTSDM1349
     1ORS OR ENDREC WAS NOT PLACED AFTER THE LAST RECEPTOR ','CARD****'/SDM1350
     2'********EXECUTION TERMINATED*******')                            SDM1351
1350  FORMAT (1X,'NO RECEPTORS HAVE BEEN CHOSEN')                       SDM1352
1360  FORMAT (1H0,'***DAYS DO NOT MATCH, IDAY = ',I4,', IDAYS = ',I4)   SDM1353
1370  FORMAT (' DATE ON MET. TAPE, ',I2,I3,' ,DOES NOT MATCH INTERNAL DASDM1354
     1TE, ',I2,I3)                                                      SDM1355
1380  FORMAT (' HOUR ',I3,' IS NOT PERMITTED. HOURS MUST BE DEFINED BETWSDM1356
     1EEN 1 AND 24')                                                    SDM1357
1390  FORMAT (' DATE BEING PROCESSED IS= ',I8/1X,'DATE OF HOURLY POINT ESDM1358
     1MISSION RECORD IS =',I8/1X,'***PLEASE CHECK EMISSION RECORDS***') SDM1359
C                                                                       SDM1360
C        OUTPUT FORMATS                                                 SDM1361
C                                                                       SDM1362
 1395 FORMAT ('0',T35,A4,A1,1X,'SDM - VERSION 90320'/1X,20A4/1X,20A4/   SDM1363
     *1X,20A4)                                                          SDM1364
 1400 FORMAT ('1',T40,A4,A1,1X,'SDM - VERSION 90320'/1X,20A4/1X,20A4/   SDM1365
     *1X,20A4)                                                          SDM1366
1410  FORMAT (1H0,T30,'GENERAL INPUT INFORMATION'//2X,'THIS RUN OF SDM  SDM1367
     1-VERSION 90320 IS FOR ','THE POLLUTANT ',A4,' FOR ',I3,1X,I3,'-HOUSDM1368
     2R PERIODS.'/2X,'CONCENTRATION ESTIMATES BEGIN ON HOUR-',I2,', JULISDM1369
     3AN DAY-',I3,', YEAR-19',I2,'.'/1X,' A FACTOR OF ',F14.7,' HAS BEENSDM1370
     4 SPECIFIED TO ','CONVERT USER LENGTH UNITS TO KILOMETERS.'/1X,I3,'SDM1371
     5 SIGNIFICANT SOURCES ARE TO BE CONSIDERED.')                      SDM1372
1420  FORMAT (1H ,'THIS RUN WILL NOT CONSIDER ANY POLLUTANT LOSS.')     SDM1373
1430  FORMAT (1H ,2X,'A HALF-LIFE OF ',F10.2,' (SECONDS) HAS BEEN ASSUMESDM1374
     1D BY THE USER.')                                                  SDM1375
1440  FORMAT (1X,' HIGH-FIVE SUMMARY CONCENTRATION TABLES ','WILL BE OUTSDM1376
     1PUT FOR ',I3,' AVERAGING PERIODS.'/'  AVG TIMES ','OF 1,3,8, AND 2SDM1377
     24 HOURS ARE AUTOMATICALLY DISPLAYED.')                            SDM1378
1450  FORMAT (1H ,2X,'A FACTOR OF ',F14.7,' HAS BEEN SPECIFIED TO CONVERSDM1379
     1T USER HEIGHT UNITS TO METERS.')                                  SDM1380
1460  FORMAT (1H0,T3,'OPTION  ',T16,'OPTION LIST',T46,'OPTION SPECIFICATSDM1381
     1ION : 0= IGNORE OPTION'/1X,T68,' 1= USE OPTION'/T25,'TECHNICAL OPTSDM1382
     2IONS'/1X,T7,I2,T16,'TERRAIN ADJUSTMENTS',T70,I1/1X,T7,I2,T16,'DO NSDM1383
     3OT INCLUDE STACK DOWNWASH CALCULATIONS',T70,I1/1X,T7,I2,T16,'DO NOSDM1384
     4T INCLUDE GRADUAL PLUME RISE CALCULATIONS',T70,I1/1X,T7,I2,T16,'CASDM1385
     5LCULATE INITIAL PLUME SIZE',T70,I1/1X,T25,'INPUT OPTIONS'/1X,T7,I2SDM1386
     6,T16,'READ MET DATA FROM CARDS',T70,I1/1X,T7,I2,T16,'READ HOURLY ESDM1387
     7MISSIONS',T70,I1/1X,T7,I2,T16,'SPECIFY SIGNIFICANT SOURCES',T70,I1SDM1388
     8/1X,T7,I2,T16,'READ RADIAL DISTANCES TO GENERATE RECEPTOR',T70,I1 SDM1389
     9/T25,'PRINTED OUTPUT OPTIONS'/1X,T7,I2,T16,'DELETE EMISSIONS WITH SDM1390
     AHEIGHT TABLE',T70,I1/1X,T7,I2,T16,'DELETE MET DATA SUMMARY FOR AVGSDM1391
     B PERIOD',T70,I1/1X,T7,I2,T16,'DELETE HOURLY CONTRIBUTIONS',T70,I1/SDM1392
     C1X,T7,I2,T16,'DELETE MET DATA ON HOURLY CONTRIBUTIONS',T70,I1/1X,TSDM1393
     D7,I2,T16,'DELETE FINAL PLUME RISE CALC ON HRLY CONTRIBUTIONS',T70,SDM1394
     EI1)                                                               SDM1395
1470  FORMAT (1X,T7,I2,T16,'DELETE HOURLY SUMMARY',T70,I1/1X,T7,I2,T16,'SDM1396
     1DELETE MET DATA ON HRLY SUMMARY',T70,I1/1X,T7,I2,T16,'DELETE FINALSDM1397
     2 PLUME RISE CALC ON HRLY SUMMARY',T70,I1/1X,T7,I2,T16,'DELETE AVG-SDM1398
     3PERIOD CONTRIBUTIONS',T70,I1/1X,T7,I2,T16,'DELETE AVERAGING PERIODSDM1399
     4 SUMMARY',T70,I1/1X,T7,I2,T16,'DELETE AVG CONCENTRATIONS AND HI-5 SDM1400
     5TABLES',T70,I1/T25,'OTHER CONTROL AND OUTPUT OPTIONS'/1X,T7,I2,T16SDM1401
     6,'RUN IS PART OF A SEGMENTED RUN',T70,I1/1X,T7,I2,T16,'WRITE PARTISDM1402
     7AL CONC TO DISK OR TAPE',T70,I1/1X,T7,I2,T16,'WRITE HOURLY CONC TOSDM1403
     8 DISK OR TAPE',T70,I1/1X,T7,I2,T16,'WRITE AVG-PERIOD CONC TO DISK SDM1404
     9OR TAPE',T70,I1/1X,T7,I2,T16,'PUNCH AVG-PERIOD CONC ONTO CARDS',T7SDM1405
     A0,I1/T25,'DEFAULT  OPTION '/1X,T7,I2,T16,                         SDM1406
     B'USE DEFAULT OPTION',T70,I1/1X,T7,I2,T16,'TOWER.BIN FORMAT (0=BINASDM1407
     CRY,1=ASCII)',T70,I1)                                              SDM1408
1480  FORMAT (1H0,2X,'ANEMOMETER HEIGHT= ',F10.2/3X,'WIND PROFILE WITH 'SDM1409
     1,'HEIGHT EXPONENTS CORRESPONDING TO STABILITY ARE AS FOLLOWS:'/8X,SDM1410
     2'FOR STABILITY A: ',F4.2/12X,'STABILITY B: ',F4.2/12X,'STABILITY CSDM1411
     3: ',F4.2,/12X,'STABILITY D: ',F4.2,/12X,'STABILITY E: ',F4.2/12X,'SDM1412
     4STABILITY F: ',F4.2)                                              SDM1413
1490  FORMAT (1H0,'ANEMOMETER HEIGHT IS:',F10.2/1X,'EXPONENTS FOR POWER-SDM1414
     1 LAW WIND INCREASE WITH HEIGHT ARE:',F4.2,5(',',F4.2)/' TERRAIN ADSDM1415
     2JUSTMENTS ARE: ',F5.3,5(',',F5.3)//)                              SDM1416
1500  FORMAT ('1',T40,'POINT SOURCE INFORMATION'//1X,T5,'SOURCE',T23,'EASDM1417
     1ST',T31,'NORTH',T39,'SO2(G/SEC)  PART(G/SEC)  STACK  STACK   STACKSDM1418
     2   STACK',3X,'POTEN. IMPACT',2X,'EFF',3X,'GRD-LVL BUOY FLUX'/1X,T2SDM1419
     33,'COORD',T31,'COORD','   EMISSIONS   EMISSIONS    HT(M) TEMP(K) DSDM1420
     4IAM(M)','VEL(M/SEC)(MICRO G/M**3) HT(M)',3X,'ELEV',6X,'F'/1X,T24,'SDM1421
     5(USER UNITS)',T116,'USER HT M**4/S**3'/1X,T117,'UNITS'/)          SDM1422
1510  FORMAT (1X,I3,1X,3A4,1X,2F9.2,2F12.2,4F8.2,6PF13.2,0PF9.2,2F9.2)  SDM1423
1520  FORMAT ('0',T3,'SIGNIFICANT ',A4,' POINT SOURCES'//1X,T8,'RANK',T2SDM1424
     12,'CHI-MAX',T33,'SOURCE NO.'/1X,T17,'(MICROGRAMS/M**3)'/1X)       SDM1425
1530  FORMAT (1X,T9,I3,T18,6PF12.2,T35,I3)                              SDM1426
1540  FORMAT (1X,'HEIGHT ABOVE 100M FOR POINT SOURCE',I4,3X,' HEIGHT=',FSDM1427
     16.2,' (METERS)','   EMISSIONS=',F10.2,' (G/SEC)')                 SDM1428
1550  FORMAT ('0',4X,'TOTAL ',A4,' EMISSION AND CUMULATIVE FRACTION ACCOSDM1429
     1RDING TO HEIGHT'//1X,T12,'TOTAL POINT   CUMULATIVE '/1X,'HEIGHT(M)SDM1430
     2 EMISSIONS(G/S)  FRACTION'/1X)                                    SDM1431
1560  FORMAT (1X,T2,I2,' -',I3,T11,F8.2,T26,F7.3,T41,F8.2,T56,F7.3)     SDM1432
1570  FORMAT ('0',T2,'TOTAL',2X,F10.2)                                  SDM1433
1580  FORMAT (1H0,21X,'ADDITIONAL INFORMATION ON SOURCES.')             SDM1434
1590  FORMAT (1H0,'  USER SPECIFIED ',I3,' (NPT) SIGNIFICANT POINT ','SOSDM1435
     1URCES AS LISTED BY POINT SOURCE NUMBER:'/2X,25I5)                 SDM1436
1600  FORMAT ('0',2X,'EMISSION INFORMATION FOR ',I4,' (NPT) POINT SOUR',SDM1437
     1'CES HAS BEEN INPUT'/2X,I2,' SIGNIFICANT POINT SOURCES(NSIGP) ','ASDM1438
     2RE TO BE',' USED FOR THIS RUN'/2X,'THE ORDER OF SIGNIFICANCE(IMPS)SDM1439
     3 FOR 25 OR LESS POINT SOURCES USED IN THIS RUN AS LISTED BY POINT SDM1440
     4SOURCE NUMBER:'/2X,25I5)                                          SDM1441
1610  FORMAT (2X,'SURFACE MET DATA FROM STATION(ISFCD) ',I6,', YEAR(ISFCSDM1442
     1YR) 19',I2/2X,'MIXING HEIGHT DATA FROM STATION(IMXD) ',I6,', YEAR(SDM1443
     2IMXYR) 19',I2)                                                    SDM1444
1620  FORMAT (1H0,T21,'RECEPTOR INFORMATION')                           SDM1445
1630  FORMAT (1H0,' SDM INTERNALLY GENERATES 36 RECEPTORS ','ON A CIRCLESDM1446
     1 CORRESPONDING TO EACH NON-ZERO ','RADIAL DISTANCE FROM A CENTERP SDM1447
     2OINT '/1X,T10,'COORDINATES ARE (USER UNITS): (',F8.3,',',F8.3,')' SDM1448
     3/1X,T10,'RADIAL DISTANCE(S) USER SPECIFIED (USER UNITS): ',5(F11.3SDM1449
     4,'  '))                                                           SDM1450
1640  FORMAT (F4.1)                                                     SDM1451
1650  FORMAT ('0',' RECEPTOR    IDENTIFICATION  EAST     NORTH     RECEPSDM1452
     1TOR HT     RECEPTOR GROUND LEVEL'/1X,T30,'COORD',T39,'COORD  ABV LSDM1453
     2OCAL GRD LVL        ELEVATION'/1X,T31,'(USER UNITS)         (METERSDM1454
     3S)          (USER HT UNITS)'/1X)                                  SDM1455
1660  FORMAT (1X,T3,I3,2A1,8X,2A4,F13.3,F10.3,F10.1,F20.1)              SDM1456
1670  FORMAT (1H0,T3,'* ONE ASTERISK INDICATES THAT THE ASSOCIATED ','RESDM1457
     1CEPTOR(S) HAVE A GROUND LEVEL ELEVATION LOWER ','THAN THE LOWEST SSDM1458
     2OURCE BASE ELEVATION.'/' CAUTION SHOULD ','BE USED IN INTERPRETINGSDM1459
     3 CONCENTRATIONS FOR THESE RECEPTORS.'/' **  TWO ASTERISKS ','INDICSDM1460
     4ATE THAT THE ASSOCIATED RECEPTOR(S) HAVE GROUND LEVEL ','ELEVATIONSDM1461
     5S ABOVE THE LOWEST STACK TOP.'/'    CONSEQUENTLY',' NO CALCULATIONSDM1462
     6S WILL BE PERFORMED WITH THIS RECEPTOR.A ','SERIES OF ASTERISKS WISDM1463
     7LL INSTEAD APPEAR IN THE OUTPUT.')                                SDM1464
1680  FORMAT (//1X,' THE NUMBER OF   DAYS  PREVIOUSLY COMPLETED EQUAL ',SDM1465
     1I3,' AND THE LAST   DAY  TO BE COMPLETED IN THIS RUN IS ',I3)     SDM1466
1690  FORMAT ('1INPUT MET DATA  ',I2,'/',I4/1X,T2,'HOUR   THETA    SPEEDSDM1467
     1   MIXING   TEMP     STABILITY'/1X,T9,'(DEG)    (M/S)  HEIGHT(M) (SDM1468
     2DEG-K)  CLASS'/1X)                                                SDM1469
1700  FORMAT (1X,T3,I2,4F9.2,6X,I1)                                     SDM1470
1710  FORMAT ('0','RESULTANT MET CONDITIONS'/1X)                        SDM1471
1720  FORMAT (2X,'WIND DIRECTION=',F7.2,T36,'RESULTANT WIND SPEED=',F7.2SDM1472
     1/2X,'AVERAGE WIND SPEED=',F7.2,T36,'AVERAGE TEMP=',F7.2/2X,'WIND PSDM1473
     2ERSISTENCE=',F6.3,T36,'MODEL STABILITY=',I2)                      SDM1474
1730  FORMAT (1H0,' THIS SEGMENT OF A SEGMENTED RUN HAS COMPLETED',I5,' SDM1475
     1(IDAY) DAYS.')                                                    SDM1476
1740  FORMAT ('0',T9,'      RECEPTORS'//1X,'RECEPTOR    IDENTIFICATION  SDM1477
     1EAST     NORTH     RECEPTOR HT     RECEPTOR GROUND LEVEL',T99,'AVGSDM1478
     2 CONC FOR PERIOD'/1X,T30,'COORD',T39,'COORD  ABV LOCAL GRD LVL    SDM1479
     3   ELEVATION',T94,'DAY',F4.0,'HR',F3.0,' TO DAY',F4.0,'HR',F3.0/1XSDM1480
     4,T31,'(USER UNITS)        (METERS)         (USER HT UNITS)',T100,'SDM1481
     5(MICROGRAMS/M**3)'/1X)                                            SDM1482
1750  FORMAT (1X,T3,I3,10X,2A4,5X,F8.2,2X,F8.2,F10.1,F20.1,T110,A1,6PF7.SDM1483
     12)                                                                SDM1484
1760  FORMAT (1H1,T29,'FIVE HIGHEST ',I2,'-HOUR ',A4,' CONCENTRATIONS((ESDM1485
     1NDING ON JULIAN DAY, HOUR)'/1X,T55,'(MICROGRAMS/M**3)'//2X,'RECEPTSDM1486
     2OR ',T38,4(I1,20X),I1,/1X)                                        SDM1487
1761  FORMAT (1H1,T29,'FIVE HIGHEST ',I2,'-HOUR ',A4,' CONCENTRATIONS((ESDM1488
     1NDING ON JULIAN DAY, HOUR)'/1X,T55,'(MICROGRAMS/M**3)'/           SDM1489
     21X,T36,'C-FLAG IDENTIFIES CONCENTRATIONS AFFECTED BY CALM HOURS'//SDM1490
     32X,'RECEPTOR ',T38,4(I1,20X),I1,/1X)                              SDM1491
1770  FORMAT (1H ,2X,I3,'(',F7.2,',',F7.2,')',2(1X,A1,6PF9.2,A1,1X,'(',ISDM1492
     13,',',I2,')'),3(2X,6PF9.2,A1,1X,'(',I3,',',I2,')'))               SDM1493
 2531 FORMAT(F5.1)                                                      SDM1494
 2532 FORMAT(F5.2)                                                      SDM1495
 2533 FORMAT(F5.3)                                                      SDM1496
 3011 FORMAT(/,39X,'SHORELINE DEFINITION',//,' DESCRIPTOR',9X,'EAST',7X,SDM1497
     1 'NORTH',4X,'BEGINNING',5X,'ENDING',6X,'FETCH',/,19X,'COORD',     SDM1498
     2 7X,'COORD',6X,'ANGLE',8X,'ANGLE',6X,'(DEG)',/,                   SDM1499
     3 22X,'(USER UNITS)',8X,'(DEG)',8X,'(DEG)',//,3A4,2X,5(F10.3,2X))  SDM1500
 3012 FORMAT(//)                                                        SDM1501
C                                                                       SDM1502
      END                                                               SDM1503
      BLOCK DATA                                                        SDM1504
C                    BLOCK DATA        (VERSION 79365), PART OF MPTER.  SDM1505
      COMMON /EXPOS/ PXUCOF(6,9),PXUEXP(6,9),HC1(10),BXUCOF(6,9),BXUEXP(SDM1506
     *6,9)                                                              SDM1507
C***COEFFICIENTS GENERATED WITH RURAL SIGMAS USING PGYZ                 SDM1508
C***   IH=9 FOR H GREATER THAN 500 METERS.                              SDM1509
      DATA PXUCOF /.10401E+00,.12133E+00,.14273E+00,.15351E+00,.18855E+0SDM1510
     10,.18668E+00,.77533E-01,.11728E+00,.14120E+00,.18239E+00,.20458E+0SDM1511
     20,.34326E+00,.67228E-01,.10013E+00,.13963E+00,.19162E+00,.38998E+0SDM1512
     30,.76271E+00,.40484E-01,.75308E-01,.13784E+00,.54357E+00,.72550E+0SDM1513
     40,.22936E+01,.28539E-01,.66936E-01,.13615E+00,.52790E+00,.12908E+0SDM1514
     51,.56943E+01,.14792E-01,.65799E-01,.13315E+00,.74832E+00,.28818E+0SDM1515
     61,.40940E+03,.12403E-01,.64321E-01,.12927E+00,.10826E+01,.77020E+0SDM1516
     72,.23011E+05,.12340E-01,.62874E-01,.12546E+00,.15580E+01,.68810E+0SDM1517
     83,.46522E+06,.12245E-01,.60615E-01,.11952E+00,.22517E+01,.42842E+0SDM1518
     93,.00000E+00/                                                     SDM1519
      DATA PXUEXP /-.19460E+01,-.19774E+01,-.20086E+01,-.20742E+01,-.218SDM1520
     122E+01,-.22176E+01,-.18479E+01,-.19661E+01,-.20050E+01,-.21317E+01SDM1521
     2,-.22094E+01,-.24209E+01,-.18060E+01,-.19196E+01,-.20017E+01,-.214SDM1522
     362E+01,-.23991E+01,-.26556E+01,-.16763E+01,-.18468E+01,-.19984E+01SDM1523
     4,-.24128E+01,-.25578E+01,-.29371E+01,-.15940E+01,-.18191E+01,-.199SDM1524
     555E+01,-.24059E+01,-.26934E+01,-.31511E+01,-.14513E+01,-.18153E+01SDM1525
     6,-.19907E+01,-.24817E+01,-.28678E+01,-.40795E+01,-.14181E+01,-.181SDM1526
     711E+01,-.19851E+01,-.25514E+01,-.34879E+01,-.48399E+01,-.14172E+01SDM1527
     8,-.18071E+01,-.19799E+01,-.26152E+01,-.38719E+01,-.53670E+01,-.141SDM1528
     960E+01,-.18012E+01,-.19721E+01,-.26744E+01,-.37956E+01,-.17020E+02SDM1529
     A/                                                                 SDM1530
C***COEFFICIENTS GENUERATED WITH URBAN SIGMAS USING BRSYSZ & BRSZ       SDM1531
C***  FROM RAM MODEL.                                                   SDM1532
C***RELATIVE CONCENTRATIONS NORMALIZED FOR WIND SPEED FROM POINT        SDM1533
C***  SOURCE, CHI*U/Q, =BXUCOF(KST,IH)*H**BXUEXP(KST,IH)                SDM1534
C***   IH=1 FOR H LESS THAN 20 METERS.                                  SDM1535
C***   IH=2 FOR H FROM 20 TO 30 METERS.                                 SDM1536
C***   IH=3 FOR H FROM 30 TO 50 METERS.                                 SDM1537
C***   IH=4 FOR H FROM 50 TO 70 METERS.                                 SDM1538
C***   IH=5 FOR H FROM 70 TO 100 METERS.                                SDM1539
C***   IH=6 FOR H FROM 100 TO 200 METERS.                               SDM1540
C***   IH=7 FOR H FROM 200 TO 300 METERS.                               SDM1541
C***   IH=8 FOR H FROM 300 TO 500 METERS.                               SDM1542
C***   IH=9 FOR H GREATER THAN 500 METERS.                              SDM1543
C                                                                       SDM1544
      DATA BXUCOF /.16808E+00,.16808E+00,.20927E+00,.20378E+00,.18861E+0SDM1545
     10,.18861E+00,.15945E+00,.15945E+00,.20527E+00,.20229E+00,.21253E+0SDM1546
     20,.21253E+00,.14777E+00,.14777E+00,.19871E+00,.20011E+00,.24888E+0SDM1547
     30,.24888E+00,.13262E+00,.13262E+00,.18908E+00,.19685E+00,.30041E+0SDM1548
     40,.30041E+00,.11745E+00,.11745E+00,.17767E+00,.19301E+00,.34521E+0SDM1549
     50,.34521E+00,.91943E-01,.91943E-01,.15327E+00,.18499E+00,.34368E+0SDM1550
     60,.34368E+00,.65533E-01,.65533E-01,.11984E+00,.17445E+00,.23640E+0SDM1551
     70,.23640E+00,.47345E-01,.47345E-01,.89821E-01,.16720E+00,.15537E+0SDM1552
     80,.15537E+00,.29993E-01,.29993E-01,.56100E-01,.16747E+00,.11009E+0SDM1553
     90,.11009E+00/                                                     SDM1554
      DATA BXUEXP /-.19722E+01,-.19722E+01,-.19896E+01,-.19965E+01,-.206SDM1555
     149E+01,-.20649E+01,-.19546E+01,-.19546E+01,-.19831E+01,-.19940E+01SDM1556
     2,-.21047E+01,-.21047E+01,-.19322E+01,-.19322E+01,-.19736E+01,-.199SDM1557
     308E+01,-.21512E+01,-.21512E+01,-.19045E+01,-.19045E+01,-.19609E+01SDM1558
     4,-.19867E+01,-.21993E+01,-.21993E+01,-.18759E+01,-.18759E+01,-.194SDM1559
     562E+01,-.19820E+01,-.22320E+01,-.22320E+01,-.18228E+01,-.18228E+01SDM1560
     6,-.19142E+01,-.19728E+01,-.22310E+01,-.22310E+01,-.17589E+01,-.175SDM1561
     789E+01,-.18677E+01,-.19617E+01,-.21604E+01,-.21604E+01,-.17019E+01SDM1562
     8,-.17019E+01,-.18172E+01,-.19543E+01,-.20868E+01,-.20868E+01,-.162SDM1563
     984E+01,-.16284E+01,-.17414E+01,-.19545E+01,-.20314E+01,-.20314E+01SDM1564
     A/                                                                 SDM1565
      DATA HC1 /10.,20.,30.,50.,70.,100.,200.,300.,500.,1000./          SDM1566
C                                                                       SDM1567
      END                                                               SDM1568
      BLOCK DATA ONE                                                    SDM1569
C                                                                       SDM1570
      COMMON /MO/ QTHETA(24),QU(24),IKST(24),QHL(24),QTEMP(24),MPS(25),NSDM1571
     1SIGP,IO,LINE1(20),LINE2(20),LINE3(20),IRANK(180)                  SDM1572
      COMMON /CHARS/ RNAME(2,180),STAR(5,180),SNAME(3)                  SDM1573
      CHARACTER*4 RNAME,STAR,SNAME                                      SDM1574
C                                                                       SDM1575
      DATA STAR /900*' '/                                               SDM1576
C                                                                       SDM1577
        END                                                             SDM1578
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCANG0001
      FUNCTION ANGARC (DELM,DELN)                                       ANG0002
C                                                                       ANG0003
C PURPOSE: DETERMINES APPROPRIATE ANGLE OF TAN(ANG) = DELM/DELN         ANG0004
C          WHICH IS REQUIRED FOR CALCULATION OF RESULTANT               ANG0005
C          WIND DIRECTION.                                              ANG0006
C                                                                       ANG0007
C I/O:  DELM, AVERAGE WIND COMPONENT IN THE EAST DIRECTION              ANG0008
C       DELN, AVERAGE WIND COMPONENT IN THE NORTH DIRECTION             ANG0009
C                                                                       ANG0010
C CALLED BY:  MAIN                                                      ANG0011
C                                                                       ANG0012
C CALLS:  NONE                                                          ANG0013
C                                                                       ANG0014
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            ANG0015
C       RESEARCH TRIANGLE PARK, NC                                      ANG0016
C                                                                       ANG0017
C SDM 1.0         REVISION HISTORY:                                     ANG0018
C     79365       MPTER VERSION                                         ANG0019
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCANG0020
                                                                        ANG0021
      IF (DELN) 10,40,80                                                ANG0022
10    IF (DELM) 20,30,20                                                ANG0023
20    ANGARC=57.29578*ATAN(DELM/DELN)+180.                              ANG0024
      RETURN                                                            ANG0025
30    ANGARC=180.                                                       ANG0026
      RETURN                                                            ANG0027
40    IF (DELM) 50,60,70                                                ANG0028
50    ANGARC=270.                                                       ANG0029
      RETURN                                                            ANG0030
60    ANGARC=0.                                                         ANG0031
C     ANGARC=0. INDICATES INDETERMINATE ANGLE                           ANG0032
      RETURN                                                            ANG0033
70    ANGARC=090.                                                       ANG0034
      RETURN                                                            ANG0035
80    IF (DELM) 90,100,110                                              ANG0036
90    ANGARC=57.29578*ATAN(DELM/DELN)+360.                              ANG0037
      RETURN                                                            ANG0038
100   ANGARC=360.                                                       ANG0039
      RETURN                                                            ANG0040
110   ANGARC=57.29578*ATAN(DELM/DELN)                                   ANG0041
      RETURN                                                            ANG0042
C                                                                       ANG0043
      END                                                               ANG0044
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAL0001
      SUBROUTINE CALC(DIST)                                             CAL0002
C                                                                       CAL0003
C PURPOSE: INTERMEDIARY BETWEEN THE MAIN MODULE AND SUBROUTINE SIMP.    CAL0004
C          TRAVEL DISTANCE UNTIL PLUME LEVELS OFF IS CALCULATED AND     CAL0005
C          GROUND-LEVEL CONCENTRATIONS EVALUATED BY SUBROUTINE SIMP ARE CAL0006
C          MULTIPLIED BY THE SOURCE STRENGTH AND CONVERTED TO UG/M^3.   CAL0007
C          CONCENTRATIONS ARE STORED IN THE COMMON BLOCK FOR OUTPUT     CAL0008
C          IN MAIN.                                                     CAL0009
C                                                                       CAL0010
C I/O:  DIST, DISTANCE FROM SHORE TO SOURCE                             CAL0011
C                                                                       CAL0012
C CALLED BY:  SFM                                                       CAL0013
C                                                                       CAL0014
C CALLS:  SIMP                                                          CAL0015
C                                                                       CAL0016
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            CAL0017
C       RESEARCH TRIANGLE PARK, NC                                      CAL0018
C                                                                       CAL0019
C SDM 1.0         REVISION HISTORY:                                     CAL0020
C                                                                       CAL0021
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAL0022
C                                                                       CAL0023
C   DEFINE VARIABLES:                                                   CAL0024
C                                                                       CAL0025
C     ACC    = DESIRED ACCURACY OF ANSWER                               CAL0026
C     ANS    = APPROXIMATE VALUE OF THE INTEGRAL OF F(X)                CAL0027
C              FOR INTERVAL FROM LB TO XP1                              CAL0028
C     ANS1   = APPROXIMATE VALUE OF THE INTEGRAL OF F(X)                CAL0029
C              FOR THE INTERVAL FROM XP1 TO XP.                         CAL0030
C     AREA   = APPROXIMATE, ABSOLUTE VALUE OF THE INTEGRAL OF           CAL0031
C              F(X) FOR THE INTERVAL FROM LB TO XP                      CAL0032
C     F(X)   = FUNCTION WHOSE INTEGRAL IS DESIRED                       CAL0033
C     IFLAG  = 1 FOR NORMAL RETURN                                      CAL0034
C              2 IF IT IS NECESSARY TO GO TO 30 LEVELS.                 CAL0035
C                ERROR MAY BE UNRELIABLE IN THIS CASE.                  CAL0036
C              3 IF MORE THAN 2000 FUNCTION EVALUATIONS.                CAL0037
C                COMPLETE THE COMPUTATIONS AND NOTE THAT                CAL0038
C                ERROR IS USUALLY UNRELIABLE.                           CAL0039
C              IFLAG MAY BE USED FOR DIAGNOSTICS.                       CAL0040
C     LB     = INITIAL X VALUE NEAR STACK                               CAL0041
C     UL     = MEAN WIND SPEED IN THE TIBL                              CAL0042
C     US     = MEAN WIND SPEED AT STACK HEIGHT IN STABLE AIR            CAL0043
C     XP     = DOWNWIND DISTANCE FROM SOURCE                            CAL0044
C     XP1    = DOWNWIND DISTANCE AT WHICH PLUME LEVELS OFF              CAL0045
C     YP     = HORIZONTAL DISTANCE                                      CAL0046
C                                                                       CAL0047
      EXTERNAL EVAL                                                     CAL0048
      REAL XP,YP,MGCM,LB                                                CAL0049
      COMMON /SDMONE/XP,YP,A,B,UL,US,HSTK,CN,F,Q,MGCM                   CAL0050
                                                                        CAL0051
      COMMON /MPR/UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC,MUOR                 CAL0052
                                                                        CAL0053
C                                                                       CAL0054
C->->  INITIALIZE VARIABLES                                             CAL0055
C                                                                       CAL0056
      ACC = 10.0E-6                                                     CAL0057
      LB = 10.00 + DIST                                                 CAL0058
      IFLAG = 0.0                                                       CAL0059
C                                                                       CAL0060
C->->  IF X COORDINATE IS TOO SMALL, GO TO                              CAL0061
C      NEXT RECEPTOR                                                    CAL0062
C                                                                       CAL0063
      IF(XP.GT.0.001) GOTO 10                                           CAL0064
      GOTO 100                                                          CAL0065
C                                                                       CAL0066
C->->  INITIALIZE VALUES OF THE INTEGRAL                                CAL0067
C                                                                       CAL0068
10    ANS = 0.0                                                         CAL0069
      ANS1 = 0.0                                                        CAL0070
C                                                                       CAL0071
C->->  DETERMINE TRAVEL DISTANCE UNTIL PLUME LEVELS OFF                 CAL0072
C                                                                       CAL0073
      XP1 = DIST+(4.50/CN)*US                                           CAL0074
      XP=XP+DIST                                                        CAL0075
C                                                                       CAL0076
C->->  DETERMINE WHETHER DISTANCE OF RECEPTOR DOWNWIND OF               CAL0077
C      SOURCE EXCEEDS TRAVEL DISTANCE UNTIL PLUME LEVELS OFF            CAL0078
C                                                                       CAL0079
      IF(XP.GE.XP1) GOTO 20                                             CAL0080
C                                                                       CAL0081
C->->  APPLY SIMPSON'S RULE OVER THE INTERVAL FROM INITIAL              CAL0082
C      POINT (LB) TO RECEPTOR POINT (XP)                                CAL0083
C                                                                       CAL0084
      CALL SIMP(EVAL,LB,XP,ACC,ANS,ERROR,AREA,IFLAG)                    CAL0085
C                                                                       CAL0086
      GOTO 30                                                           CAL0087
C                                                                       CAL0088
C->->  RECEPTOR POINT IS IN REGION WHERE PLUME HAS LEVELED              CAL0089
C      OFF.  APPLY SIMPSON'S RULE IN TWO STEPS.  FIRST APPLY            CAL0090
C      OVER THE INTERVAL FROM INITIAL POINT (LB) TO XP1.                CAL0091
C      SECOND APPLY OVER THE X DISTANCE FROM XP1 TO RECEPTOR            CAL0092
C      POINT (XP).  USING TWO CALLS TO SIMP HERE SAVES                  CAL0093
C      ON COMPUTATION TIME.                                             CAL0094
C                                                                       CAL0095
20    CALL SIMP(EVAL,LB,XP1,ACC,ANS,ERROR,AREA,IFLAG)                   CAL0096
      CALL SIMP(EVAL,XP1,XP,ACC,ANS1,ERROR,AREA,IFLAG)                  CAL0097
C                                                                       CAL0098
C->->  SUM VALUES OF THE INTEGRAL FOR THE LEFT (ANS) AND                CAL0099
C      RIGHT (ANS1) HALVES OF THE INTERVAL                              CAL0100
C                                                                       CAL0101
30    ANS = ANS+ANS1                                                    CAL0102
C                                                                       CAL0103
C->->  MULTIPLY BY SOURCE STRENGTH AND CONVERT ANSWER                   CAL0104
C      TO MICROGRAMS PER M3                                             CAL0105
C                                                                       CAL0106
      MGCM = ANS*Q/1.E-06                                               CAL0107
C                                                                       CAL0108
100   CONTINUE                                                          CAL0109
200   RETURN                                                            CAL0110
      END                                                               CAL0111
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCEVL0001
      FUNCTION EVAL(X)                                                  EVL0002
C                                                                       EVL0003
C PURPOSE: FUNCTION EVAL RETURNS A VALUE OF THE INTEGRAL FOR (X),       EVL0004
C          THE DESIGNATED POINT ON THE INTERVAL OF INTEGRATION.         EVL0005
C          THE FUNCTION RETURNS THE VALUE TO SUBROUTINE SIMP            EVL0006
C          THROUGH THE FUNCTION NAME.                                   EVL0007
C                                                                       EVL0008
C I/O:  X, INTEGRAL EVALUATION POINT                                    EVL0009
C                                                                       EVL0010
C CALLED BY:  SIMP                                                      EVL0011
C                                                                       EVL0012
C CALLS:  NONE                                                          EVL0013
C                                                                       EVL0014
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            EVL0015
C       RESEARCH TRIANGLE PARK, NC                                      EVL0016
C                                                                       EVL0017
C SDM 1.0         REVISION HISTORY:                                     EVL0018
C                                                                       EVL0019
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCEVL0020
C                                                                       EVL0021
C     DEFINE VARIABLES:                                                 EVL0022
C                                                                       EVL0023
C     CA    = COEFFICIENT IN PLUME RISE EQUATION                        EVL0024
C     CYS   = CONSTANT FOR SIGMA Y IN STABLE AIR                        EVL0025
C     CZS   = CONSTANT FOR SIGMA Z IN STABLE                            EVL0026
C     BL    = TIBL HEIGHT AT DESIGNATED POINT ON                        EVL0027
C             INTERVAL OF INTEGRATION                                   EVL0028
C     BL1   = TIBL HEIGHT AT RECEPTOR POINT                             EVL0029
C     FN    = TIME AFTER WHICH PLUME HAS LEVELED OFF                    EVL0030
C     D     = TRAVEL TIME = X/US                                        EVL0031
C     H     = PLUME HEIGHT                                              EVL0032
C     VARYL = SIGMA Y IN CONVECTIVE LAYER                               EVL0033
C     VARYS = SIGMA Y IN STABLE LAYER                                   EVL0034
C     VARZ  = SIGMA Z IN STABLE LAYER                                   EVL0035
C                                                                       EVL0036
      REAL XP,YP,MGCM,EVAL,MF                                           EVL0037
      COMMON /SDMONE/XP,YP,A,B,UL,US,HSTK,CN,F,Q,MGCM                   EVL0038
                                                                        EVL0039
      COMMON /MPR/UPL,Z,H,HL,XX,Y,KST,DELH,SY,SZ,RC,MUOR                EVL0040
                                                                        EVL0041
C                                                                       EVL0042
C->->  DEFINE CONSTANTS                                                 EVL0043
C                                                                       EVL0044
      CA  = 1.6                                                         EVL0045
      CYS = 0.67                                                        EVL0046
      CZS = 0.40                                                        EVL0047
C                                                                       EVL0048
C->->  REEXPRESS FREQUENTLY USED CALCULATIONS                           EVL0049
C                                                                       EVL0050
      FU = F/US                                                         EVL0051
      FN = 4.50/CN                                                      EVL0052
      IF(X.LE.0.001) GOTO 100                                           EVL0053
      D = X/US                                                          EVL0054
C                                                                       EVL0055
C->->  DETERMINE SIGMA Y IN STABLE AIR                                  EVL0056
C                                                                       EVL0057
      VARYS = CYS*(D**(2./3.))*(FU**(1./3.))                            EVL0058
C                                                                       EVL0059
C->->  DETERMINE SIGMA Y IN THE UNSTABLE AIR                            EVL0060
C                                                                       EVL0061
      VARYL = B*(XP-X)/3.0                                              EVL0062
C                                                                       EVL0063
C->->  DETERMINE TIBL HEIGHT AT EVALUATION POINT (X)                    EVL0064
C      AND AT RECEPTOR POINT (XP)                                       EVL0065
C                                                                       EVL0066
      BL = A*SQRT(X)                                                    EVL0067
      BL1 = A*SQRT(XP)                                                  EVL0068
C                                                                       EVL0069
C->->  DETERMINE FIRST PART OF CONCENTRATION EQUATION                   EVL0070
C                                                                       EVL0071
      MF = 1./(2.*3.14159*BL1*UL)                                       EVL0072
C                                                                       EVL0073
C->->  DETERMINE IF TRAVEL TIME IN STABLE AIR EXCEEDS                   EVL0074
C      TIME AFTER WHICH PLUME HAS LEVELED OFF.  IF YES,                 EVL0075
C      FINAL PLUME RISE CALCULATIONS APPLY.                             EVL0076
C                                                                       EVL0077
      IF(D.GT.FN) GOTO 10                                               EVL0078
C                                                                       EVL0079
C->->  DETERMINE PLUME HEIGHT USING GRADUAL PLUME                       EVL0080
C      RISE EQUATION                                                    EVL0081
C                                                                       EVL0082
       H = (CA*(FU**(1./3.))*(D**(2./3.))) + HSTK                       EVL0083
C                                                                       EVL0084
C      ADDED 9-6-88 TO ACCOUNT FOR MPTER/MISRA PLUME RISE DIFFERNCES    EVL0085
C                                                                       EVL0086
       H = AMIN1(H,2.6*(FU/CN**2.)**.3333+HSTK)                         EVL0087
C                                                                       EVL0088
C->->  DETERMINE SIGMA Z IN STABLE AIR FOR GRADUALLY                    EVL0089
C      RISING PLUME                                                     EVL0090
C                                                                       EVL0091
      VARZ = CZS*(D**(2./3.))*(FU**(1./3.))                             EVL0092
C                                                                       EVL0093
C->->  DETERMINE VALUE OF DERIVATIVE IN                                 EVL0094
C      CONCENTRATION EQUATION FOR RISING PLUME                          EVL0095
C                                                                       EVL0096
      DERIV = (-1./6.)*(A*UL)/(CZS*(F**(1./3.)))                        EVL0097
     #*(X**(-7./6.)) +                                                  EVL0098
     #(HSTK * UL)/(CZS*(F**(1./3.)))*(2./3.)*(X**(-5./3.))              EVL0099
C                                                                       EVL0100
C     IF DISTANCE IS LARGE, TREAT AS LEVEL PLUME                        EVL0101
C                                                                       EVL0102
      IF (DERIV.LE.0.) GO TO 10                                         EVL0103
C                                                                       EVL0104
      GO TO 20                                                          EVL0105
C                                                                       EVL0106
C->->  PLUME HAS LEVELED OFF IN STABLE AIR                              EVL0107
C                                                                       EVL0108
C->->  DETERMINE SIGMA Z IN STABLE AIR FOR A LEVEL                      EVL0109
C      PLUME                                                            EVL0110
C                                                                       EVL0111
10    VARZ = 1.1*(FU/(CN*CN))**(1./3.)                                  EVL0112
C                                                                       EVL0113
C->->  DETERMINE VALUE OF DERIVATIVE IN                                 EVL0114
C      CONCENTRATION EQUATION FOR LEVEL PLUME                           EVL0115
C                                                                       EVL0116
      DERIV = A/(2.*SQRT(X)*VARZ)                                       EVL0117
C                                                                       EVL0118
20    VARZS = VARZ*VARZ                                                 EVL0119
      SIGS = (VARYS*VARYS+VARYL*VARYL)                                  EVL0120
      BLDIFS = (BL-H)*(BL-H)                                            EVL0121
C                                                                       EVL0122
C->->  DETERMINE VALUE OF EXPONENTIAL                                   EVL0123
C      IN CONCENTRATION EQUATION                                        EVL0124
C                                                                       EVL0125
      C1 = -.5*(BLDIFS/VARZS+YP*YP/SIGS)                                EVL0126
C                                                                       EVL0127
C->->  TAKE ALOG OF EXPRESSION INSIDE                                   EVL0128
C      INTEGRAL OF CONCENTRATION EQUATION                               EVL0129
C                                                                       EVL0130
      C = ALOG(DERIV)+C1-(ALOG(SIGS))/2.0                               EVL0131
C                                                                       EVL0132
      IF(ABS(C).GT.70.0) GOTO 100                                       EVL0133
C                                                                       EVL0134
C->->  TAKE EXPONENTIAL OF EXPRESSION                                   EVL0135
C      INSIDE INTEGRAL OF CONCENTRATION EQUATION                        EVL0136
C                                                                       EVL0137
       EVALL = EXP(C)                                                   EVL0138
C                                                                       EVL0139
C->->  DETERMINE VALUE OF THE CONCENTRATION                             EVL0140
C      EQUATION (MINUS MULTIPLICATION BY                                EVL0141
C      THE SOURCE STRENGTH)                                             EVL0142
C                                                                       EVL0143
       EVAL = EVALL*MF                                                  EVL0144
       GOTO 30                                                          EVL0145
100    EVAL = 0.                                                        EVL0146
30     CONTINUE                                                         EVL0147
       END                                                              EVL0148
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCINT0001
      SUBROUTINE INTERF(FLAG,IDAY,LH,J)                                 INT0002
C                                                                       INT0003
C PURPOSE: INTERFACES MPTER AND MSFM.  TESTS FOR APPLICABILITY          INT0004
C          OF MSFM AND COMPUTES REQUIRED MSFM INPUT VALUES.             INT0005
C                                                                       INT0006
C                                                                       INT0007
C I/O:  FLAG, FLAG TO DETERMINE STATUS OF MSFM APPLICABILITY            INT0008
C       IDAY, JULIAN DAY                                                INT0009
C         LH, LAST HOUR                                                 INT0010
C          J, SOURCE NUMBER                                             INT0011
C                                                                       INT0012
C CALLED BY:  PTR                                                       INT0013
C                                                                       INT0014
C CALLS:  XSHORE                                                        INT0015
C         SFM                                                           INT0016
C                                                                       INT0017
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            INT0018
C       RESEARCH TRIANGLE PARK, NC                                      INT0019
C                                                                       INT0020
C SDM 1.0         REVISION HISTORY:                                     INT0021
C                                                                       INT0022
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCINT0023
                                                                        INT0024
      COMMON /SHORE/XSL(250),YSL(250),BA(250),EA(250),FETCH(250),       INT0025
     & INDEX(250),THETA                                                 INT0026
                                                                        INT0027
      COMMON /MSFM/ MSFMFL                                              INT0028
      INTEGER MSFMFL(50,24)                                             INT0029
                                                                        INT0030
      REAL MGCM                                                         INT0031
      COMMON /SDMONE/XP,YP,A,B,UL,US,HSTK,CN,F,Q,MGCM                   INT0032
                                                                        INT0033
      COMMON /MPR/UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC,MUOR                 INT0034
                                                                        INT0035
C         /MP/  BETWEEN MAIN PROGRAM AND PTR                            INT0036
      COMMON /MP/ SOURCE(9,250),CONTWO,PSAV(250),IPSIGS(250),U,TEMP,SINTINT0037
     1,COST,PL(6),ELP(250),ELHN,HANE,TLOS,CELM,CTER                     INT0038
                                                                        INT0039
C         /MO/  BETWEEN MAIN PROGRAM AND OUTHR                          INT0040
      COMMON /MO/ QTHETA(24),QU(24),IKST(24),QHL(24),QTEMP(24),MPS(25),NINT0041
     1SIGP,IO,LINE1(20),LINE2(20),LINE3(20),IRANK(180)                  INT0042
      COMMON /CHARS/ RNAME(2,180),STAR(5,180),SNAME(3)                  INT0043
                                                                        INT0044
      COMMON/ MPOR/ IOPT(26)                                            INT0045
C  *** DCD Modification 11/14/90                                        INT0046
C      Add Common Block LOCAL to save local variables                   INT0047
      COMMON /LOCAL/ DIST,HO,DTHDZ,PTMOL,PTMOW,TRAD                     INT0048
                                                                        INT0049
      CHARACTER*4 RNAME,STAR,SNAME                                      INT0050
      INTEGER FLAG                                                      INT0051
                                                                        INT0052
      G=9.81                                                            INT0053
      CSUBP=1020.                                                       INT0054
      RHO=1.188                                                         INT0055
      KM2M=1000.                                                        INT0056
                                                                        INT0057
      IF (FLAG.EQ.1) GO TO 1000                                         INT0058
      IF (FLAG.EQ.4) GO TO 100                                          INT0059
      IF (FLAG.EQ.2.OR.FLAG.EQ.3) GO TO 9999                            INT0060
                                                                        INT0061
C     DCD MODIFICATION  10/29/90                                        INT0062
C     INPUT TOWER METEOROLOGICAL DATA IN EITHER BINARY OR ASCII         INT0063
   5  IF(IOPT(26) .EQ. 0) THEN                                          INT0064
         READ (19) IDAT,IHOUR,UL,US,PTMOL,PTMOW,DTHDZ,HO                INT0065
      ELSE                                                              INT0066
         READ (19,*) IDAT,IHOUR,UL,US,PTMOL,PTMOW,DTHDZ,HO              INT0067
      ENDIF                                                             INT0068
C     END OF MODIFICATION                                               INT0069
      IF (IDAT.LT.IDAY) GO TO 5                                         INT0070
      IF (IHOUR.LT.LH) GO TO 5                                          INT0071
      IF (IHOUR.GT.LH.OR.IDAT.GT.IDAY) THEN                             INT0072
         WRITE (6,*) 'FAULTY TOWER DATA AT DAY',IDAY                    INT0073
         STOP                                                           INT0074
      ENDIF                                                             INT0075
C     DCD MODIFICATION  10/29/90                                        INT0076
C     LIMIT STACK TOP WIND SPEED (US) TO 1 M/S                          INT0077
      IF(US .LT. 1.0) US = 1.0                                          INT0078
C     END OF MODIFICATION                                               INT0079
C                                                                       INT0080
C     BA=BEGINNING ANGLE, EA=ENDING ANGLE                               INT0081
C     TRAD=WIND DIRECTION                                               INT0082
C                                                                       INT0083
      TRAD=QTHETA(LH)                                                   INT0084
                                                                        INT0085
C     DCD MODIFICATION  10/31/90                                        INT0086
C     CHANGE CHECK FROM EA < 360 TO EA-FETCH <= 360                     INT0087
      IF (EA(J)-FETCH(J).LE.360.) THEN                                  INT0088
C     END OF MODIFICATION                                               INT0089
         IF (TRAD.LT.BA(J)+FETCH(J).OR.TRAD.GT.EA(J)-FETCH(J)) THEN     INT0090
            FLAG=2                                                      INT0091
            RETURN                                                      INT0092
         ENDIF                                                          INT0093
       ELSE                                                             INT0094
         IF (TRAD.GT.BA(J)+FETCH(J)) GO TO 20                           INT0095
         IF (TRAD+360.LT.EA(J)-FETCH(J)) GO TO 20                       INT0096
         FLAG=2                                                         INT0097
         RETURN                                                         INT0098
      ENDIF                                                             INT0099
C                                                                       INT0100
C     KST = STABILITY CLASS                                             INT0101
C                                                                       INT0102
  20  IF (KST.GT.3) THEN                                                INT0103
         FLAG=2                                                         INT0104
         RETURN                                                         INT0105
      ENDIF                                                             INT0106
                                                                        INT0107
C                                                                       INT0108
C    UL = MEAN WIND SPEED IN THE TIBL                                   INT0109
C    CODE MODIFIED BY DCD  10/26/90                                     INT0110
C    SFM CALCULATIONS NOT MADE IF UL < 2 M/S                            INT0111
C                                                                       INT0112
      IF (UL.LT.2.0) THEN                                               INT0113
         FLAG = 2                                                       INT0114
         RETURN                                                         INT0115
      ENDIF                                                             INT0116
                                                                        INT0117
C    END OF CODE MODIFICATION                                           INT0118
                                                                        INT0119
      IF (DTHDZ.LE.0) THEN                                              INT0120
         FLAG=2                                                         INT0121
         RETURN                                                         INT0122
      ENDIF                                                             INT0123
C                                                                       INT0124
C   CODE MODIFIED BY DCD 10/26/90                                       INT0125
C   CHANGED H0.LT.20 TO H0.LE.20                                        INT0126
C   AS PER TABLE 2-1 OF USER'S GUIDE                                    INT0127
C                                                                       INT0128
      IF (HO.LE.20) THEN                                                INT0129
         FLAG=2                                                         INT0130
         RETURN                                                         INT0131
      ENDIF                                                             INT0132
C                                                                       INT0133
C     TBLHGT = TIBL HEIGHT                                              INT0134
C     XSHORE = DISTANCE FROM SHORE TO STACK                             INT0135
  100 X1=SOURCE(1,J)*CONTWO                                             INT0136
      Y1=SOURCE(2,J)*CONTWO                                             INT0137
      X2=XSL(J)*CONTWO                                                  INT0138
      Y2=YSL(J)*CONTWO                                                  INT0139
      DIST=XSHORE(X1,Y1,TRAD,X2,Y2,BA(J),EA(J),FETCH(J))                INT0140
      DIST=DIST*KM2M                                                    INT0141
      TBLHGT=(((2*HO)/(CSUBP*RHO*DTHDZ*UL))**.5)*SQRT(DIST)             INT0142
      HSTK=SOURCE(5,J)                                                  INT0143
C                                                                       INT0144
C     THT = STACK HEIGHT                                                INT0145
C                                                                       INT0146
      IF (HSTK.LT.TBLHGT) THEN                                          INT0147
         FLAG=3                                                         INT0148
         RETURN                                                         INT0149
      ENDIF                                                             INT0150
      Q=1                                                               INT0151
C  DCD Modification 11/16/90                                            INT0152
C  F passed from common; F1 not necessary                               INT0153
C     F1=F                                                              INT0154
      CN = ((G/PTMOW)*DTHDZ)**0.5                                       INT0155
C                                                                       INT0156
C->->  DETERMINE THE TIBL A FACTOR                                      INT0157
C                                                                       INT0158
      A = ((2.*HO)/(CSUBP*RHO*DTHDZ*UL))**0.5                           INT0159
C                                                                       INT0160
C->->  DETERMINE THE TIBL HEIGHT FOR USE IN CALCULATING                 INT0161
C      CONVECTIVE VELOCITY (W*)                                         INT0162
C                                                                       INT0163
      HT = A*(5000.**0.5)                                               INT0164
C                                                                       INT0165
C->->  DETERMINE PART OF SIGMA Y EQUATION                               INT0166
C      FOR THE CONVECTIVE LAYER (B = W*/UL)                             INT0167
C                                                                       INT0168
      B = (((G*HO*HT)/(RHO*CSUBP*PTMOL))**.333)/UL                      INT0169
                                                                        INT0170
      FLAG=1                                                            INT0171
                                                                        INT0172
      WRITE (20,2000) J,IDAY,LH,HO,UL,US,DTHDZ,PTMOL,PTMOW              INT0173
 2000 FORMAT (' TIBL:','SOURCE= ',I3,' DAY= ',I3,' HOUR= ',I2,          INT0174
     & ' HO= ',F7.2,' UL= ',F7.2,' US= ',F7.2,' DTHDZ= ',F8.4,          INT0175
     & ' PTMOL= ',F8.1,' PTMOW= ',F8.1)                                 INT0176
                                                                        INT0177
1000  CONTINUE                                                          INT0178
      XP=X*1000.                                                        INT0179
      YP=Y*1000.                                                        INT0180
      CALL SFM(DIST)                                                    INT0181
C  DCD Modification  11/15/90                                           INT0182
C   Remove Hourly Fumigation Flag                                       INT0183
C   Should be stored by Hour AND Receptor                               INT0184
C     MSFMHR(IDAY*24+LH)=1                                              INT0185
      MSFMFL(J,LH)=1                                                    INT0186
      RC=MGCM/1.E6                                                      INT0187
9999  CONTINUE                                                          INT0188
      RETURN                                                            INT0189
      END                                                               INT0190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOTA0001
      SUBROUTINE OUTAVG                                                 OTA0002
C                                                                       OTA0003
C PURPOSE: PROVIDES OUTPUT CONCENTRATIONS IN MICROGRAMS PER CUBIC       OTA0004
C          METER FOR EACH AVERAGING TIME FOR:                           OTA0005
C          1) CONTRIBUTIONS FROM SIGNIFICANT SOURCES                    OTA0006
C          2) SUMMARIES                                                 OTA0007
C                                                                       OTA0008
C I/O:  NONE                                                            OTA0009
C                                                                       OTA0010
C CALLED BY:  MAIN                                                      OTA0011
C                                                                       OTA0012
C CALLS:  NONE                                                          OTA0013
C                                                                       OTA0014
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            OTA0015
C       RESEARCH TRIANGLE PARK, NC                                      OTA0016
C                                                                       OTA0017
C SDM 1.0         REVISION HISTORY:                                     OTA0018
C                                                                       OTA0019
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOTA0020
                                                                        OTA0021
      COMMON /MPOR/ IOPT(26)                                            OTA0022
      COMMON /MPO/ NRECEP,NAVG,NB,LH,NPT,IDATE(2),RREC(180),SREC(180),ZROTA0023
     1(180),ELR(180),PHCHI(180),PHSIGS(180,26),HSAV(250),DSAV(250),PCHI(OTA0024
     2180),PSIGS(180,26),IPOL                                           OTA0025
      COMMON /MO/ QTHETA(24),QU(24),IKST(24),QHL(24),QTEMP(24),MPS(25),NOTA0026
     1SIGP,IO,LINE1(20),LINE2(20),LINE3(20),IRANK(180)                  OTA0027
      COMMON /CHARS/ RNAME(2,180),STAR(5,180),SNAME(3)                  OTA0028
      CHARACTER*4 RNAME,STAR,SNAME                                      OTA0029
C                                                                       OTA0030
C                                                                       OTA0031
      CHARACTER*4 IPOLT(2),IPOLU                                        OTA0032
      DATA IPOLT /'SO2 ','PART'/                                        OTA0033
                                                                        OTA0034
C            AT THIS ENTRY POINT, CONCENTRATION OUTPUT                  OTA0035
C        IN MICROGRAMS PER CUBIC METER ARE PRINTED FOR THE              OTA0036
C        AVERAGING PERIOD. CONTRIBUTIONS AND/OR SUMMARY                 OTA0037
C        INFORMATION IS AVAILABLE.                                      OTA0038
C        AVERAGE CONCENTRATIONS OVER SPECIFIED TIME PERIOD              OTA0039
C                                                                       OTA0040
C     DCD MODIFICATION 10/29/90                                         OTA0041
      IPOLU = IPOLT(1)                                                  OTA0042
      IF(IPOL.EQ.4) IPOLU = IPOLT(2)                                    OTA0043
C     END OF MODIFICATION                                               OTA0044
      DO 190 K=1,NRECEP                                                 OTA0045
      PCHI(K)=PCHI(K)/NAVG                                              OTA0046
      HSAV(K)=PCHI(K)                                                   OTA0047
      DO 180 I=1,26                                                     OTA0048
180   PSIGS(K,I)=PSIGS(K,I)/NAVG                                        OTA0049
190   CONTINUE                                                          OTA0050
C        OPTION(17): SKIP OUTPUT OF THE AVERAGED CONTRIBUTIONS.         OTA0051
      IF (IOPT(17).EQ.1) GO TO 270                                      OTA0052
C       WRITE AVERAGING-TIME SIGNIFICANT                                OTA0053
C       SOURCE CONTRIBUTIONS.                                           OTA0054
      WRITE (IO,350) LINE1,LINE2,LINE3                                  OTA0055
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                OTA0056
      IF (NSIGP.GT.10) GO TO 210                                        OTA0057
C        PRINT FIRST PAGE OF OUTPUT AND TOTALS FOR 10 OR LESS SIGNIF SOUOTA0058
      WRITE (IO,380) (I,I=1,NSIGP)                                      OTA0059
      WRITE (IO,390)                                                    OTA0060
      WRITE (IO,380) (MPS(I),I=1,NSIGP)                                 OTA0061
      WRITE (IO,400)                                                    OTA0062
      DO 200 K=1,NRECEP                                                 OTA0063
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=1,NSIGP)       OTA0064
C        PRINT TOTALS                                                   OTA0065
      WRITE (IO,420) PSIGS(K,26),PCHI(K)                                OTA0066
200   CONTINUE                                                          OTA0067
      GO TO 270                                                         OTA0068
C        PRINT FIRST PAGE FOR MORE THAN 10 SIGNIF SOURCES               OTA0069
210   WRITE (IO,380) (I,I=1,10)                                         OTA0070
      WRITE (IO,430) (MPS(I),I=1,10)                                    OTA0071
      WRITE (IO,400)                                                    OTA0072
      DO 220 K=1,NRECEP                                                 OTA0073
220   WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=1,10)          OTA0074
      IF (NSIGP.GT.20) GO TO 240                                        OTA0075
C        PRINT SECOND PAGE AND TOTALS FOR 11 TO 20 SIGNIF SOURCES       OTA0076
      WRITE (IO,350) LINE1,LINE2,LINE3                                  OTA0077
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                OTA0078
      WRITE (IO,380) (I,I=11,NSIGP)                                     OTA0079
      WRITE (IO,390)                                                    OTA0080
      WRITE (IO,380) (MPS(I),I=11,NSIGP)                                OTA0081
      WRITE (IO,400)                                                    OTA0082
      DO 230 K=1,NRECEP                                                 OTA0083
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=11,NSIGP)      OTA0084
230   WRITE (IO,420) PSIGS(K,26),PCHI(K)                                OTA0085
      GO TO 270                                                         OTA0086
C        WRITE SECOND PAGE FOR MORE THAN 20 SIGNIF SOURCES              OTA0087
240   WRITE (IO,350) LINE1,LINE2,LINE3                                  OTA0088
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                OTA0089
      WRITE (IO,380) (I,I=11,20)                                        OTA0090
      WRITE (IO,430) (MPS(I),I=11,20)                                   OTA0091
      WRITE (IO,400)                                                    OTA0092
      DO 250 K=1,NRECEP                                                 OTA0093
250   WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=11,20)         OTA0094
      WRITE (IO,350) LINE1,LINE2,LINE3                                  OTA0095
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                OTA0096
C        WRITE LAST PAGE AND TOTALS FOR MORE THAN 20 SIGNIF SOURCES     OTA0097
      WRITE (IO,380) (I,I=21,NSIGP)                                     OTA0098
      WRITE (IO,390)                                                    OTA0099
      WRITE (IO,380) (MPS(I),I=21,NSIGP)                                OTA0100
      WRITE (IO,400)                                                    OTA0101
      DO 260 K=1,NRECEP                                                 OTA0102
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=21,NSIGP)      OTA0103
260   WRITE (IO,420) PSIGS(K,26),PCHI(K)                                OTA0104
C                                                                       OTA0105
C        WRITE AVERAGING-TIME SUMMARY.                                  OTA0106
C                                                                       OTA0107
C        OPTION(18): SKIP OUTPUT OF THE AVERAGED SUMMARIES.             OTA0108
270   IF (IOPT(18).EQ.1) GO TO 310                                      OTA0109
      WRITE (IO,350) LINE1,LINE2,LINE3                                  OTA0110
      WRITE (IO,530) NAVG,IPOLU,IDATE,NB                                OTA0111
      WRITE (IO,500)                                                    OTA0112
C        CALCULATE GRAND TOTALS AND RANK CONCENTRATIONS                 OTA0113
      DO 290 I=1,NRECEP                                                 OTA0114
      CMAX=-1.0                                                         OTA0115
      DO 280 K=1,NRECEP                                                 OTA0116
      IF (HSAV(K).LE.CMAX) GO TO 280                                    OTA0117
      CMAX=HSAV(K)                                                      OTA0118
      LMAX=K                                                            OTA0119
280   CONTINUE                                                          OTA0120
      IRANK(LMAX)=I                                                     OTA0121
      HSAV(LMAX)=-1.0                                                   OTA0122
290   CONTINUE                                                          OTA0123
      DO 300 K=1,NRECEP                                                 OTA0124
      WRITE (IO,510) K,STAR(1,K),STAR(2,K),(RNAME(J,K),J=1,2),RREC(K),SROTA0125
     1EC(K),ZR(K),ELR(K),PSIGS(K,26),PCHI(K),IRANK(K)                   OTA0126
300   CONTINUE                                                          OTA0127
310   IF (IOPT(24).EQ.0) GO TO 330                                      OTA0128
C        PUNCH CONCENTRATIONS FOR CONTOURING(MICROGRAMS/CUBIC METER)    OTA0129
C        RECEPTOR COORDINATES IN USER UNITS.                            OTA0130
      DO 320 K=1,NRECEP                                                 OTA0131
      GWU=PCHI(K)*1.0E+06                                               OTA0132
      WRITE (IO,540) RREC(K),SREC(K),GWU,K,ZR(K),ELR(K)                 OTA0133
      WRITE (1,540) RREC(K),SREC(K),GWU,K,ZR(K),ELR(K)                  OTA0134
320   CONTINUE                                                          OTA0135
330   IF (IOPT(23).EQ.0) GO TO 340                                      OTA0136
C        WRITE PERIODIC CONC. TO DISK/TAPE - FOR LONG-TERM APPLICATION  OTA0137
C        FOR EACH RUN, THIS WRITE STATEMENT WILL GENERATE               OTA0138
C        'NPER' RECORDS.                                                OTA0139
      WRITE (13) IDATE(2),NB,(PCHI(K),K=1,NRECEP)                       OTA0140
340   RETURN                                                            OTA0141
C                                                                       OTA0142
350   FORMAT ('1',20A4/1X,20A4/1X,20A4)                                 OTA0143
360   FORMAT('0',T30,A4,' CONTRIBUTION(MICROGRAMS/M**3) FROM SIGNIFICANTOTA0144
     1 POINT SOURCES ',5X,I2,'/',I4,' : HOUR ',I2//)                    OTA0145
370   FORMAT (1H0,T5,'RANK')                                            OTA0146
380   FORMAT ('+',T12,10(I3,7X))                                        OTA0147
390   FORMAT ('+',T113,'TOTAL     TOTAL'/1X,T113,'SIGNIF    ALL POINT'/1OTA0148
     1X,T113,'POINT     SOURCES'/1X,'SOURCE #')                         OTA0149
400   FORMAT (1X,'RECEP #')                                             OTA0150
410   FORMAT (1X,I3,2A1,6P,10F10.3)                                     OTA0151
420   FORMAT ('+',T109,6P,2F10.3)                                       OTA0152
430   FORMAT (1X,'SOURCE #',T12,10(I3,7X))                              OTA0153
440   FORMAT('0',T25,A4,' SUMMARY CONCENTRATION TABLE(MICROGRAMS/M**3) 'OTA0154
     1,5X,I2,'/',I4,' : HOUR ',I2/1X)                                   OTA0155
450   FORMAT (1X,T2,'HOUR   THETA    SPEED   MIXING   TEMP   STABILITY'/OTA0156
     11X,T9,'(DEG)    (M/S) HEIGHT(M)   (K)     CLASS'/1X)              OTA0157
460   FORMAT (1X,T3,I2,4F9.2,6X,I1//)                                   OTA0158
470   FORMAT (13X,10I11)                                                OTA0159
480   FORMAT (' FINAL HT (M) ',10F11.2)                                 OTA0160
490   FORMAT (' DIST FIN HT (KM)',10F11.3)                              OTA0161
500   FORMAT ('0',T7,'RECEPTOR',T23,'EAST',T33,'NORTH',T43,'RECEPTOR HT'OTA0162
     1,T61,'RECEPTOR',T78,'TOTAL FROM',T93,'TOTAL FROM',T106,'CONCENTRATOTA0163
     2ION'/' ',T7,'NO. NAME',T22,'COORD',T33,'COORD',T44,'ABV GRD (M)',TOTA0164
     359,'GRD-LVL ELEV',T77,'SIGNIF POINT',T93,'ALL SOURCES',T111,'RANK'OTA0165
     4/' ',T58,'(USER HT UNITS)',T80,'SOURCES'//)                       OTA0166
510   FORMAT (1H ,I8,2A1,2X,2A4,2F10.2,F12.1,F20.1,6P,2F15.4,I15)       OTA0167
520   FORMAT ('0',T22,I2,'-HOUR AVERAGE ',A4,' CONTRIBUTION(MICROGRAMS/MOTA0168
     1**3) FROM SIGNIFICANT POINT SOURCES',5X,I2,'/',I3,'  START HOUR: 'OTA0169
     2,I2//1X,T5,'RANK')                                                OTA0170
530   FORMAT ('0',T25,I2,'-HOUR AVERAGE ',A4,' SUMMARY CONCENTRATION TABOTA0171
     1LE(MICROGRAMS/M**3)',5X,I2,'/',I3,'  START HOUR: ',I2//1X)        OTA0172
540   FORMAT ('CNTL',1X,3F10.3,20X,I4,2F10.1)                           OTA0173
C                                                                       OTA0174
      END                                                               OTA0175
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOTH0001
      SUBROUTINE OUTHR                                                  OTH0002
C                                                                       OTH0003
C PURPOSE: THIS SUBROUTINE PROVIDES OUTPUT CONCENTRATIONS IN            OTH0004
C          MICROGRAMS PER CUBIC METER FOR EACH HOUR FOR:                OTH0005
C           1) CONTRIBUTIONS FROM SIGNIFICANT SOURCES, AND              OTH0006
C           2) SUMMARIES.                                               OTH0007
C                                                                       OTH0008
C I/O:  NONE                                                            OTH0009
C                                                                       OTH0010
C CALLED BY:  MAIN                                                      OTH0011
C                                                                       OTH0012
C CALLS:  NONE                                                          OTH0013
C                                                                       OTH0014
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            OTH0015
C       RESEARCH TRIANGLE PARK, NC                                      OTH0016
C                                                                       OTH0017
C SDM 1.0         REVISION HISTORY:                                     OTH0018
C     79365       MPTER VERSION                                         OTH0019
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOTH0020
C                                                                       OTH0021
C        BEYOND ENTRY POINT OUTAVG THE SUBROUTINE PROVIDES              OTH0022
C        CONCENTRATION OUTPUT FOR EACH AVERAGING PERIOD AGAIN           OTH0023
C        IN THE ABOVE MANNER.                                           OTH0024
C                                                                       OTH0025
C->->->->SECTION OUTHR.A - COMMON, DIMENSION, AND DATA.                 OTH0026
C                                                                       OTH0027
      COMMON /MPOR/ IOPT(26)                                            OTH0028
      COMMON /MPO/ NRECEP,NAVG,NB,LH,NPT,IDATE(2),RREC(180),SREC(180),ZROTH0029
     1(180),ELR(180),PHCHI(180),PHSIGS(180,26),HSAV(250),DSAV(250),PCHI(OTH0030
     2180),PSIGS(180,26),IPOL                                           OTH0031
      COMMON /MO/ QTHETA(24),QU(24),IKST(24),QHL(24),QTEMP(24),MPS(25),NOTH0032
     1SIGP,IO,LINE1(20),LINE2(20),LINE3(20),IRANK(180)                  OTH0033
      COMMON /CHARS/ RNAME(2,180),STAR(5,180),SNAME(3)                  OTH0034
      CHARACTER*4 RNAME,STAR,SNAME                                      OTH0035
C                                                                       OTH0036
C                                                                       OTH0037
      CHARACTER*4 IPOLT(2),IPOLU                                        OTH0038
      DATA IPOLT /'SO2 ','PART'/                                        OTH0039
      IPOLU=IPOLT(1)                                                    OTH0040
      IF (IPOL.EQ.4) IPOLU=IPOLT(2)                                     OTH0041
C        OPTION(11): PRINT ONLY THE HOURLY SUMMARIES.                   OTH0042
      IF (IOPT(11).EQ.1) GO TO 100                                      OTH0043
C                                                                       OTH0044
C->->->->SECTION OUTHR.B - WRITE HOURLY CONTRIBUTION TITLE.             OTH0045
C                                                                       OTH0046
      WRITE (IO,350) LINE1,LINE2,LINE3                                  OTH0047
      WRITE (IO,360)IPOLU,IDATE,LH                                      OTH0048
C                                                                       OTH0049
C->->->->SECTION OUTHR.C - WRITE HOURLY MET DATA.                       OTH0050
C                                                                       OTH0051
      IF (IOPT(12).EQ.1) GO TO 10                                       OTH0052
      WRITE (IO,450)                                                    OTH0053
      WRITE (IO,460) LH,QTHETA(LH),QU(LH),QHL(LH),QTEMP(LH),IKST(LH)    OTH0054
C                                                                       OTH0055
C->->->->SECTION OUTHR.D - WRITE FINAL PLUME HEIGHT AND DISTANCE        OTH0056
C                          FINAL RISE.                                  OTH0057
C                                                                       OTH0058
10    IF (IOPT(13).EQ.1) GO TO 20                                       OTH0059
      WRITE (IO,470) (I,I=1,10)                                         OTH0060
C        HSAV ARE THE CALCULATED PLUME HEIGHTS FOR THIS HOUR            OTH0061
      WRITE (IO,480) (HSAV(I),I=1,NPT)                                  OTH0062
      WRITE (IO,490) (DSAV(I),I=1,NPT)                                  OTH0063
C                                                                       OTH0064
C->->->->SECTION OUTHR.E - WRITE HRLY SIGNIFICANT SOURCE CONTRIB.       OTH0065
C                                                                       OTH0066
20    IF (NSIGP.GT.10) GO TO 40                                         OTH0067
C        PRINT FIRST PAGE OF OUTPUT AND TOTALS FOR 10 OR LESS SIGNIF SOUOTH0068
      WRITE (IO,370)                                                    OTH0069
      WRITE (IO,380) (I,I=1,NSIGP)                                      OTH0070
      WRITE (IO,390)                                                    OTH0071
      WRITE (IO,380) (MPS(I),I=1,NSIGP)                                 OTH0072
      WRITE (IO,400)                                                    OTH0073
      DO 30 K=1,NRECEP                                                  OTH0074
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=1,NSIGP)      OTH0075
C        PRINT TOTALS                                                   OTH0076
      WRITE (IO,420) PHSIGS(K,26),PHCHI(K)                              OTH0077
30    CONTINUE                                                          OTH0078
      GO TO 100                                                         OTH0079
C        PRINT FIRST PAGE FOR MORE THAN 10 SIGNIFICANT SOURCES.         OTH0080
40    WRITE (IO,370)                                                    OTH0081
      WRITE (IO,380) (I,I=1,10)                                         OTH0082
      WRITE (IO,430) (MPS(I),I=1,10)                                    OTH0083
      WRITE (IO,400)                                                    OTH0084
      DO 50 K=1,NRECEP                                                  OTH0085
50    WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=1,10)         OTH0086
      IF (NSIGP.GT.20) GO TO 70                                         OTH0087
C        PRINT SECOND PAGE AND TOTALS FOR 11 TO 20 SIGNIFICANT SOURCES  OTH0088
      WRITE (IO,350) LINE1,LINE2,LINE3                                  OTH0089
      WRITE (IO,360)IPOLU,IDATE,LH                                      OTH0090
      WRITE (IO,370)                                                    OTH0091
      WRITE (IO,380) (I,I=11,NSIGP)                                     OTH0092
      WRITE (IO,390)                                                    OTH0093
      WRITE (IO,380) (MPS(I),I=11,NSIGP)                                OTH0094
      WRITE (IO,400)                                                    OTH0095
      DO 60 K=1,NRECEP                                                  OTH0096
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=11,NSIGP)     OTH0097
60    WRITE (IO,420) PHSIGS(K,26),PHCHI(K)                              OTH0098
      GO TO 100                                                         OTH0099
C        WRITE SECOND PAGE FOR MORE THAN 20 SIGNIFICANT SOURCES.        OTH0100
70    WRITE (IO,350) LINE1,LINE2,LINE3                                  OTH0101
      WRITE (IO,360)IPOLU,IDATE,LH                                      OTH0102
      WRITE (IO,370)                                                    OTH0103
      WRITE (IO,380) (I,I=11,20)                                        OTH0104
      WRITE (IO,430) (MPS(I),I=11,20)                                   OTH0105
      WRITE (IO,400)                                                    OTH0106
      DO 80 K=1,NRECEP                                                  OTH0107
80    WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=11,20)        OTH0108
      WRITE (IO,350) LINE1,LINE2,LINE3                                  OTH0109
      WRITE (IO,360)IPOLU,IDATE,LH                                      OTH0110
      WRITE (IO,370)                                                    OTH0111
C        WRITE LAST PAGE AND TOTALS FOR MORE THAN 20 SIGNIF. SOURCES.   OTH0112
      WRITE (IO,380) (I,I=21,NSIGP)                                     OTH0113
      WRITE (IO,390)                                                    OTH0114
      WRITE (IO,380) (MPS(I),I=21,NSIGP)                                OTH0115
      WRITE (IO,400)                                                    OTH0116
      DO 90 K=1,NRECEP                                                  OTH0117
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=21,NSIGP)     OTH0118
90    WRITE (IO,420) PHSIGS(K,26),PHCHI(K)                              OTH0119
C        OPTION(14): SKIP OUTPUT OF THE HOURLY SUMMARIES.               OTH0120
100   IF (IOPT(14).EQ.1) GO TO 170                                      OTH0121
C                                                                       OTH0122
C->->->->SECTION OUTHR.F - WRITE HOURLY SUMMARY TITLE.                  OTH0123
C                                                                       OTH0124
      WRITE (IO,350) LINE1,LINE2,LINE3                                  OTH0125
      WRITE (IO,440)IPOLU,IDATE,LH                                      OTH0126
C                                                                       OTH0127
C->->->->SECTION OUTHR.G - WRITE HOURLY MET DATA.                       OTH0128
C                                                                       OTH0129
      IF (IOPT(15).EQ.1) GO TO 110                                      OTH0130
      WRITE (IO,450)                                                    OTH0131
      WRITE (IO,460) LH,QTHETA(LH),QU(LH),QHL(LH),QTEMP(LH),IKST(LH)    OTH0132
C                                                                       OTH0133
C->->->->SECTION OUTHR.H - WRITE FINAL PLUME HEIGHT AND                 OTH0134
C        DISTANCE TO FINAL RISE.                                        OTH0135
C                                                                       OTH0136
110   IF (IOPT(16).EQ.1) GO TO 120                                      OTH0137
      WRITE (IO,470) (I,I=1,10)                                         OTH0138
C        HSAV ARE THE CALCULATED PLUME HEIGHTS FOR THIS HOUR            OTH0139
      WRITE (IO,480) (HSAV(I),I=1,NPT)                                  OTH0140
      WRITE (IO,490) (DSAV(I),I=1,NPT)                                  OTH0141
C                                                                       OTH0142
C->->->->SECTION OUTHR.I - WRITE HOURLY SUMMARY TABLE.                  OTH0143
C                                                                       OTH0144
120   WRITE (IO,500)                                                    OTH0145
C        CALCULATE GRAND TOTALS AND RANK CONCENTRATIONS                 OTH0146
      DO 130 K=1,NRECEP                                                 OTH0147
C        HSAV IS USED AS A DUMMY VARIABLE FOR THE REMAINDER OF THIS     OTH0148
C        SUBROUTINE. IT IS ZEROED AGAIN IN PTR BEFORE ITS NORMAL USE.   OTH0149
130   HSAV(K)=PHCHI(K)                                                  OTH0150
C        DETERMINE RANKING ACCORDING TO CONCENTRATION                   OTH0151
      DO 150 I=1,NRECEP                                                 OTH0152
      CMAX=-1.0                                                         OTH0153
      DO 140 K=1,NRECEP                                                 OTH0154
      IF (HSAV(K).LE.CMAX) GO TO 140                                    OTH0155
      CMAX=HSAV(K)                                                      OTH0156
      LMAX=K                                                            OTH0157
140   CONTINUE                                                          OTH0158
      IRANK(LMAX)=I                                                     OTH0159
      HSAV(LMAX)=-1.0                                                   OTH0160
150   CONTINUE                                                          OTH0161
      DO 160 K=1,NRECEP                                                 OTH0162
      WRITE (IO,510) K,STAR(1,K),STAR(2,K),(RNAME(J,K),J=1,2),RREC(K),SROTH0163
     1EC(K),ZR(K),ELR(K),PHSIGS(K,26),PHCHI(K),IRANK(K)                 OTH0164
160   CONTINUE                                                          OTH0165
170   RETURN                                                            OTH0166
                                                                        OTH0167
350   FORMAT ('1',20A4/1X,20A4/1X,20A4)                                 OTH0168
360   FORMAT('0',T30,A4,' CONTRIBUTION(MICROGRAMS/M**3) FROM SIGNIFICANTOTH0169
     1 POINT SOURCES ',5X,I2,'/',I4,' : HOUR ',I2//)                    OTH0170
370   FORMAT (1H0,T5,'RANK')                                            OTH0171
380   FORMAT ('+',T12,10(I3,7X))                                        OTH0172
390   FORMAT ('+',T113,'TOTAL     TOTAL'/1X,T113,'SIGNIF    ALL POINT'/1OTH0173
     1X,T113,'POINT     SOURCES'/1X,'SOURCE #')                         OTH0174
400   FORMAT (1X,'RECEP #')                                             OTH0175
410   FORMAT (1X,I3,2A1,6P,10F10.3)                                     OTH0176
420   FORMAT ('+',T109,6P,2F10.3)                                       OTH0177
430   FORMAT (1X,'SOURCE #',T12,10(I3,7X))                              OTH0178
440   FORMAT('0',T25,A4,' SUMMARY CONCENTRATION TABLE(MICROGRAMS/M**3) 'OTH0179
     1,5X,I2,'/',I4,' : HOUR ',I2/1X)                                   OTH0180
450   FORMAT (1X,T2,'HOUR   THETA    SPEED   MIXING   TEMP   STABILITY'/OTH0181
     11X,T9,'(DEG)    (M/S) HEIGHT(M)   (K)     CLASS'/1X)              OTH0182
460   FORMAT (1X,T3,I2,4F9.2,6X,I1//)                                   OTH0183
470   FORMAT (13X,10I11)                                                OTH0184
480   FORMAT (' FINAL HT (M) ',10F11.2)                                 OTH0185
490   FORMAT (' DIST FIN HT (KM)',10F11.3)                              OTH0186
500   FORMAT ('0',T7,'RECEPTOR',T23,'EAST',T33,'NORTH',T43,'RECEPTOR HT'OTH0187
     1,T61,'RECEPTOR',T78,'TOTAL FROM',T93,'TOTAL FROM',T106,'CONCENTRATOTH0188
     2ION'/' ',T7,'NO. NAME',T22,'COORD',T33,'COORD',T44,'ABV GRD (M)',TOTH0189
     359,'GRD-LVL ELEV',T77,'SIGNIF POINT',T93,'ALL SOURCES',T111,'RANK'OTH0190
     4/' ',T58,'(USER HT UNITS)',T80,'SOURCES'//)                       OTH0191
510   FORMAT (1H ,I8,2A1,2X,2A4,2F10.2,F12.1,F20.1,6P,2F15.4,I15)       OTH0192
520   FORMAT ('0',T22,I2,'-HOUR AVERAGE ',A4,' CONTRIBUTION(MICROGRAMS/MOTH0193
     1**3) FROM SIGNIFICANT POINT SOURCES',5X,I2,'/',I3,'  START HOUR: 'OTH0194
     2,I2//1X,T5,'RANK')                                                OTH0195
530   FORMAT ('0',T25,I2,'-HOUR AVERAGE ',A4,' SUMMARY CONCENTRATION TABOTH0196
     1LE(MICROGRAMS/M**3)',5X,I2,'/',I3,'  START HOUR: ',I2//1X)        OTH0197
540   FORMAT ('CNTL',1X,3F10.3,20X,I4,2F10.1)                           OTH0198
                                                                        OTH0199
      END                                                               OTH0200
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPGD0001
      SUBROUTINE PGYZ                                                   PGD0002
C                                                                       PGD0003
C PURPOSE: CALCULATES PASQUILL-GIFFORD DISPERSION PARAMETERS.           PGD0004
C          VERTICAL DISPERSION PARAMETER VALUE, SZ DETERMINED BY        PGD0005
C          SZ = A * X ** B WHERE A AND B ARE FUNCTIONS OF BOTH STABILITYPGD0006
C          AND RANGE OF X.                                              PGD0007
C          HORIZONTAL DISPERSION PARAMETER VALUE, SY DETERMINED BY      PGD0008
C          LOGARITHMIC INTERPOLATION OF PLUME HALF-ANGLE ACCORDING TO   PGD0009
C          DISTANCE AND CALCULATION OF 1/2.15 TIMES HALF-ARC LENGTH.    PGD0010
C                                                                       PGD0011
C I/O:  NONE                                                            PGD0012
C                                                                       PGD0013
C CALLED BY:  RCP                                                       PGD0014
C                                                                       PGD0015
C CALLS:  NONE                                                          PGD0016
C                                                                       PGD0017
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            PGD0018
C       RESEARCH TRIANGLE PARK, NC                                      PGD0019
C                                                                       PGD0020
C SDM 1.0         REVISION HISTORY:                                     PGD0021
C     79365       MPTER VERSION                                         PGD0022
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPGD0023
                                                                        PGD0024
      COMMON /MPR/ UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC,MUOR                PGD0025
      DIMENSION XA(7), XB(2), XD(5), XE(8), XF(9), AA(8), BA(8), AB(3), PGD0026
     1BB(3), AD(6), BD(6), AE(9), BE(9), AF(10), BF(10)                 PGD0027
      DATA XA /.5,.4,.3,.25,.2,.15,.1/                                  PGD0028
      DATA XB /.4,.2/                                                   PGD0029
      DATA XD /30.,10.,3.,1.,.3/                                        PGD0030
      DATA XE /40.,20.,10.,4.,2.,1.,.3,.1/                              PGD0031
      DATA XF /60.,30.,15.,7.,3.,2.,1.,.7,.2/                           PGD0032
      DATA AA /453.85,346.75,258.89,217.41,179.52,170.22,158.08,122.8/  PGD0033
      DATA BA /2.1166,1.7283,1.4094,1.2644,1.1262,1.0932,1.0542,.9447/  PGD0034
      DATA AB /109.30,98.483,90.673/                                    PGD0035
      DATA BB /1.0971,0.98332,0.93198/                                  PGD0036
      DATA AD /44.053,36.650,33.504,32.093,32.093,34.459/               PGD0037
      DATA BD /0.51179,0.56589,0.60486,0.64403,0.81066,0.86974/         PGD0038
      DATA AE /47.618,35.420,26.970,24.703,22.534,21.628,21.628,23.331,2PGD0039
     14.26/                                                             PGD0040
      DATA BE /0.29592,0.37615,0.46713,0.50527,0.57154,0.63077,0.75660,0PGD0041
     1.81956,0.8366/                                                    PGD0042
      DATA AF /34.219,27.074,22.651,17.836,16.187,14.823,13.953,13.953,1PGD0043
     14.457,15.209/                                                     PGD0044
      DATA BF /0.21716,0.27436,0.32681,0.41507,0.46490,0.54503,0.63227,0PGD0045
     1.68465,0.78407,0.81558/                                           PGD0046
C                                                                       PGD0047
      IF (MUOR.EQ.2)  GO TO 9                                           PGD0048
C                                                                       PGD0049
C           MCELROY-POOLER URBAN DISPERSION PARAMETERS FROM ST. LOUIS   PGD0050
C             EXPERIMENT AS PUT IN EQUATION FORM BY BRIGGS.             PGD0051
C              X IS DISTANCE IN KM.                                     PGD0052
C              KST IS PASQUILL STABILITY CLASS.                         PGD0053
C              SY AND SZ ARE IN METERS.                                 PGD0054
      GO TO(2,2,3,4,5,5), KST                                           PGD0055
2     SY=320.*X/SQRT(1.+0.4*X)                                          PGD0056
      SZ=240.*X*SQRT(1.+X)                                              PGD0057
      GO TO 6                                                           PGD0058
3     SY=220.*X/SQRT(1.+0.4*X)                                          PGD0059
      SZ=200.*X                                                         PGD0060
      GO TO 6                                                           PGD0061
4     SY=160.*X/SQRT(1.+0.4*X)                                          PGD0062
      SZ=140.*X/SQRT(1.+0.3*X)                                          PGD0063
      GO TO 6                                                           PGD0064
5     SY=110.*X/SQRT(1.+0.4*X)                                          PGD0065
      SZ=80.*X/SQRT(1.+1.5*X)                                           PGD0066
6     IF (SZ.GT.5000.) SZ=5000.                                         PGD0067
      RETURN                                                            PGD0068
C                                                                       PGD0069
    9 XY=X                                                              PGD0070
      GO TO (10,40,70,80,110,140), KST                                  PGD0071
C        STABILITY A                                                    PGD0072
10    TH=(24.167-2.5334*ALOG(XY))/57.2958                               PGD0073
      IF (X.GT.3.11) GO TO 170                                          PGD0074
      DO 20 ID=1,7                                                      PGD0075
      IF (X.GE.XA(ID)) GO TO 30                                         PGD0076
20    CONTINUE                                                          PGD0077
      ID=8                                                              PGD0078
30    SZ=AA(ID)*X**BA(ID)                                               PGD0079
      GO TO 190                                                         PGD0080
C        STABILITY B                                                    PGD0081
40    TH=(18.333-1.8096*ALOG(XY))/57.2958                               PGD0082
      IF (X.GT.35.) GO TO 170                                           PGD0083
      DO 50 ID=1,2                                                      PGD0084
      IF (X.GE.XB(ID)) GO TO 60                                         PGD0085
50    CONTINUE                                                          PGD0086
      ID=3                                                              PGD0087
60    SZ=AB(ID)*X**BB(ID)                                               PGD0088
      GO TO 180                                                         PGD0089
C        STABILITY C                                                    PGD0090
70    TH=(12.5-1.0857*ALOG(XY))/57.2958                                 PGD0091
      SZ=61.141*X**0.91465                                              PGD0092
      GO TO 180                                                         PGD0093
C        STABILITY D                                                    PGD0094
80    TH=(8.3333-0.72382*ALOG(XY))/57.2958                              PGD0095
      DO 90 ID=1,5                                                      PGD0096
      IF (X.GE.XD(ID)) GO TO 100                                        PGD0097
90    CONTINUE                                                          PGD0098
      ID=6                                                              PGD0099
100   SZ=AD(ID)*X**BD(ID)                                               PGD0100
      GO TO 180                                                         PGD0101
C        STABILITY E                                                    PGD0102
110   TH=(6.25-0.54287*ALOG(XY))/57.2958                                PGD0103
      DO 120 ID=1,8                                                     PGD0104
      IF (X.GE.XE(ID)) GO TO 130                                        PGD0105
120   CONTINUE                                                          PGD0106
      ID=9                                                              PGD0107
130   SZ=AE(ID)*X**BE(ID)                                               PGD0108
      GO TO 180                                                         PGD0109
C        STABILITY F                                                    PGD0110
140   TH=(4.1667-0.36191*ALOG(XY))/57.2958                              PGD0111
      DO 150 ID=1,9                                                     PGD0112
      IF (X.GE.XF(ID)) GO TO 160                                        PGD0113
150   CONTINUE                                                          PGD0114
      ID=10                                                             PGD0115
160   SZ=AF(ID)*X**BF(ID)                                               PGD0116
      GO TO 180                                                         PGD0117
170   SZ=5000.                                                          PGD0118
      GO TO 190                                                         PGD0119
180   IF (SZ.GT.5000.) SZ=5000.                                         PGD0120
190   SY=465.116*XY*SIN(TH)/COS(TH)                                     PGD0121
C        465.116 = 1000. (M/KM) /2.15                                   PGD0122
      RETURN                                                            PGD0123
C                                                                       PGD0124
      END                                                               PGD0125
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPTR0001
      SUBROUTINE PTR(IDAY,PNAME)                                        PTR0002
C                                                                       PTR0003
C PURPOSE:  SET UP SOURCE AND RECEPTOR INFORMATION FOR POINT SOURCE     PTR0004
C           CONCENTRATION CALCULATIONS                                  PTR0005
C                                                                       PTR0006
C I/O:  IDAY, JULIAN DAY                                                PTR0007
C       PNAME, SOURCE NAME                                              PTR0008
C                                                                       PTR0009
C CALLED BY:  MAIN                                                      PTR0010
C                                                                       PTR0011
C CALLS:  INTERF                                                        PTR0012
C         RCP                                                           PTR0013
C                                                                       PTR0014
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            PTR0015
C       RESEARCH TRIANGLE PARK, NC                                      PTR0016
C                                                                       PTR0017
C SDM 1.0         REVISION HISTORY:                                     PTR0018
C     81350       MPTER VERSION                                         PTR0019
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPTR0020
C                                                                       PTR0021
C->->->->SECTION PTR.A - COMMON AND DIMENSION.                          PTR0022
C                                                                       PTR0023
      COMMON /MPOR/ IOPT(26)                                            PTR0024
                                                                        PTR0025
      COMMON /MPO/ NRECEP,NAVG,NB,LH,NPT,IDATE(2),RREC(180),SREC(180),ZRPTR0026
     1(180),ELR(180),PHCHI(180),PHSIGS(180,26),HSAV(250),DSAV(250),PCHI(PTR0027
     2180),PSIGS(180,26),IPOL                                           PTR0028
                                                                        PTR0029
      REAL MGCM                                                         PTR0030
      COMMON /SDMONE/XP,YP,A,B,UL,US,HSTK,CN,F,Q,MGCM                   PTR0031
                                                                        PTR0032
      COMMON /MPR/ UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC,MUOR                PTR0033
                                                                        PTR0034
      COMMON /MP/ SOURCE(9,250),CONTWO,PSAV(250),IPSIGS(250),U,TEMP,SINTPTR0035
     1,COST,PL(6),ELP(250),ELHN,HANE,TLOS,CELM,CTER                     PTR0036
                                                                        PTR0037
      DIMENSION UPH(250), HPR(250), FP(250), DH(250), PARTC(250)        PTR0038
      CHARACTER*4 PNAME(3,250)                                          PTR0039
C  *** DCD Modification 11/14/90                                        PTR0040
C      Add Common Block LOCAL to save Local Variables in INTERF         PTR0041
      COMMON /LOCAL/ DIST,HO,DTHDZ,PTMOL,PTMOW,TRAD                     PTR0042
C                                                                       PTR0043
C     FLAG - ADDED FOR SDM (6-88)                                       PTR0044
C                                                                       PTR0045
      INTEGER FLAG                                                      PTR0046
C                                                                       PTR0047
C->->->->SECTION PTR.B - INITIALIZE AND START RECEPTOR LOOP.            PTR0048
C                                                                       PTR0049
C        ZERO EFFECTIVE STACK HEIGHT FOR EACH SOURCE                    PTR0050
C                                                                       PTR0051
C          NPT - THE NUMBER OF POINT SOURCES                            PTR0052
      FLAG=0                                                            PTR0053
      DO 10 J=1,NPT                                                     PTR0054
C        HSAV WILL BE USED TO STORE THE SOURCE PLUME HEIGHTS.           PTR0055
10    HSAV(J)=0.0                                                       PTR0056
C        LOOP ON RECEPTORS                                              PTR0057
C          NRECEP - THE NUMBER OF RECEPTORS                             PTR0058
C                                                                       PTR0059
C->->->->SECTION PTR.C - START SOURCES LOOP, CALCULATE                  PTR0060
C                        UPWIND AND CROSSWIND DISTANCES.                PTR0061
C                                                                       PTR0062
      DO 170 J=1,NPT                                                    PTR0063
      IF (FLAG.EQ.1.OR.FLAG.EQ.3) FLAG=4                                PTR0064
      DO 180 K=1,NRECEP                                                 PTR0065
C        IF IOPT(1)=1, TERRAIN ADJUSTMENTS ARE MADE.                    PTR0066
      IF (IOPT(1).EQ.0) GO TO 20                                        PTR0067
C          ELR - RECEPTOR GROUND LEVEL ELEVATION                        PTR0068
      ER=ELR(K)                                                         PTR0069
C          ELHN - LOWEST SOURCE STACK-TOP ELEVATION?                    PTR0070
      IF (ER.LE.ELHN) GO TO 20                                          PTR0071
      PCHI(K)=99999.E+26                                                PTR0072
      PHCHI(K)=99999.E+26                                               PTR0073
      GO TO 180                                                         PTR0074
20    CONTINUE                                                          PTR0075
C          ZR - RECEPOR HEIGHT ABOVE GROUND                             PTR0076
      Z=ZR(K)                                                           PTR0077
      PARTC(J)=0.0                                                      PTR0078
C          RQ - EAST COORDINATE OF THE SOURCE                           PTR0079
      RQ=SOURCE(1,J)                                                    PTR0080
C          SQ - NORTH COORDINATE OF THE SOURCE                          PTR0081
      SQ=SOURCE(2,J)                                                    PTR0082
C          ELP - SOURCE GROUND LEVEL ELEVATION                          PTR0083
      EP=ELP(J)                                                         PTR0084
C        DETERMINE UPWIND DISTANCE                                      PTR0085
C        XDUM,YDUM IN USER UNITS. X,Y IN KM.                            PTR0086
C          RREC - EAST COORDINATE OF THE RECEPTOR                       PTR0087
      XDUM=RQ-RREC(K)                                                   PTR0088
C          SREC - NORTH COORDINATE OF THE RECEPTOR                      PTR0089
      YDUM=SQ-SREC(K)                                                   PTR0090
C        SINT AND COST ARE THE SIN AND COS OF THE WIND DIRECTION        PTR0091
C        CONTWO - MULTIPLIER CONSTANT TO CONVERT USER UNITS TO KM       PTR0092
      X=(YDUM*COST+XDUM*SINT)*CONTWO                                    PTR0093
C         X IS THE UPWIND DISTANCE  OF THE SOURCE FROM THE RECEPTOR.    PTR0094
C        IF X IS NEGATIVE, INDICATING THAT THE SOURCE IS DOWNWIND OF    PTR0095
C        THE RECEPTOR, THE CALCULATION IS TERMINATED ASSUMING NO        PTR0096
C        CONTRIBUTION FROM THAT SOURCE.                                 PTR0097
      IF (X.LE.0.0) GO TO 180                                           PTR0098
C                                                                       PTR0099
C        DETERMINE CROSSWIND DISTANCE                                   PTR0100
C                                                                       PTR0101
      Y=(YDUM*SINT-XDUM*COST)*CONTWO                                    PTR0102
      H=HSAV(J)                                                         PTR0103
C        SKIP PLUME RISE CALCULATION IF EFFECTIVE HT. HAS ALREADY BEEN  PTR0104
C           CALCULATED FOR THIS SOURCE                                  PTR0105
      IF (H.EQ.0.0) GO TO 30                                            PTR0106
      DELH=DH(J)                                                        PTR0107
C                                                                       PTR0108
C->->->->SECTION PTR.D - EXTRAPOLATE WIND SPEED TO STACK TOP            PTR0109
C                        CALCULATE PLUME RISE.                          PTR0110
C                                                                       PTR0111
      GO TO 110                                                         PTR0112
C         MODIFY WIND SPEED BY POWER LAW PROFILE IN ORDER TO TAKE INTO  PTR0113
C        ACCOUNT THE INCREASE OF WIND SPEED WITH HEIGHT.                PTR0114
C        ASSUME WIND MEASUREMENTS ARE REPRESENTATIVE FOR HEIGHT = HANE. PTR0115
C         THT IS THE PHYSICAL STACK HEIGHT                              PTR0116
30    THT=SOURCE(5,J)                                                   PTR0117
C        POINT SOURCE HEIGHT NOT ALLOWED TO BE LESS THAN 1 METER.       PTR0118
      IF (THT.LT.1.) THT=1.                                             PTR0119
C          U - WIND SPEED AT HEIGHT 'HANE'                              PTR0120
C          PL - POWER FOR THE WIND PROFILE                              PTR0121
C          UPL - WIND AT THE PHYSICAL STACK HEIGHT                      PTR0122
      UPL=U*(THT/HANE)**PL(KST)                                         PTR0123
C        WIND SPEED NOT ALLOWED TO BE LESS THAN 1 METER/SEC.            PTR0124
      IF (UPL.LT.1.) UPL=1.                                             PTR0125
C        STORE THE STACK TOP WIND FOR THE JTH SOURCE FOR THIS HOUR      PTR0126
      UPH(J)=UPL                                                        PTR0127
      VS=SOURCE(8,J)                                                    PTR0128
      BUOY=SOURCE(9,J)                                                  PTR0129
      TS=SOURCE(6,J)                                                    PTR0130
C        TEMP- THE AMBIENT AIR TEMPERATURE FOR THIS HOUR                PTR0131
      DELT=TS-TEMP                                                      PTR0132
      F=BUOY*DELT/TS                                                    PTR0133
C         IOPT(6) HOURLY EMISSION INPUT FROM TAPE/DISK?  0=NO, 1=YES.   PTR0134
      IF (IOPT(6).EQ.0) GO TO 40                                        PTR0135
C        MODIFY EXIT VELOCITY AND BUOYANCY BY RATIO OF HOURLY EMISSIONS PTR0136
C        TO AVERAGE EMISSIONS                                           PTR0137
      SCALE = SOURCE(IPOL,J)/PSAV(J)                                    PTR0138
      VS = VS*SCALE                                                     PTR0139
      F = F*SCALE                                                       PTR0140
40    D=SOURCE(7,J)                                                     PTR0141
C                                                                       PTR0142
C*****PLUME RISE AND STACK TIP DOWNWASH CALCULATIONS                    PTR0143
C                                                                       PTR0144
C        CALCULATE  H PRIME WHICH TAKES INTO ACCOUNT STACK DOWNWASH     PTR0145
C        BRIGGS(1973) PAGE 4                                            PTR0146
      HPRM=THT                                                          PTR0147
C        IF IOPT(2)=1, THEN NO STACK DOWNWASH COMPUTATION               PTR0148
      IF (IOPT(2).EQ.1) GO TO 50                                        PTR0149
      DUM=VS/UPL                                                        PTR0150
      IF (DUM.LT.1.5) HPRM=THT+2.*D*(DUM-1.5)                           PTR0151
C        'HPRM' IS BRIGGS' H-PRIME                                      PTR0152
      IF (HPRM.LT.0.) HPRM=0.                                           PTR0153
C                                                                       PTR0154
C        CALCULATE PLUME RISE                                           PTR0155
C         MOMENTUM RISE EQUATION                                        PTR0156
C                                                                       PTR0157
50    DELHM=3.*VS*D/UPL                                                 PTR0158
      IF(KST.GT.4)GO TO 70                                              PTR0159
C                                                                       PTR0160
C        PLUME RISE FOR NEUTRAL - UNSTABLE CONDITIONS                   PTR0161
C                                                                       PTR0162
      IF(TS.LT.TEMP)GO TO 80                                            PTR0163
      IF(F.GE.55.)GO TO 60                                              PTR0164
C                                                                       PTR0165
C        COMBINATION OF BRIGG'S(1971) EQNS. 6&7, PAGE 1031, FOR F<55.   PTR0166
C                                                                       PTR0167
      DELH=21.425*F**0.75/UPL                                           PTR0168
      IF(DELHM.GT.DELH)GO TO 80                                         PTR0169
      DISTF=0.049*F**0.625                                              PTR0170
      GO TO 100                                                         PTR0171
C                                                                       PTR0172
C        COMBINATION OF BRIGG'S(1971) EQNS. 6&7, PAGE 1031, FOR F>=55.  PTR0173
C                                                                       PTR0174
60    DELH=38.71*F**0.6/UPL                                             PTR0175
      IF(DELHM.GT.DELH)GO TO 80                                         PTR0176
      DISTF=0.119*F**0.4                                                PTR0177
      GO TO 100                                                         PTR0178
C                                                                       PTR0179
C        PLUME RISE FOR STABLE CONDITIONS                               PTR0180
C                                                                       PTR0181
70    DTHDZ=0.02                                                        PTR0182
      IF(KST.GT.5)DTHDZ=0.035                                           PTR0183
      S=9.80616*DTHDZ/TEMP                                              PTR0184
C                                                                       PTR0185
C        MOMENTUM RISE EQUATION                                         PTR0186
C        BRIGG'S(1969) EQUATION 4.28, PAGE 59                           PTR0187
C                                                                       PTR0188
      DHA=1.5*(VS*VS*D*D*TEMP/(4.*TS*UPL))**0.333333/S**0.166667        PTR0189
      IF(DHA.LT.DELHM)DELHM=DHA                                         PTR0190
      IF(TS.LT.TEMP)GO TO 80                                            PTR0191
C                                                                       PTR0192
C        STABLE, BUOYANT RISE (WITH WIND)                               PTR0193
C                                                                       PTR0194
      DELH=2.6*(F/(UPL*S))**0.333333                                    PTR0195
      IF(DELHM.GT.DELH)GO TO 80                                         PTR0196
      DISTF=0.0020715*UPL/SQRT(S)                                       PTR0197
      GO TO 100                                                         PTR0198
80    DELH=DELHM                                                        PTR0199
      DISTF=0.                                                          PTR0200
100   H=HPRM+DELH                                                       PTR0201
105   HSAV(J)=H                                                         PTR0202
      DH(J)=DELH                                                        PTR0203
      DSAV(J)=DISTF                                                     PTR0204
      UPH(J)=UPL                                                        PTR0205
      HPR(J)=HPRM                                                       PTR0206
      FP(J)=F                                                           PTR0207
C        IF SOURCE-RECEPTOR DISTANCE IS GREATER OR EQUAL TO DISTANCE TO PTR0208
C        FINAL RISE, SKIP PLUME RISE CALCULATION AND USE FINAL RISE.    PTR0209
110   IF (X.GE.DSAV(J)) GO TO 120                                       PTR0210
      IF (IOPT(4).EQ.0.AND.IOPT(3).EQ.1) GO TO 120                      PTR0211
C        CALCULATE GRADUAL PLUME RISE IF (1) THE USER SPECIFIES SO,     PTR0212
C        OR (2) USER EMPLOYS CALCULATION OF INITIAL DISPERSION.....     PTR0213
C        IN THIS CASE, USE OF FINAL EFFECTIVE HEIGHT IN THE CALCULATION PTR0214
C       OF DISPERSION COEFFICIENTS COULD LEAD TO MISLEADING VALUES SINCEPTR0215
C          SIGMA-Y,-Z =  DELTA-H/3.5                                    PTR0216
      DELH=160.*FP(J)**0.333333*X**0.666667/UPH(J)                      PTR0217
C        PLUME RISE FOR DISTANCE X(160 IS 1.6*1000**.67 BECAUSE X IN KM)PTR0218
      IF (DELH.GT.DH(J)) DELH=DH(J)                                     PTR0219
      IF (IOPT(3).EQ.1) GO TO 120                                       PTR0220
C        IF SPECIFYING CALCULATION OF INITIAL DISPERSION BUT ARE NOT    PTR0221
C        SPECIFYING CALCULATION OF GRADUAL PLUME RISE, THEN DO NOT      PTR0222
C        ADD THE NEW GRADUAL DELTA-H TO THE EFFECTIVE HEIGHT. OTHERWISE,PTR0223
C        CHECK THE GRADUAL RISE PLUME HEIGHT WITH FINAL EFFECTIVE HEIGHTPTR0224
C        AND SET THE PLUME HEIGHT TO THE SMALLER OF THE TWO VALUES.     PTR0225
        H=HPR(J)+DELH                                                   PTR0226
C        ADD PLUME RISE TO STACK HEIGHT FOR TOTAL EFFECTIVE STACK HT.   PTR0227
C        END PLUME RISE CALCULATION                                     PTR0228
120   UPL=UPH(J)                                                        PTR0229
C                                                                       PTR0230
C->->->->SECTION PTR.E - CALCULATE THE CONTRIBUTION OF                  PTR0231
C                        ONE SOURCE TO ONE RECEPTOR.                    PTR0232
C                                                                       PTR0233
C ** DCD Modification 11/14/90                                          PTR0234
C    Plume could be above the mixing height and still fumigate          PTR0235
C    Therefore, comment out next four lines                             PTR0236
C    Save plume height without terrain adjustments (HNOTER)             PTR0237
C     IF(KST.GT.4)GOTO130                                               PTR0238
C     IF (H.LT.HL) GO TO 130                                            PTR0239
C     PROD=0.                                                           PTR0240
C     GO TO 150                                                         PTR0241
C        IF IOPT(1) = 1, TERRAIN ADJUSTMENTS ARE MADE                   PTR0242
130   HNOTER = H                                                        PTR0243
      IF (IOPT(1).EQ.0) GO TO 140                                       PTR0244
      DUM=ER-EP                                                         PTR0245
      H=H+CELM*(CTER*DUM-DUM)                                           PTR0246
C         RCP RETURNS THE DISPERSION PARAMETERS, SY AND SZ (METERS)     PTR0247
C        AND THE RELATIVE CONCENTRATION VALUES CHI/Q (SEC/M**3)         PTR0248
                                                                        PTR0249
140   CALL INTERF(FLAG,IDAY,LH,J)                                       PTR0250
C ** DCD Modification 11/19/90                                          PTR0251
C    Do not make MPTER calculations if plume is above mixing height     PTR0252
      IF(HNOTER .GE. HL) THEN                                           PTR0253
         PROD = 0.                                                      PTR0254
         GO TO 150                                                      PTR0255
      ELSE                                                              PTR0256
         IF (FLAG.NE.1)  CALL RCP                                       PTR0257
      ENDIF                                                             PTR0258
C END OF DCD MODIFICATIONS                                              PTR0259
C        CALCULATE TRAVEL TIME IN KM-SEC/M TO INCLUDE DECAY RATE OF     PTR0260
C        POLLUTANT.                                                     PTR0261
      TT=X/UPL                                                          PTR0262
C        TLOS IN METERS/KM-SEC, SO TT*TLOS IS DIMENSIONLESS             PTR0263
C        INCLUDE THE POLLUTANT LOSS                                     PTR0264
      PROD=RC*SOURCE(IPOL,J)/EXP(TT*TLOS)                               PTR0265
C                                                                       PTR0266
C     SHORELINE DISPERSION MODEL SPECIAL OUTPUT                         PTR0267
C                                                                       PTR0268
      IF (FLAG.EQ.1.AND.PROD.GT.5.0E-10) WRITE (20,1000)                PTR0269
     & (PNAME(I,J),I=1,3),RREC(K),SREC(K),PROD*1E6                      PTR0270
C                                                                       PTR0271
C        IF HAFL IS ZERO, TLOS WILL START AS ZERO AND                   PTR0272
C        RESULT IN NO COMPUTATION OF POLLUTANT LOSS.                    PTR0273
C        INCREMENT CONCENTRATION AT K-TH RECEPTOR(G/M**3)               PTR0274
C          PCHI - SUM FOR THE AVERAGING TIME AT RECEPTOR K              PTR0275
150   PCHI(K)=PCHI(K)+PROD                                              PTR0276
C          PHCHI - CONCENTRATION FOR THIS HOUR AT RECEPTOR K            PTR0277
      PHCHI(K)=PHCHI(K)+PROD                                            PTR0278
      KSIG=IPSIGS(J)                                                    PTR0279
      IF (KSIG.EQ.0) GO TO 160                                          PTR0280
C        STORE CONCENTRATIONS FROM SIGNIFICANT SOURCES.(G/M**3)         PTR0281
      PSIGS(K,KSIG)=PSIGS(K,KSIG)+PROD                                  PTR0282
      PHSIGS(K,KSIG)=PHSIGS(K,KSIG)+PROD                                PTR0283
      PSIGS(K,26)=PSIGS(K,26)+PROD                                      PTR0284
      PHSIGS(K,26)=PHSIGS(K,26)+PROD                                    PTR0285
160   PARTC(J)=PROD                                                     PTR0286
C                                                                       PTR0287
C->->->->SECTION PTR.F - END SOURCE AND RECEPTOR LOOPS.                 PTR0288
C                                                                       PTR0289
180   CONTINUE                                                          PTR0290
C        END OF LOOP FOR SOURCES                                        PTR0291
C        WRITE PARTIAL CONCENTRATIONS ON DISK(G/M**3)  IF IOPT(21) = 1. PTR0292
C      IF (IOPT(21).EQ.0) GO TO 180                                     PTR0293
C        USER PLEASE NOTE: PARTIAL CONC. IN G/M**3, NOT MICROGRAM/M**3  PTR0294
C      WRITE (10) IDATE,LH,K,(PARTC(J),J=1,NPT)                         PTR0295
170   CONTINUE                                                          PTR0296
C                                                                       PTR0297
C                                                                       PTR0298
C                                                                       PTR0299
      IF (FLAG.NE.1.AND.FLAG.NE.3) GO TO 200                            PTR0300
      DO 190 K=1,NRECEP                                                 PTR0301
      IF (PHCHI(K).GT.5.E-10) WRITE (20,1000) 'ALL ','SOUR','CES ',     PTR0302
     & RREC(K),SREC(K),PHCHI(K)*1E6                                     PTR0303
 1000 FORMAT (' CONC:',3A4,F13.3,F13.3,F10.3)                           PTR0304
  190 CONTINUE                                                          PTR0305
  200 CONTINUE                                                          PTR0306
C        END OF LOOP FOR RECEPTORS                                      PTR0307
      RETURN                                                            PTR0308
C                                                                       PTR0309
C***   SECTIONS OF SUBROUTINE PTR.                                      PTR0310
C         SECTION PTR.A -  COMMON AND DIMENSION.                        PTR0311
C         SECTION PTR.B -  INITIALIZE AND START RECEPTOR LOOP.          PTR0312
C         SECTION PTR.C -  START SOURCES LOOP; CALCULATE UPWIND AND     PTR0313
C                           CROSSWIND DISTANCES.                        PTR0314
C         SECTION PTR.D -  EXTRAPOLATE WIND SPEED TO STACK TOP;         PTR0315
C                           CALCULATE PLUME RISE.                       PTR0316
C         SECTION PTR.E -  CALCULATE CONTRIBUTION FROM A SOURCE TO ONE  PTR0317
C                           RECEPTOR.                                   PTR0318
C         SECTION PTR.F -  END SOURCE AND RECEPTOR LOOPS.               PTR0319
C                                                                       PTR0320
      END                                                               PTR0321
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCRNK0001
      SUBROUTINE RANK (L)                                               RNK0002
C                                                                       RNK0003
C PURPOSE:  ARRANGE CONCENTRATIONS OF VARIOUS AVERAGING TIMES INTO      RNK0004
C           HIGH FIVE TABLES.  HMAXA ARRAY STORES THE HIGHEST FIVE      RNK0005
C           CONCENTRATIONS FOR EACH RECEPTOR FOR EACH AVERAGING         RNK0006
C           TIME.                                                       RNK0007
C                                                                       RNK0008
C I/O:  L, AVERAGING TIME                                               RNK0009
C                                                                       RNK0010
C CALLED BY:  MAIN                                                      RNK0011
C                                                                       RNK0012
C CALLS:  NONE                                                          RNK0013
C                                                                       RNK0014
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            RNK0015
C       RESEARCH TRIANGLE PARK, NC                                      RNK0016
C                                                                       RNK0017
C SDM 1.0         REVISION HISTORY:                                     RNK0018
C     79365       MPTER VERSION                                         RNK0019
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCRNK0020
C                                                                       RNK0021
C        VARIABLES OUTPUT:                                              RNK0022
C             HMAXA(J,K,L)  CONCENTRATIONS ACCORDING TO                 RNK0023
C                   J : RANK OF CONC. (1-5)                             RNK0024
C                   K : RECEPTOR NUMBER                                 RNK0025
C                   L : AVG TIME                                        RNK0026
C             NDAY(J,K,L) : ASSOCIATED DAY OF CONC.                     RNK0027
C             IHR(J,K,L) : ENDING HOUR OF CONC.                         RNK0028
      COMMON /MSFM/ MSFMFL                                              RNK0029
      INTEGER MSFMFL(50,24)                                             RNK0030
      COMMON/MR/HMAXA(5,180,5),NDAY(5,180,5),IHR(5,180,5),CONC(180,5),  RNK0031
     1          JDAY,NR                                                 RNK0032
      COMMON /MPO/ NRECEP,NAVG,NB,LH,NPT,IDATE(2),RREC(180),SREC(180),ZRRNK0033
     1(180),ELR(180),PHCHI(180),PHSIGS(180,26),HSAV(250),DSAV(250),PCHI(RNK0034
     2180),PSIGS(180,26),IPOL                                           RNK0035
      IO=6                                                              RNK0036
C        RESET AVERAGING PERIOD FLAG AND SET CALM FLAG, LL.             RNK0037
C        CALMS ACCOUNTED FOR ONLY WHEN DEFAULT OPTION ON.               RNK0038
      LL=0                                                              RNK0039
      IF(L.GT.4)LL=1                                                    RNK0040
      IF (L.GT.100) LL=2                                                RNK0041
      IF (L.EQ.111) L=1                                                 RNK0042
      IF(L.EQ.22.OR.L.EQ.222)L=2                                        RNK0043
      IF(L.EQ.33.OR.L.EQ.333)L=3                                        RNK0044
      IF(L.EQ.44.OR.L.EQ.444)L=4                                        RNK0045
      DO 50 K=1,NRECEP                                                  RNK0046
      IF (CONC(K,L).LE.HMAXA(5,K,L)) GO TO 50                           RNK0047
      DO 10 J=1,5                                                       RNK0048
      IF (CONC(K,L).GT.HMAXA(J,K,L)) GO TO 20                           RNK0049
C           CONCENTRATION IS ONE OF THE TOP FIVE                        RNK0050
10    CONTINUE                                                          RNK0051
      WRITE (IO,70)                                                     RNK0052
      GO TO 50                                                          RNK0053
C        THE FOLLOWING DO-LOOP HAS THE EFFECT OF INSERTING A NEW        RNK0054
C        CONCENTRATION ENTRY INTO ITS PROPER POSITION WHILE SHIFTING    RNK0055
C        DOWN THE 'OLD' LOWER CONCENTRATIONS THUS ESTABLISHING THE      RNK0056
C        'HIGH-FIVE' CONCENTRATION TABLE.                               RNK0057
20    IF (J.EQ.5) GO TO 40                                              RNK0058
      DO 30 IJ=4,J,-1                                                   RNK0059
      IJP1=IJ+1                                                         RNK0060
      HMAXA(IJP1,K,L)=HMAXA(IJ,K,L)                                     RNK0061
      NDAY(IJP1,K,L) = NDAY(IJ,K,L)                                     RNK0062
30    IHR(IJP1,K,L) = IHR(IJ,K,L)                                       RNK0063
C           INSERT LATEST CONC, DAY AND ENDING HR INTO THE              RNK0064
C           PROPER RANK IN THE HIGH-FIVE TABLE                          RNK0065
40    HMAXA(J,K,L)=CONC(K,L)                                            RNK0066
      NDAY(J,K,L) = JDAY                                                RNK0067
      IHR(J,K,L) = LH                                                   RNK0068
C        ADD 100 TO HOUR TO SET CALM FLAG FOR MAIN.                     RNK0069
      IF(LL.EQ.1.AND.L.NE.1)IHR(J,K,L)=IHR(J,K,L)+100                   RNK0070
C   DCD Modification 11/15/90                                           RNK0071
C   Comment out following line which refers to MSFMHR                   RNK0072
C   fumigation flag                                                     RNK0073
C     IF (LL.EQ.2) IHR(J,K,L)=IHR(J,K,L)+200                            RNK0074
50    CONTINUE                                                          RNK0075
      DO 60 K=1,NRECEP                                                  RNK0076
      CONC(K,L)=0.                                                      RNK0077
60    CONTINUE                                                          RNK0078
      RETURN                                                            RNK0079
C                                                                       RNK0080
70    FORMAT (1X,'   ****ERROR IN FINDING THE MAX CONCENTRATION***')    RNK0081
C                                                                       RNK0082
      END                                                               RNK0083
                                                                        RNK0084
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCRCP0001
      SUBROUTINE RCP                                                    RCP0002
C                                                                       RCP0003
C PURPOSE: DETERMINES NORMALIZED (CHI/Q) CONCENTRATIONS FROM EACH       RCP0004
C          POINT SOURCE FOR EACH RECEPTOR.                              RCP0005
C                                                                       RCP0006
C I/O:  NONE                                                            RCP0007
C                                                                       RCP0008
C CALLED BY:  PTR                                                       RCP0009
C                                                                       RCP0010
C CALLS:  PGYZ                                                          RCP0011
C                                                                       RCP0012
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            RCP0013
C       RESEARCH TRIANGLE PARK, NC                                      RCP0014
C                                                                       RCP0015
C SDM 1.0         REVISION HISTORY:                                     RCP0016
C     86329       MPTER VERSION                                         RCP0017
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCRCP0018
                                                                        RCP0019
C->->->->SECTION RCP.A - COMMON.                                        RCP0020
      COMMON /MPOR/ IOPT(26)                                            RCP0021
      COMMON /MPR/ UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC,MUOR                RCP0022
C                                                                       RCP0023
C***   MODIFICATIONS:                                                   RCP0024
C        11/27/79 BY K.W.BALDRIDGE, C.S.C., CONVERTED CODE FROM FIELDATARCP0025
C                 TO ASCII FORTRAN AND MADE CODE MORE STANDARD          RCP0026
C                                                                       RCP0027
C->->->->SECTION RCP.B - EXPLANATIONS AND COMPUTATIONS                  RCP0028
C                        COMMON TO ALL CONDITIONS.                      RCP0029
C                                                                       RCP0030
C  RCP DETERMINES RELATIVE CONCENTRATIONS, CHI/Q, FROM POINT SOURCES.   RCP0031
C         IT CALLS UPON  PGYZ TO OBTAIN STANDARD DEVIATIONS.            RCP0032
C        THE INPUT VARIABLES ARE....                                    RCP0033
C         UPL  WIND SPEED (M/SEC)                                       RCP0034
C         Z    RECEPTOR HEIGHT (M)                                      RCP0035
C         H    EFFECTIVE STACK HEIGHT (M)                               RCP0036
C         HL   MIXING HEIGHT- TOP OF NEUTRAL OR UNSTABLE LAYER(M).      RCP0037
C         X    DISTANCE RECEPTOR IS DOWNWIND OF SOURCE (KM)             RCP0038
C         Y    DISTANCE RECEPTOR IS CROSSWIND FROM SOURCE (KM)          RCP0039
C         KST  STABILITY CLASS                                          RCP0040
C         DELH PLUME RISE(METERS)                                       RCP0041
C        THE OUTPUT VARIABLES ARE....                                   RCP0042
C          SY HORIZONTAL DISPERSION PARAMETER                           RCP0043
C          SZ VERTICAL DISPERSION PARAMETER                             RCP0044
C         RC   RELATIVE CONCENTRATION (SEC/M**3) ,CHI/Q                 RCP0045
C        THE FOLLOWING EQUATION IS SOLVED --                            RCP0046
C        RC = (1/(2*PI*UPL*SIGMA Y*SIGMA Z))* (EXP(-0.5*(Y/SIGMA Y)**2))RCP0047
C           (EXP(-0.5*((Z-H)/SIGMA Z)**2) + EXP(-0.5*((Z+H)/SIGMA Z)**2)RCP0048
C             PLUS THE SUM OF THE FOLLOWING 4 TERMS K TIMES (N=1,K) --  RCP0049
C                  FOR NEUTRAL OR UNSTABLE CASES:                       RCP0050
C              TERM 1- EXP(-0.5*((Z-H-2NL)/SIGMA Z)**2)                 RCP0051
C              TERM 2- EXP(-0.5*((Z+H-2NL)/SIGMA Z)**2)                 RCP0052
C              TERM 3- EXP(-0.5*((Z-H+2NL)/SIGMA Z)**2)                 RCP0053
C              TERM 4- EXP(-0.5*((Z+H+2NL)/SIGMA Z)**2)                 RCP0054
C NOTE THAT MIXING HEIGHT- THE TOP OF THE NEUTRAL OR UNSTABLE LAYER-    RCP0055
C HAS A VALUE ONLY FOR STABILITIES 1-4, THAT IS, MIXING HEIGHT,         RCP0056
C THE HEIGHT OF THE NEUTRAL OR UNSTABLE LAYER, DOES NOT EXIST FOR STABLERCP0057
C LAYERS AT THE GROUND SURFACE- STABILITY 5 OR 6.                       RCP0058
C        THE ABOVE EQUATION IS SIMILAR TO EQUATION (5.8) P 36 IN        RCP0059
C         WORKBOOK OF ATMOSPHERIC DISPERSION ESTIMATES WITH THE ADDITIONRCP0060
C         OF THE EXPONENTIAL INVOLVING Y.                               RCP0061
C       IF STABLE, SKIP CONSIDERATION OF MIXING HEIGHT.                 RCP0062
      IF (KST.GE.5) GO TO 50                                            RCP0063
      IF (Z-HL) 50,50,40                                                RCP0064
40    RC=0.                                                             RCP0065
      RETURN                                                            RCP0066
C        IF X IS LESS THAN 1 METER, SET RC=0. AND RETURN.  THIS AVOIDS  RCP0067
C         PROBLEMS OF INCORRECT VALUES NEAR THE SOURCE.                 RCP0068
50    IF (X.LT.0.001) GO TO 40                                          RCP0069
C        CALL PGYZ TO OBTAIN VALUES FOR SY AND SZ                       RCP0070
      CALL PGYZ                                                         RCP0071
C         SY = SIGMA Y, THE STANDARD DEVIATION OF CONCENTRATION IN THE  RCP0072
C         Y-DIRECTION (M)                                               RCP0073
C         SZ = SIGMA Z, THE STANDARD DEVIATION OF CONCENTRATION IN THE  RCP0074
C         Z-DIRECTION (M)                                               RCP0075
C       IF IOPT(4)=1, CONSIDER BUOYANCY INDUCED DISPERSION OF PLUME DUE RCP0076
C         TO TURBULENCE DURING BUOYANT RISE.                            RCP0077
      IF (IOPT(4).EQ.0) GO TO 70                                        RCP0078
      DUM=DELH/3.5                                                      RCP0079
      DUM=DUM*DUM                                                       RCP0080
      SY=SQRT(SY*SY+DUM)                                                RCP0081
      SZ=SQRT(SZ*SZ+DUM)                                                RCP0082
70    C1=1.                                                             RCP0083
      IF (Y.EQ.0.0) GO TO 100                                           RCP0084
      YD=1000.*Y                                                        RCP0085
C        YD IS CROSSWIND DISTANCE IN METERS.                            RCP0086
      DUM=YD/SY                                                         RCP0087
      TEMP=0.5*DUM*DUM                                                  RCP0088
      IF (TEMP.GE.50.) GO TO 40                                         RCP0089
      C1=EXP(TEMP)                                                      RCP0090
100   IF (KST.GT.4) GO TO 120                                           RCP0091
      IF (HL.LT.5000.) GO TO 200                                        RCP0092
C        IF STABLE CONDITION OR UNLIMITED MIXING HEIGHT,                RCP0093
C         USE EQUATION 3.2 IF Z = 0, OR EQ 3.1 FOR NON-ZERO Z.          RCP0094
C         (EQUATION NUMBERS REFER TO WORKBOOK OF ATMOSPHERIC DISPERSION RCP0095
C         ESTIMATES.)                                                   RCP0096
120   C2=2.*SZ*SZ                                                       RCP0097
      IF (Z) 40,130,150                                                 RCP0098
C       NOTE: AN ERRONEOUS NEGATIVE Z WILL RESULT IN ZERO CONCENTRATIONSRCP0099
C                                                                       RCP0100
C->->->->SECTION RCP.C - STABLE OR UNLIMITED MIXING, Z IS ZERO.         RCP0101
C                                                                       RCP0102
130   C3=H*H/C2                                                         RCP0103
      IF (C3.GE.50.) GO TO 40                                           RCP0104
      A2=1./EXP(C3)                                                     RCP0105
C        WADE EQUATION 3.2.                                             RCP0106
      RC=A2/(3.14159*UPL*SY*SZ*C1)                                      RCP0107
      RETURN                                                            RCP0108
C                                                                       RCP0109
C->->->->SECTION RCP.D - STABLE OR UNLIMITED MIXING, Z IS NON-ZERO.     RCP0110
C                                                                       RCP0111
150   A2=0.                                                             RCP0112
      A3=0.                                                             RCP0113
      CA=Z-H                                                            RCP0114
      CB=Z+H                                                            RCP0115
      C3=CA*CA/C2                                                       RCP0116
      C4=CB*CB/C2                                                       RCP0117
      IF (C3.GE.50.) GO TO 170                                          RCP0118
      A2=1./EXP(C3)                                                     RCP0119
170   IF (C4.GE.50.) GO TO 190                                          RCP0120
      A3=1./EXP(C4)                                                     RCP0121
C        WADE EQUATION 3.1.                                             RCP0122
190   RC=(A2+A3)/(6.28318*UPL*SY*SZ*C1)                                 RCP0123
      RETURN                                                            RCP0124
C                                                                       RCP0125
C->->->->SECTION RCP.E - UNSTABLE, ASSURED OF UNIFORM MIXING.           RCP0126
C                                                                       RCP0127
C        IF SIGMA-Z IS GREATER THAN 1.6 TIMES THE MIXING HEIGHT,        RCP0128
C         THE DISTRIBUTION BELOW THE MIXING HEIGHT IS UNIFORM WITH      RCP0129
C         HEIGHT REGARDLESS OF SOURCE HEIGHT OR RECEPTOR HEIGHT BECAUSE RCP0130
C         OF REPEATED EDDY REFLECTIONS FROM THE GROUND AND THE MIXING HTRCP0131
200   IF (SZ/HL.LE.1.6) GO TO 220                                       RCP0132
C        WADE EQUATION 3.5.                                             RCP0133
      RC=1./(2.5066*UPL*SY*HL*C1)                                       RCP0134
      RETURN                                                            RCP0135
C        INITIAL VALUE OF AN SET = 0.                                   RCP0136
C         AN - THE NUMBER OF TIMES THE SUMMATION TERM IS EVALUATED      RCP0137
C               AND ADDED IN.                                           RCP0138
220   AN=0.                                                             RCP0139
      IF (Z) 40,380,230                                                 RCP0140
C                                                                       RCP0141
C->->->->SECTION RCP.F - UNSTABLE, CALCULATE MULTIPLE EDDY              RCP0142
C                        REFLECTIONS, Z IS NON-ZERO.                    RCP0143
C                                                                       RCP0144
C       STATEMENTS 220-260 CALCULATE RC, THE RELATIVE CONCENTRATION,    RCP0145
C         USING THE EQUATION DISCUSSED ABOVE.  SEVERAL INTERMEDIATE     RCP0146
C         VARIABLES ARE USED TO AVOID REPEATING CALCULATIONS.           RCP0147
C         CHECKS ARE MADE TO BE SURE THAT THE ARGUMENT OF THE           RCP0148
C         EXPONENTIAL FUNCTION IS NEVER GREATER THAN 50 (OR LESS THAN   RCP0149
C         -50).                                                         RCP0150
C        CALCULATE MULTIPLE EDDY REFLECTIONS FOR RECEPTOR HEIGHT Z.     RCP0151
230   A1=1./(6.28318*UPL*SY*SZ*C1)                                      RCP0152
      C2=2.*SZ*SZ                                                       RCP0153
      A2=0.                                                             RCP0154
      A3=0.                                                             RCP0155
      CA=Z-H                                                            RCP0156
      CB=Z+H                                                            RCP0157
      C3=CA*CA/C2                                                       RCP0158
      C4=CB*CB/C2                                                       RCP0159
      IF (C3.GE.50.) GO TO 250                                          RCP0160
      A2=1./EXP(C3)                                                     RCP0161
250   IF (C4.GE.50.) GO TO 270                                          RCP0162
      A3=1./EXP(C4)                                                     RCP0163
270   SUM=0.                                                            RCP0164
      THL=2.*HL                                                         RCP0165
280   AN=AN+1.                                                          RCP0166
      A4=0.                                                             RCP0167
      A5=0.                                                             RCP0168
      A6=0.                                                             RCP0169
      A7=0.                                                             RCP0170
      C5=AN*THL                                                         RCP0171
      CC=CA-C5                                                          RCP0172
      CD=CB-C5                                                          RCP0173
      CE=CA+C5                                                          RCP0174
      CF=CB+C5                                                          RCP0175
      C6=CC*CC/C2                                                       RCP0176
      C7=CD*CD/C2                                                       RCP0177
      C8=CE*CE/C2                                                       RCP0178
      C9=CF*CF/C2                                                       RCP0179
      IF (C6.GE.50.) GO TO 300                                          RCP0180
      A4=1./EXP(C6)                                                     RCP0181
300   IF (C7.GE.50.) GO TO 320                                          RCP0182
      A5=1./EXP(C7)                                                     RCP0183
320   IF (C8.GE.50.) GO TO 340                                          RCP0184
      A6=1./EXP(C8)                                                     RCP0185
340   IF (C9.GE.50.) GO TO 360                                          RCP0186
      A7=1./EXP(C9)                                                     RCP0187
360   T=A4+A5+A6+A7                                                     RCP0188
      SUM=SUM+T                                                         RCP0189
      IF (T.GE.0.01) GO TO 280                                          RCP0190
      RC=A1*(A2+A3+SUM)                                                 RCP0191
      RETURN                                                            RCP0192
C                                                                       RCP0193
C->->->->SECTION RCP.G - UNSTABLE, CALCULATE MULTIPLE EDDY              RCP0194
C                        REFLECTIONS, Z IS ZERO.                        RCP0195
C                                                                       RCP0196
C        CALCULATE MULTIPLE EDDY REFLECTIONS FOR GROUND LEVEL RECEPTOR  RCP0197
C        HEIGHT.                                                        RCP0198
380   A1=1./(6.28318*UPL*SY*SZ*C1)                                      RCP0199
      A2=0.                                                             RCP0200
      C2=2.*SZ*SZ                                                       RCP0201
      C3=H*H/C2                                                         RCP0202
      IF (C3.GE.50.) GO TO 400                                          RCP0203
      A2=2./EXP(C3)                                                     RCP0204
400   SUM=0.                                                            RCP0205
      THL=2.*HL                                                         RCP0206
410   AN=AN+1.                                                          RCP0207
      A4=0.                                                             RCP0208
      A6=0.                                                             RCP0209
      C5=AN*THL                                                         RCP0210
      CC=H-C5                                                           RCP0211
      CE=H+C5                                                           RCP0212
      C6=CC*CC/C2                                                       RCP0213
      C8=CE*CE/C2                                                       RCP0214
      IF (C6.GE.50.) GO TO 430                                          RCP0215
      A4=2./EXP(C6)                                                     RCP0216
430   IF (C8.GE.50.) GO TO 450                                          RCP0217
      A6=2./EXP(C8)                                                     RCP0218
450   T=A4+A6                                                           RCP0219
      SUM=SUM+T                                                         RCP0220
      IF (T.GE.0.01) GO TO 410                                          RCP0221
      RC=A1*(A2+SUM)                                                    RCP0222
      RETURN                                                            RCP0223
C                                                                       RCP0224
C->->->->SECTION RCP.H - FORMAT                                         RCP0225
C                                                                       RCP0226
C***   SECTIONS OF SUBROUTINE RCP.                                      RCP0227
C         SECTION RCP.A -  COMMON.                                      RCP0228
C         SECTION RCP.B -  EXPLANATIONS AND COMPUTATIONS COMMON TO ALL  RCP0229
C                           CONDITIONS.                                 RCP0230
C         SECTION RCP.C -  STABLE OR UNLIMITED MIXING, Z IS ZERO.       RCP0231
C         SECTION RCP.D -  STABLE OR UNLIMITED MIXING, Z IS NON-ZERO.   RCP0232
C         SECTION RCP.E -  UNSTABLE, ASSURED OF UNIFORM MIXING.         RCP0233
C         SECTION RCP.F -  UNSTABLE, CALCULATE MULTIPLE EDDY            RCP0234
C                           REFLECTIONS; Z IS NON-ZERO.                 RCP0235
C         SECTION RCP.G -  UNSTABLE, CALCULATE MULTIPLE EDDY            RCP0236
C                           REFLECTIONS; Z IS ZERO.                     RCP0237
C         SECTION RCP.H -  FORMAT.                                      RCP0238
C                                                                       RCP0239
      END                                                               RCP0240
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSFM0001
      SUBROUTINE SFM(DIST)                                              SFM0002
C                                                                       SFM0003
C PURPOSE: SFM (SHORELINE FUMIGATION MODEL) IS AN ALGORITHM CODE        SFM0004
C          WHICH PRODUCES ESTIMATES                                     SFM0005
C          OF GROUND LEVEL POLLUTANT CONCENTRATION FOR USER-DEFINED     SFM0006
C          RECEPTORS LOCATED DOWNWIND OF AN ELEVATED, SINGLE POINT      SFM0007
C          SOURCE SITUATED AT THE SHORELINE.                            SFM0008
C                                                                       SFM0009
C I/O:  DIST, DISTANCE FROM SOURCE TO SHORELINE                         SFM0010
C                                                                       SFM0011
C CALLED BY:  INTERF                                                    SFM0012
C                                                                       SFM0013
C CALLS:  CALC                                                          SFM0014
C                                                                       SFM0015
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            SFM0016
C       RESEARCH TRIANGLE PARK, NC                                      SFM0017
C                                                                       SFM0018
C SDM 1.0         REVISION HISTORY:                                     SFM0019
C                                                                       SFM0020
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSFM0021
C                                                                       SFM0022
C   ORIGINAL AUTHOR:  DR. P.K. MISRA                                    SFM0023
C                     AIR RESOURCES BRANCH                              SFM0024
C                     ONTARIO MINISTRY OF ENVIRONMENT                   SFM0025
C                     CANADA                                            SFM0026
C                                                                       SFM0027
C   MODIFIED FOR INCORPORATION IN MPTER BY:                             SFM0028
C            SUZANNE TEMPLEMAN                                          SFM0029
C            MARINE, EARTH AND ATMOSPHERIC SCIENCES DEPT.               SFM0030
C            NORTH CAROLINA STATE UNIVERSITY                            SFM0031
C            RALEIGH, NC  27695-8208                                    SFM0032
C                                                                       SFM0033
C            JUNE 1988                                                  SFM0034
C                                                                       SFM0035
C   EXECUTION OF THE CODE IS APPROPRIATE PROVIDED THE                   SFM0036
C   FOLLOWING CONDITIONS ARE SATISFIED:                                 SFM0037
C                                                                       SFM0038
C   1) WIND DIRECTION AT THE SHORELINE SOURCE IS ONSHORE                SFM0039
C   2) IT IS DAYTIME AND THE SURFACE, SENSIBLE HEAT                     SFM0040
C      FLUX OVER LAND IS AT LEAST +5 W M[-2                             SFM0041
C   3) LAPSE RATE OVER WATER IS STABLE                                  SFM0042
C                                                                       SFM0043
C   THE USER IS REFERRED TO THE USER'S GUIDE FOR A MORE                 SFM0044
C   DETAILED EXPLANATION OF WHEN THE MODEL SHOULD BE APPLIED.           SFM0045
C                                                                       SFM0046
C          THE PROGRAM CONSISTS OF FOUR MODULES:                        SFM0047
C                1. MAIN MODULE                                         SFM0048
C                2. SUBROUTINE CALC                                     SFM0049
C                3. SUBROUTINE SIMP                                     SFM0050
C                4. FUNCTION EVAL                                       SFM0051
C                                                                       SFM0052
C**************************************************************         SFM0053
C                                                                       SFM0054
C                                                                       SFM0055
C  DEFINE VARIABLES:                                                    SFM0056
C                                                                       SFM0057
C   A     =  TIBL A FACTOR, GIVEN BY:                                   SFM0058
C            ((2*HO)/(RHO*CSUBP*DTHDZ*UL))**0.5  (M[1/2)                SFM0059
C   B     =  W*/UL                                                      SFM0060
C   RHO   =  ATMOSPHERIC DENSITY  (KG M[-3)                             SFM0061
C   CSUBP =  SPECIFIC HEAT AT CONSTANT PRESSURE (J K[-1 KG[-1)          SFM0062
C   CK    =  VON KARMAN'S CONSTANT  (0.4)                               SFM0063
C   CN    =  BRUNT-VAISALA FREQUENCY (S[-1)                             SFM0064
C   DTHDZ =  POTENTIAL TEMPERATURE GRADIENT OVERWATER (K M[-1)          SFM0065
C   F     =  PLUME BUOYANCY (M4 S[-3)                                   SFM0066
C   G     =  ACCELERATION DUE TO GRAVITY AT THE SURFACE (M S[-2)        SFM0067
C   HO    =  SURFACE SENSIBLE HEAT FLUX OVER LAND (W M[-2)              SFM0068
C   HSTK  =  STACK HEIGHT (M)                                           SFM0069
C   NR    =  NUMBER OF RECEPTORS                                        SFM0070
C   OWZ2,1  = HEIGHT OVER WATER AT LEVELS 2, 1 (M)                      SFM0071
C   PTOW2,1 = POTENTIAL TEMPERATURE OVER WATER AT                       SFM0072
C             LEVELS 2, 1 (K)                                           SFM0073
C   PTMOL =  MEAN POTENTIAL TEMPERATURE OVER LAND                       SFM0074
C            BETWEEN LEVELS 2, 1 (K)                                    SFM0075
C   PTMOW =  MEAN POTENTIAL TEMPERATURE OVER WATER                      SFM0076
C            BETWEEN LEVELS 2, 1 (K)                                    SFM0077
C   Q     =  SOURCE STRENGTH (G S[-1)                                   SFM0078
C   UL    =  MEAN WIND SPEED IN THE TIBL (M S[-1)                       SFM0079
C   US    =  MEAN WIND SPEED IN THE STABLE LAYER (M S[-1)               SFM0080
C   W*    =  CONVECTIVE VELOCITY (M S[-1)                               SFM0081
C   X     =  DOWNWIND DISTANCE (M)                                      SFM0082
C***********************************************************            SFM0083
      REAL XP,YP,MGCM                                                   SFM0084
      REAL A,B,CN,H,HSTK,Q,UL,US                                        SFM0085
      COMMON /SDMONE/XP,YP,A,B,UL,US,HSTK,CN,F,Q,MGCM                   SFM0086
                                                                        SFM0087
      COMMON /MPR/UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC,MUOR                 SFM0088
                                                                        SFM0089
C                                                                       SFM0090
C->->  CALL TO CALC SUBROUTINE TO BEGIN CALCULATION                     SFM0091
C      OF GROUND LEVEL CONCENTRATIONS                                   SFM0092
C                                                                       SFM0093
      CALL CALC(DIST)                                                   SFM0094
C                                                                       SFM0095
      RETURN                                                            SFM0096
      END                                                               SFM0097
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSMP0001
      SUBROUTINE SIMP(EVAL,EA,EB,ACC,ANS,ERROR,AREA,IFLAG)              SMP0002
C                                                                       SMP0003
C PURPOSE: SIMP IS AN ITERATIVE CODE BASED ON SIMPSON'S RULE,           SMP0004
C          A NUMERICAL TECHNIQUE DESIGNED TO EVALUATE THE               SMP0005
C          DEFINITE INTEGRAL OF A CONTINUOUS FUNCTION WITH              SMP0006
C          FINITE LIMITS OF INTEGRATION.                                SMP0007
C                                                                       SMP0008
C I/O:  EVAL(X), VALUE OF THE INTEGRAL EVALUATED AT X                   SMP0009
C        EA,EB, LOWER AND UPPER LIMITS OF INTEGRATION                   SMP0010
C          ACC, DESIRED ACCURACY OF ANSWER                              SMP0011
C          ANS, APPROXIMATE VALUE OF THE INTEGRAL OF F(X)               SMP0012
C               FROM EA TO EB                                           SMP0013
C        ERROR, ESTIMATED ERROR OF ANSWER                               SMP0014
C         AREA, APPROXIMATE, ABSOLUTE VALUE OF THE INTEGRAL             SMP0015
C               F(X) FROM EA TO EB                                      SMP0016
C        IFLAG, 1 FOR NORMAL RETURN                                     SMP0017
C               2 IF IT IS NECESSARY TO GO TO 30 LEVELS OR              SMP0018
C                 USE LENGTH.  ERROR MAY BE UNRELIABLE                  SMP0019
C                 IN THIS CASE.                                         SMP0020
C               3 IF MORE THAN 2000 FUNCTION EVALUATIONS                SMP0021
C                 THEN COMPLETE THE COMPUTATIONS.  ERROR                SMP0022
C                 IS USUALLY UNRELIABLE.                                SMP0023
C               IFLAG MAY BE USED FOR DIAGNOSTICS.                      SMP0024
C                                                                       SMP0025
C CALLED BY:  CALC                                                      SMP0026
C                                                                       SMP0027
C CALLS:  EVAL                                                          SMP0028
C                                                                       SMP0029
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            SMP0030
C       RESEARCH TRIANGLE PARK, NC                                      SMP0031
C                                                                       SMP0032
C SDM 1.0         REVISION HISTORY:                                     SMP0033
C                                                                       SMP0034
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSMP0035
      DIMENSION FV(5),LORR(30),FIT(30),F2T(30),F3T(30),                 SMP0036
     #DAT(30),ARESTT(30),ESTT(30),EPST(30),PSUM(30)                     SMP0037
      EXTERNAL EVAL                                                     SMP0038
                                                                        SMP0039
      REAL MGCM                                                         SMP0040
      COMMON /SDMONE/XP,YP,A,B,UL,US,HSTK,CN,F,Q,MGCM                   SMP0041
                                                                        SMP0042
      COMMON /MPR/UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC,MUOR                 SMP0043
                                                                        SMP0044
C                                                                       SMP0045
C      U = UNIT ROUND-OFF                                               SMP0046
C->->  SET U TO APPROXIMATELY THE UNIT ROUND-OFF                        SMP0047
C                                                                       SMP0048
      U = 9.0E-7                                                        SMP0049
C                                                                       SMP0050
C->->  INITIALIZE VARIABLES                                             SMP0051
C                                                                       SMP0052
      FOURU = 4.0*U                                                     SMP0053
      IFLAG = 1                                                         SMP0054
      EPS = ACC                                                         SMP0055
      ERROR=0.0                                                         SMP0056
      AREA = 0.0                                                        SMP0057
      AREST = 0.0                                                       SMP0058
      LVL = 1                                                           SMP0059
      LORR(LVL) = 1                                                     SMP0060
      PSUM(LVL) = 0.0                                                   SMP0061
      ALPHA = EA                                                        SMP0062
      DA = EB-EA                                                        SMP0063
C                                                                       SMP0064
C->->   DETERMINE VALUES OF THE FUNCTION AT THE ENDS                    SMP0065
C       AND MID-POINT OF THE INTERVAL                                   SMP0066
C                                                                       SMP0067
      FV(1) = EVAL(ALPHA)                                               SMP0068
      FV(3) = EVAL(ALPHA+0.5*DA)                                        SMP0069
      FV(5) = EVAL(ALPHA+DA)                                            SMP0070
C                                                                       SMP0071
C->->   START SUMMATION OF NUMBER OF FUNCTION EVALUATIONS               SMP0072
C                                                                       SMP0073
      KOUNT = 3                                                         SMP0074
      WT = DA/6.0                                                       SMP0075
C                                                                       SMP0076
C->->   DETERMINE ESTIMATE OF THE INTEGRAL FOR THE INTERVAL             SMP0077
C       BETWEEN THE DESIGNATED ENDPOINTS                                SMP0078
C                                                                       SMP0079
      EST = WT*(FV(1)+4.0*FV(3)+FV(5))                                  SMP0080
10    DX = 0.5*DA                                                       SMP0081
C                                                                       SMP0082
C->->   DETERMINE VALUES OF THE FUNCTION AT THE ONE QUARTER             SMP0083
C       AND THREE QUARTER POINTS OF THE INTERVAL                        SMP0084
C                                                                       SMP0085
      FV(2) = EVAL(ALPHA+0.5*DX)                                        SMP0086
      FV(4) = EVAL(ALPHA+1.5*DX)                                        SMP0087
      KOUNT = KOUNT+2                                                   SMP0088
      WT = DX/6.0                                                       SMP0089
C                                                                       SMP0090
C->->   DETERMINE ESTIMATES OF THE AREA UNDER THE LEFT HALF             SMP0091
C       AND RIGHT HALF OF THE CURVE THEN SUM                            SMP0092
C                                                                       SMP0093
      ESTL = WT*(FV(1)+4.0*FV(2)+FV(3))                                 SMP0094
      ESTR = WT*(FV(3)+4.0*FV(4)+FV(5))                                 SMP0095
      SUM = ESTL+ESTR                                                   SMP0096
C                                                                       SMP0097
C->->   DETERMINE ESTIMATES OF THE AREA UNDER THE CURVE                 SMP0098
C       BETWEEN THE DESIGNATED ENDPOINTS BASED ON THE                   SMP0099
C       ABSOLUTE VALUES OF THE FUNCTION EVALUATIONS                     SMP0100
C                                                                       SMP0101
      ARESTL = WT*(ABS(FV(1))+ABS(4.0*FV(2))+ABS(FV(3)))                SMP0102
      ARESTR = WT*(ABS(FV(3))+ABS(4.0*FV(4))+ABS(FV(5)))                SMP0103
      AREA = AREA+((ARESTL+ARESTR)-AREST)                               SMP0104
      DIFF = EST-SUM                                                    SMP0105
C                                                                       SMP0106
C->->  IF ERROR IS ACCEPTABLE GO TO 20.  IF INTERVAL IS TOO             SMP0107
C      SMALL OR TOO MANY LEVELS OR TOO MANY FUNCTION                    SMP0108
C      EVALUATIONS, SET A FLAG AND GO TO 20 ANYWAY.                     SMP0109
C                                                                       SMP0110
      IF(ABS(DIFF).LE.EPS*ABS(AREA)) GOTO 20                            SMP0111
      IF(ABS(DX).LE.FOURU*ABS(ALPHA)) GOTO 50                           SMP0112
      IF(LVL.GE.30) GOTO 50                                             SMP0113
      IF(KOUNT.GE.2000) GOTO 60                                         SMP0114
C                                                                       SMP0115
C->->  STORE INFORMATION TO PROCESS RIGHT HALF OF THE                   SMP0116
C      CURVE.  NOW, USING A GREATER NUMBER OF SUB-INTERVALS,            SMP0117
C      RECALCULATE AREA UNDER THE LEFT HALF OF THE CURVE.               SMP0118
C                                                                       SMP0119
      LVL = LVL+1                                                       SMP0120
      LORR(LVL) = 0                                                     SMP0121
      FIT(LVL) = FV(3)                                                  SMP0122
      F2T(LVL) = FV(4)                                                  SMP0123
      F3T(LVL) = FV(5)                                                  SMP0124
      DA = DX                                                           SMP0125
      DAT(LVL) = DX                                                     SMP0126
      AREST = ARESTL                                                    SMP0127
      ARESTT(LVL) = ARESTR                                              SMP0128
      EST = ESTL                                                        SMP0129
      ESTT(LVL) = ESTR                                                  SMP0130
      EPS = EPS/1.4                                                     SMP0131
      EPST(LVL) = EPS                                                   SMP0132
      FV(5) = FV(3)                                                     SMP0133
      FV(3) = FV(2)                                                     SMP0134
      GOTO 10                                                           SMP0135
C                                                                       SMP0136
C->->  ACCEPT APPROXIMATE INTEGRAL SUM.  IF LEFT HALF                   SMP0137
C      OF CURVE WAS PROCESSED, MOVE TO RIGHT HALF.                      SMP0138
C      IF RIGHT HALF OF CURVE WAS PROCESSED, ADD RESULTS                SMP0139
C      TO FINISH.  LORR (A MNEMONIC FOR LEFT OR RIGHT)                  SMP0140
C      TELLS WHETHER INTERVAL IS RIGHT OR LEFT AT EACH                  SMP0141
C      LEVEL.                                                           SMP0142
C                                                                       SMP0143
20    ERROR = ERROR+DIFF/15.0                                           SMP0144
30    IF(LORR(LVL).EQ. 0) GOTO 40                                       SMP0145
      SUM = PSUM(LVL)+SUM                                               SMP0146
      LVL = LVL-1                                                       SMP0147
      IF(LVL.GT.1) GOTO 30                                              SMP0148
      ANS = SUM                                                         SMP0149
      RETURN                                                            SMP0150
C                                                                       SMP0151
C->->  MOVE RIGHT.  RESTORE SAVED INFORMATION TO PROCESS                SMP0152
C      RIGHT HALF OF INTERVAL.                                          SMP0153
C                                                                       SMP0154
40    PSUM(LVL) = SUM                                                   SMP0155
      LORR(LVL) = 1                                                     SMP0156
      ALPHA = ALPHA+DA                                                  SMP0157
      DA = DAT(LVL)                                                     SMP0158
      FV(1) = FIT(LVL)                                                  SMP0159
      FV(3) = F2T(LVL)                                                  SMP0160
      FV(5) = F3T(LVL)                                                  SMP0161
      AREST = ARESTT(LVL)                                               SMP0162
      EST = ESTT(LVL)                                                   SMP0163
      EPS = EPST(LVL)                                                   SMP0164
      GOTO 10                                                           SMP0165
C                                                                       SMP0166
C->->  ACCEPT 'POOR' VALUE.  SET APPROPRIATE FLAGS.                     SMP0167
C                                                                       SMP0168
50    IFLAG = 2                                                         SMP0169
      GOTO 20                                                           SMP0170
60    IFLAG = 3                                                         SMP0171
      GOTO 20                                                           SMP0172
      END                                                               SMP0173
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCXSH0001
      FUNCTION XSHORE(X0,Y0,TRAD,X1,Y1,BA,EA,FETCH)                     XSH0002
C                                                                       XSH0003
C PURPOSE:  CALCULATES DISTANCE FORM SOURCE TO SHORELINE.               XSH0004
C                                                                       XSH0005
C I/O:  X0, EAST COORDINATE OF POINT SOURCE                             XSH0006
C       Y0, NORTH COORDINATE OF POINE SOURCE                            XSH0007
C     TRAD, WIND DIRECTION                                              XSH0008
C       X1, EAST COORDINATE OF SHORELINE                                XSH0009
C       Y1, NORTH COORDINATE OF SHORELINE                               XSH0010
C       BA, BEGINNING ANGLE OF SHORE                                    XSH0011
C       EA, ENDING ANGLE OF SHORE                                       XSH0012
C    FETCH, DEGREE OF ACCEPTABLE WIND FETCH FOR ONSHORE DETERMINATION   XSH0013
C                                                                       XSH0014
C CALLED BY:  INTERF                                                    XSH0015
C                                                                       XSH0016
C CALLS:  NONE                                                          XSH0017
C                                                                       XSH0018
C       U.S. ENVIRONMENTAL PROTECTION AGENCY                            XSH0019
C       RESEARCH TRIANGLE PARK, NC                                      XSH0020
C                                                                       XSH0021
C SDM 1.0         REVISION HISTORY:                                     XSH0022
C                                                                       XSH0023
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCXSH0024
                                                                        XSH0025
cccc  LAHEY Compiler does not support COTAN ..... 8/27/90               XSH0026
c                                                                       XSH0027
      COTAN(X) = 1.0/TAN(X)                                             XSH0028
c                                                                       XSH0029
      RADCON=0.017453293                                                XSH0030
      IF ((X1-X0).NE.0.) THEN                                           XSH0031
         TSTANG=ATAN((Y1-Y0)/(X1-X0))/RADCON                            XSH0032
        ELSE                                                            XSH0033
         IF (Y1.GT.Y0) TSTANG=90                                        XSH0034
         IF (Y1.LT.Y0) TSTANG=270                                       XSH0035
         IF (Y1.EQ.Y0) THEN                                             XSH0036
            XSHORE=0.                                                   XSH0037
            RETURN                                                      XSH0038
         ENDIF                                                          XSH0039
      ENDIF                                                             XSH0040
      IF (X1.LT.X0) TSTANG=TSTANG+180                                   XSH0041
      IF (TSTANG.LE.0) TSTANG=TSTANG+360                                XSH0042
      TSTANG=360-TSTANG+90                                              XSH0043
      IF (TSTANG.LT.BA+FETCH) TSTANG=TSTANG+360                         XSH0044
      IF (TRAD.LT.BA) TRAD=TRAD+360                                     XSH0045
      IF (TRAD.GT.EA) TRAD=TRAD-360                                     XSH0046
      IF (TSTANG.GT.EA-FETCH) THEN                                      XSH0047
         IF (TSTANG.GT.EA+180-FETCH) THEN                               XSH0048
            ANG=BA                                                      XSH0049
           ELSE                                                         XSH0050
            ANG=EA                                                      XSH0051
         ENDIF                                                          XSH0052
         GO TO 100                                                      XSH0053
      ENDIF                                                             XSH0054
      IF (TSTANG.GT.TRAD) ANG=BA                                        XSH0055
      IF (TSTANG.LT.TRAD) ANG=EA                                        XSH0056
      IF (TSTANG.EQ.TRAD) THEN                                          XSH0057
         XSHORE=SQRT((X1-X0)**2+(Y1-Y0)**2)                             XSH0058
         RETURN                                                         XSH0059
      ENDIF                                                             XSH0060
  100 CONTINUE                                                          XSH0061
      ANG=360-ANG+90                                                    XSH0062
      WD=360-TRAD+90                                                    XSH0063
                                                                        XSH0064
      IF (WD/180.EQ.INT(WD/180.)) THEN                                  XSH0065
         V=Y0                                                           XSH0066
         IF (ANG/90..EQ.INT(ANG/90.).AND.ANG/180..NE.INT(ANG/180.)) THENXSH0067
            U=X1                                                        XSH0068
            GO TO 10                                                    XSH0069
         ENDIF                                                          XSH0070
         U=X1+(Y0-Y1)*COTAN(ANG*RADCON)                                 XSH0071
         GO TO 10                                                       XSH0072
      ENDIF                                                             XSH0073
                                                                        XSH0074
      IF (WD/90.EQ.INT(WD/90.)) THEN                                    XSH0075
         U=X0                                                           XSH0076
         IF (ANG/180..EQ.INT(ANG/180.)) THEN                            XSH0077
            V=Y1                                                        XSH0078
            GO TO 10                                                    XSH0079
         ENDIF                                                          XSH0080
         V=Y1+(X0-X1)*TAN(ANG*RADCON)                                   XSH0081
         GO TO 10                                                       XSH0082
      ENDIF                                                             XSH0083
                                                                        XSH0084
      IF (ANG/180..EQ.INT(ANG/180.)) THEN                               XSH0085
         V=Y1                                                           XSH0086
         U=X0+(Y1-Y0)*COTAN(WD*RADCON)                                  XSH0087
         GO TO 10                                                       XSH0088
      ENDIF                                                             XSH0089
                                                                        XSH0090
      IF (ANG/90..EQ.INT(ANG/90.)) THEN                                 XSH0091
         U=X1                                                           XSH0092
         V=Y0+(X1-X0)*TAN(WD*RADCON)                                    XSH0093
         GO TO 10                                                       XSH0094
      ENDIF                                                             XSH0095
                                                                        XSH0096
      U=(Y1-Y0+X0*TAN(WD*RADCON)-X1*TAN(ANG*RADCON))/                   XSH0097
     &     (TAN(WD*RADCON)-TAN(ANG*RADCON))                             XSH0098
      V=Y1+(U-X1)*TAN(ANG*RADCON)                                       XSH0099
                                                                        XSH0100
   10 XSHORE=SQRT((U-X0)**2+(V-Y0)**2)                                  XSH0101
      RETURN                                                            XSH0102
      END                                                               XSH0103

