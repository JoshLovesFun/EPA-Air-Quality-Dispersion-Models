CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBLK00010
      BLOCK DATA                                                        BLK00020
C                                                                       BLK00030
C  ZZ0000.FOR                                                           BLK00040
C                                                                       BLK00050
C  BLOCK DATA                                                           BLK00060
C                                                                       BLK00070
C  PLUVUE II                                                            BLK00080
C  Version 92243   Level 1.0                                            BLK00090
C                                                                       BLK00100
C  U.S. Environmental Protection Agency                                 BLK00110
C  Office of Air Quality Planning and Standards                         BLK00120
C  Research Triangle Park, North Carolina                               BLK00130
C                                                                       BLK00140
C  Modifications:                                                       BLK00150
C                                                                       BLK00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBLK00170
      SAVE                                                              BLK00180
      COMMON/MIESCT/ROG,SIGMA,NLAMB,LAMB(20),JX,IT,TT(200),DUM(20),     BLK00190
     1PDUM(20,50),DUMA(20),DUMG(20)                                     BLK00200
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     BLK00210
     1,NTHETA,RFR,RFI                                                   BLK00220
      COMMON/COLOR/YCAP,VAL,X,Y,YCAPD,VALD,XD,YD,DELUV,DELAB,           BLK00230
     1XBAR(39),YBAR(39),ZBAR(39),PI,CONT1,CONT2,CONT3,BRATIO            BLK00240
      REAL LAMB                                                         BLK00250
      DATA LAMB / 0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,11*0.0/  BLK00260
      DATA TT /0.,22.,45.,90.,135.,158.,180.,193*0./                    BLK00270
      DATA NLAMB / 9 /                                                  BLK00280
      DATA NTHETA / 7 /                                                 BLK00290
      DATA ABSNO2 /1.58, 1.63,1.67,1.71,1.67, 1.63, 1.54, 1.45,         BLK00300
     1             1.36, 1.17, 1.06, 0.92, 0.80, 0.69, 0.61, 0.54,      BLK00310
     2             0.46, 0.39, 0.31, 0.26, 0.21, 0.18, 0.15, 0.12, 0.1, BLK00320
     3             0.080, 0.063, 0.053, 0.041, 0.035, 0.029, 0.024,     BLK00330
     4             0.021, 0.017, 0.014, 0.010, 0.0068, 0.0034,0./       BLK00340
      DATA  SOLAR / 1163., 1124., 1121., 1423., 1730., 1740., 1659.,    BLK00350
     1 1801., 1998., 2059., 2040., 2042., 1959., 1941., 1879., 1838.,   BLK00360
     2 1839., 1785., 1731., 1704., 1712., 1716., 1699., 1640., 1635.,   BLK00370
     3 1602., 1570., 1544., 1511., 1466., 1456., 1427., 1402., 1369.,   BLK00380
     4 1344., 1314., 1290.,1260., 1235.   /                             BLK00390
      DATA XBAR /  0.0000, 0.0014, 0.0042, 0.0143, 0.0435, 0.1344,      BLK00400
     1 0.2839, 0.3483, 0.3362, 0.2908, 0.1954, 0.0956, 0.0320,          BLK00410
     2 0.0049, 0.0093, 0.0633, 0.1655, 0.2904, 0.4334, 0.5945, 0.7621,  BLK00420
     3 0.9163, 1.0263, 1.0622, 1.0026, 0.8544, 0.6424, 0.4479, 0.2835,  BLK00430
     4 0.1649, 0.0874, 0.0468, 0.0227, 0.0114, 0.0058, 0.0029, 0.0014,  BLK00440
     5 0.0007, 0.0003 /                                                 BLK00450
      DATA YBAR / 0.0000, 0.0000, 0.0001, 0.0004, 0.0012, 0.0040,       BLK00460
     1 0.0116, 0.0230, 0.0380, 0.0600,0.0910, 0.1390, 0.2080, 0.3230,   BLK00470
     2 0.5030, 0.7100, 0.8620, 0.9540, 0.9950, 0.9950, 0.9520, 0.8700,  BLK00480
     3 0.7570,0.6310, 0.5030, 0.3810, 0.2650, 0.1750, 0.1070, 0.0610,   BLK00490
     4 0.0320, 0.0170, 0.0082, 0.0041, 0.0021, 0.0010, 0.0005, 0.0002,  BLK00500
     5 0.0001   /                                                       BLK00510
      DATA ZBAR / 0.0000, 0.0065, 0.0201, 0.0679, 0.2074, 0.6456,       BLK00520
     1 1.3856, 1.7471, 1.7721, 1.6692, 1.2876, 0.8130, 0.4652, 0.2720,  BLK00530
     2 0.1582, 0.0782, 0.0422, 0.0203, 0.0087, 0.0039, 0.0021, 0.0017,  BLK00540
     3 0.0011, 0.0008, 0.0003, 0.0002, 13*0.0000 /                      BLK00550
      END                                                               BLK00560
