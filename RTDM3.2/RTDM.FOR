C***********************************************************************RTD00005
C                                                                       RTD00006
C                         RTDM3.2 (DATED 89226)                         RTD00010
C                                                                       RTD00011
C           *** SEE RTDM3.2 MODEL CHANGE BULLETIN MCB#2 ***             RTD00012
C                                                                       RTD00013
C     ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD    RTD00014
C                                                                       RTD00015
C                             919-541-5742                              RTD00016
C                                                                       RTD00017
C***********************************************************************RTD00018
C                      RTDM - VERSION 3.2 (DATED 87307)                 RTD00020
C
C********************************************************************   RTD00010
C       MAIN PROGRAM FOR RTDM                                           RTD00030
C                                                                       RTD00040
C       ENVIRONMENTAL RESEARCH AND TECHNOLOGY                           RTD00050
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          RTD00060
C                                                                       RTD00070
C       RTDM      VERSION 3.2     LEVEL 861015                          RTD00080
C                                                                       RTD00090
C********************************************************************   RTD00100
C                                                                       RTD00110
        COMMON /HEAD/NPAGE,NLINE                                        RTD00120
C       REAL KEYS(5),NINES                                              IBM00130
        CHARACTER*4 KEYS(5),NINES,ENDJ,WORD                             IBM00001
        DATA KEYS /'PARA','POIN','STAC','TERR','EXEC'/                  RTD00140
        DATA NINES/'9999'/,ENDJ/'ENDJ'/                                 RTD00150
C                                                                       RTD00160
        NPAGE = 0                                                       RTD00170
        NLINE = 0                                                       RTD00180
        IPARAM = 0                                                      RTD00190
        IPOINT = 0                                                      RTD00200
        ISTACK = 0                                                      RTD00210
C
C       PRINT OUT HEADER
C
        WRITE(6,10)
   10   FORMAT('1', 21X, 'RTDM3.2            (DATED 89226)',/)
C
        CALL PAGE                                                       RTD00220
C                                                                       RTD00230
C       CALL BLKFIX    @ INITIALIZE AND SET UP COMMON BLOCKS            IBM00240
   30   READ(5,1000)WORD                                                RTD00250
 1000   FORMAT(A4)                                                      RTD00260
        IF(WORD.EQ.NINES)GO TO 30                                       RTD00270
        IF(WORD.EQ.ENDJ)GO TO 9000                                      RTD00280
        DO 50 I=1,5                                                     RTD00290
        IF(WORD.EQ.KEYS(I))GO TO 70                                     RTD00300
   50   CONTINUE                                                        RTD00310
        WRITE(6,2000) WORD                                              RTD00320
 2000   FORMAT(' KEYWORD NOT AMONG THOSE EXPECTED: ',A4)                RTD00330
        GO TO 9000                                                      RTD00340
   70   K=I                                                             RTD00350
C                                                                       RTD00360
       GO TO (100,200,300,400,500), K                                   RTD00370
C                                                                       RTD00380
C       'PARAMETERS'                                                    RTD00390
C                                                                       RTD00400
  100   CALL INPAR                                                      RTD00410
        CALL WRPAR                                                      RTD00420
        IPARAM = 1                                                      RTD00430
        GO TO 30                                                        RTD00440
C                                                                       RTD00450
C       'POINTS'                                                        RTD00460
C                                                                       RTD00470
  200   IF(IPARAM.EQ.1.AND.ISTACK.EQ.1) GO TO 250                       RTD00480
        WRITE(6,1)                                                      RTD00490
    1   FORMAT(//,'  POINTS KEYWORD MUST FOLLOW PARAMETERS AND STACKS.')RTD00500
        GO TO 9000                                                      RTD00510
  250   CALL POINTS                                                     RTD00520
        IPOINT = 1                                                      RTD00530
        GO TO 30                                                        RTD00540
C                                                                       RTD00550
C       'STACKS'                                                        RTD00560
C                                                                       RTD00570
  300   IF(IPARAM.EQ.1) GO TO 350                                       RTD00580
        WRITE(6,2)                                                      RTD00590
    2   FORMAT(//,'  STACKS KEYWORD MUST FOLLOW PARAMETERS.')           RTD00600
        GO TO 9000                                                      RTD00610
  350   CALL STAKS                                                      RTD00620
        ISTACK = 1                                                      RTD00630
        GO TO 30                                                        RTD00640
C                                                                       RTD00650
C       'TERRAIN'                                                       RTD00660
C                                                                       RTD00670
  400   IF(IPARAM.EQ.1.AND.ISTACK.EQ.1.AND.IPOINT.EQ.1) GO TO 450       RTD00680
        WRITE(6,3)                                                      RTD00690
    3   FORMAT(//,'  TERRAIN KEYWORD MUST FOLLOW PARAMETERS, STACKS, ', RTD00700
     X  'AND POINTS.')                                                  RTD00710
        GO TO 9000                                                      RTD00720
  450   CALL TERAIN                                                     RTD00730
        GO TO 30                                                        RTD00740
C                                                                       RTD00750
C       'EXECUTE'                                                       RTD00760
C                                                                       RTD00770
  500   IF(IPARAM.EQ.1.AND.ISTACK.EQ.1.AND.IPOINT.EQ.1) GO TO 550       RTD00780
        WRITE(6,4)                                                      RTD00790
    4   FORMAT(//,'  EXECUTE KEYWORD MUST FOLLOW PARAMETERS, STACKS, ', RTD00800
     X  'AND POINTS.')                                                  RTD00810
        GO TO 9000                                                      RTD00820
  550   CALL RTXEQ                                                      RTD00830
        GO TO 9000                                                      RTD00840
C                                                                       RTD00850
C       'ENDJOB'                                                        RTD00860
C                                                                       RTD00870
 9000   CALL PAGE                                                       RTD00880
        STOP                                                            RTD00890
        END                                                             RTD00900

        BLOCK DATA BLKFIX                                               IBM00002
C       SUBROUTINE BLKFIX                                               IBM00010
C                                                                       BLK00020
C********************************************************************   BLK00030
C                                                                       BLK00040
C       BLOCK DATA HOLDING DEFAULTS FOR RTDM COMMON BLOCKS              BLK00050
C                                                                       BLK00060
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          BLK00070
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          BLK00080
C                                                                       BLK00090
C       RTDM      VERSION 3.2     LEVEL 861015                          BLK00100
C                                                                       BLK00110
C********************************************************************   BLK00120
C                                                                       BLK00130
C                                                                       BLK00140
C                                                                       BLK00150
C                                                                       BLK00160
C-------------  PARAMETERS   ------------------------------------------ BLK00170
C                                                                       BLK00180
        COMMON /PR001/ HSCALE                                           BLK00190
C                                                                       BLK00200
C--------------------------------                                       BLK00210
C                                                                       BLK00220
        COMMON /PR002/ VSCALE                                           BLK00230
C                                                                       BLK00240
C--------------------------------                                       BLK00250
C                                                                       BLK00260
        COMMON /PR003/ USCALE                                           BLK00270
C                                                                       BLK00280
C--------------------------------                                       BLK00290
C                                                                       BLK00300
        COMMON /PR004/ ZWIND1,ZWIND2,IDILUT,ZA                          BLK00310
C                                                                       BLK00320
C--------------------------------                                       BLK00330
C                                                                       BLK00340
        COMMON /PR005/ EXPON(6)                                         BLK00350
C                                                                       BLK00360
C--------------------------------                                       BLK00370
C                                                                       BLK00380
        COMMON /PR006/ ICOEF                                            BLK00390
C                                                                       BLK00400
C--------------------------------                                       BLK00410
C                                                                       BLK00420
        DIMENSION AY(6,3),BY(6,3),CY(6,3)                               BLK00430
        COMMON /PR007/ AY, BY, CY, XY1,XY2                              BLK00440
C                                                                       BLK00450
C--------------------------------                                       BLK00460
C                                                                       BLK00470
        DIMENSION AZ(6,3),BZ(6,3),CZ(6,3)                               BLK00480
        COMMON /PR008/ AZ, BZ, CZ, XZ1,XZ2                              BLK00490
C                                                                       BLK00500
C--------------------------------                                       BLK00510
C                                                                       BLK00520
        COMMON /PR009/ IPPP,PPVPTG                                      BLK00530
C                                                                       BLK00540
C--------------------------------                                       BLK00550
C                                                                       BLK00560
        COMMON /PR010/ IBUOY,ALPHA                                      BLK00570
C                                                                       BLK00580
C--------------------------------                                       BLK00590
C                                                                       BLK00600
        COMMON /PR011/ IDMX                                             BLK00610
C                                                                       BLK00620
C--------------------------------                                       BLK00630
C                                                                       BLK00640
        COMMON /PR012/ ITRANS                                           BLK00650
C                                                                       BLK00660
C--------------------------------                                       BLK00670
C                                                                       BLK00680
        COMMON /PR013/ TERCOR(6)                                        BLK00690
C                                                                       BLK00700
C--------------------------------                                       BLK00710
C                                                                       BLK00720
        COMMON /PR014/ RVPTG(2)                                         BLK00730
C                                                                       BLK00740
C--------------------------------                                       BLK00750
C                                                                       BLK00760
        COMMON /PR015/ ITIPD                                            BLK00770
C                                                                       BLK00780
C--------------------------------                                       BLK00790
C                                                                       BLK00800
        COMMON /PR016/ IY                                               BLK00810
C                                                                       BLK00820
C--------------------------------                                       BLK00830
C                                                                       BLK00840
        COMMON /PR017/ IZ                                               BLK00850
C                                                                       BLK00860
C--------------------------------                                       BLK00870
C                                                                       BLK00880
        COMMON /PR018/ IRVPTG                                           BLK00890
C                                                                       BLK00900
C--------------------------------                                       BLK00910
C                                                                       BLK00920
        COMMON /PR019/ IHVPTG                                           BLK00930
C                                                                       BLK00940
C--------------------------------                                       BLK00950
C                                                                       BLK00960
        COMMON /PR020/ ISHEAR,SHCOEF                                    BLK00970
C                                                                       BLK00980
C--------------------------------                                       BLK00990
C                                                                       BLK01000
        COMMON /PR021/ IEPS                                             BLK01010
C                                                                       BLK01020
C--------------------------------                                       BLK01030
C                                                                       BLK01040
        COMMON /PR022/ IREFL                                            BLK01050
C                                                                       BLK01060
C--------------------------------                                       BLK01070
C                                                                       BLK01080
        COMMON /PR023/ IHORIZ,SECTOR(6)                                 BLK01090
C                                                                       BLK01100
C--------------------------------                                       BLK01110
C                                                                       BLK01120
        COMMON /PR024/ IEMIS                                            BLK01130
C                                                                       BLK01140
C--------------------------------                                       BLK01150
C                                                                       BLK01160
        COMMON /PR025/ IVERB                                            BLK01170
C                                                                       BLK01180
C--------------------------------                                       BLK01190
C                                                                       BLK01200
C                                                                       BLK01210
C------------------------------------------------------------------     BLK01220
C               DEFAULT VALUES OF PARAMETERS                            BLK01230
C------------------------------------------------------------------     BLK01240
C                                                                       BLK01250
C-------------------------------- /PR001/                               BLK01260
C                                                                       BLK01270
        DATA HSCALE /1000./                                             BLK01280
C                                                                       BLK01290
C-------------------------------- /PR002/                               BLK01300
C                                                                       BLK01310
        DATA VSCALE /.3048/                                             BLK01320
C                                                                       BLK01330
C-------------------------------- /PR003/                               BLK01340
C                                                                       BLK01350
        DATA USCALE /.4471/                                             BLK01360
C                                                                       BLK01370
C-------------------------------- /PR004/                               BLK01380
C                                                                       BLK01390
        DATA ZWIND1 /10./,ZWIND2 /10./,IDILUT /0/,ZA/0.0/               BLK01400
C                                                                       BLK01410
C-------------------------------- /PR005/                               BLK01420
C                                                                       BLK01430
        DATA EXPON /.09,.11,.12,.14,.20,.30/                            BLK01440
C                                                                       BLK01450
C-------------------------------- /PR006/                               BLK01460
C                                                                       BLK01470
        DATA ICOEF /3/                                                  BLK01480
C                                                                       BLK01490
C-------------------------------- /PR007/                               BLK01500
C                                                                       BLK01510
        DATA (AY(I,1),I=1,6)/0.40,0.36,0.34,0.32,0.31,0.31/             BLK01520
        DATA (BY(I,1),I=1,6)/0.91,0.86,0.82,0.78,0.71,0.71/             BLK01530
        DATA (CY(I,1),I=1,6)/0.0,0.0,0.0,0.0,0.0,0.0/                   BLK01540
        DATA (AY(I,2),I=1,6)/0.40,0.36,0.34,0.32,0.31,0.31/             BLK01550
        DATA (BY(I,2),I=1,6)/0.91,0.86,0.82,0.78,0.71,0.71/             BLK01560
        DATA (CY(I,2),I=1,6)/0.0,0.0,0.0,0.0,0.0,0.0/                   BLK01570
        DATA (AY(I,3),I=1,6)/0.40,0.36,0.34,0.32,0.31,0.31/             BLK01580
        DATA (BY(I,3),I=1,6)/0.91,0.86,0.82,0.78,0.71,0.71/             BLK01590
        DATA (CY(I,3),I=1,6)/0.0,0.0,0.0,0.0,0.0,0.0/                   BLK01600
        DATA XY1/1.E2/,XY2/1.E3/                                        BLK01610
C                                                                       BLK01620
C-------------------------------- /PR008/                               BLK01630
C                                                                       BLK01640
        DATA (AZ(I,1),I=1,6)/0.40,0.33,0.28,0.22,0.06,0.06/             BLK01650
        DATA (BZ(I,1),I=1,6)/0.91,0.86,0.82,0.78,0.71,0.71/             BLK01660
        DATA (CZ(I,1),I=1,6)/0.0,0.0,0.0,0.0,0.0,0.0/                   BLK01670
        DATA (AZ(I,2),I=1,6)/0.40,0.33,0.28,0.22,0.06,0.06/             BLK01680
        DATA (BZ(I,2),I=1,6)/0.91,0.86,0.82,0.78,0.71,0.71/             BLK01690
        DATA (CZ(I,2),I=1,6)/0.0,0.0,0.0,0.0,0.0,0.0/                   BLK01700
        DATA (AZ(I,3),I=1,6)/0.40,0.33,0.28,0.22,0.06,0.06/             BLK01710
        DATA (BZ(I,3),I=1,6)/0.91,0.86,0.82,0.78,0.71,0.71/             BLK01720
        DATA (CZ(I,3),I=1,6)/0.0,0.0,0.0,0.0,0.0,0.0/                   BLK01730
        DATA XZ1/1.E2/,XZ2/1.E3/                                        BLK01740
C                                                                       BLK01750
C-------------------------------- /PR009/                               BLK01760
C                                                                       BLK01770
        DATA IPPP /0/, PPVPTG /0.006/                                   BLK01780
C                                                                       BLK01790
C-------------------------------- /PR010/                               BLK01800
C                                                                       BLK01810
         DATA IBUOY /1/, ALPHA /3.16228/                                BLK01820
C                                                                       BLK01830
C-------------------------------- /PR011/                               BLK01840
C                                                                       BLK01850
         DATA IDMX /1/                                                  BLK01860
C                                                                       BLK01870
C-------------------------------- /PR012/                               BLK01880
C                                                                       BLK01890
         DATA ITRANS /1/                                                BLK01900
C                                                                       BLK01910
C-------------------------------- /PR013/                               BLK01920
C                                                                       BLK01930
        DATA TERCOR /0.5,0.5,0.5,0.5,0.5,0.5/                           BLK01940
C                                                                       BLK01950
C-------------------------------- /PR014/                               BLK01960
C                                                                       BLK01970
        DATA RVPTG /0.020,0.035/                                        BLK01980
C                                                                       BLK01990
C-------------------------------- /PR015/                               BLK02000
C                                                                       BLK02010
        DATA ITIPD /0/                                                  BLK02020
C                                                                       BLK02030
C-------------------------------- /PR016/                               BLK02040
C                                                                       BLK02050
        DATA IY /0/                                                     BLK02060
C                                                                       BLK02070
C-------------------------------- /PR017/                               BLK02080
C                                                                       BLK02090
        DATA IZ /0/                                                     BLK02100
C                                                                       BLK02110
C-------------------------------- /PR018/                               BLK02120
C                                                                       BLK02130
        DATA IRVPTG /0/                                                 BLK02140
C                                                                       BLK02150
C-------------------------------- /PR019/                               BLK02160
C                                                                       BLK02170
        DATA IHVPTG /0/                                                 BLK02180
C                                                                       BLK02190
C-------------------------------- /PR020/                               BLK02200
C                                                                       BLK02210
        DATA ISHEAR /0/, SHCOEF /0.17/                                  BLK02220
C                                                                       BLK02230
C-------------------------------- /PR021/                               BLK02240
C                                                                       BLK02250
        DATA IEPS /0/                                                   BLK02260
C                                                                       BLK02270
C-------------------------------- /PR022/                               BLK02280
C                                                                       BLK02290
        DATA IREFL /1/                                                  BLK02300
C                                                                       BLK02310
C-------------------------------- /PR023/                               BLK02320
C                                                                       BLK02330
        DATA IHORIZ /2/, SECTOR /6*22.5/                                BLK02340
C                                                                       BLK02350
C-------------------------------- /PR024/                               BLK02360
C                                                                       BLK02370
        DATA IEMIS /0/                                                  BLK02380
C                                                                       BLK02390
C-------------------------------- /PR025/                               BLK02400
C                                                                       BLK02410
        DATA IVERB /0/                                                  BLK02420
C                                                                       BLK02430
C--------------------------------                                       BLK02440
C                                                                       BLK02450
C       RETURN                                                          IBM02460
        END                                                             BLK02470

        SUBROUTINE INPAR                                                INP00010
C                                                                       INP00020
C********************************************************************   INP00030
C                                                                       INP00040
C       THIS SUBROUTINE READS THE RUN PARAMETERS                        INP00050
C                                                                       INP00060
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          INP00070
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          INP00080
C                                                                       INP00090
C       RTDM      VERSION 3.2     LEVEL 861015                          INP00100
C                                                                       INP00110
C********************************************************************   INP00120
C                                                                       INP00130
C******************************************************                 INP00140
C                                                                       INP00150
C      COMMON BLOCK FOR MODEL PARAMETERS AND SWITCHES                   INP00160
C                                                                       INP00170
C******************************************************                 INP00180
C                                                                       INP00190
        DIMENSION AY(6,3),BY(6,3),CY(6,3),                              INP00200
     X          AZ(6,3),BZ(6,3),CZ(6,3)                                 INP00210
        COMMON /PR001/ HSCALE                                           INP00220
        COMMON /PR002/ VSCALE                                           INP00230
        COMMON /PR003/ USCALE                                           INP00240
        COMMON /PR004/ ZWIND1,ZWIND2,IDILUT,ZA                          INP00250
        COMMON /PR005/ EXPON(6)                                         INP00260
        COMMON /PR006/ ICOEF                                            INP00270
        COMMON /PR007/ AY, BY, CY, XY1,XY2                              INP00280
        COMMON /PR008/ AZ, BZ, CZ, XZ1,XZ2                              INP00290
        COMMON /PR009/ IPPP,PPVPTG                                      INP00300
        COMMON /PR010/ IBUOY,ALPHA                                      INP00310
        COMMON /PR011/ IDMX                                             INP00320
        COMMON /PR012/ ITRANS                                           INP00330
        COMMON /PR013/ TERCOR(6)                                        INP00340
        COMMON /PR014/ RVPTG(2)                                         INP00350
        COMMON /PR015/ ITIPD                                            INP00360
        COMMON /PR016/ IY                                               INP00370
        COMMON /PR017/ IZ                                               INP00380
        COMMON /PR018/ IRVPTG                                           INP00390
        COMMON /PR019/ IHVPTG                                           INP00400
        COMMON /PR020/ ISHEAR,SHCOEF                                    INP00410
        COMMON /PR021/ IEPS                                             INP00420
        COMMON /PR022/ IREFL                                            INP00430
        COMMON /PR023/ IHORIZ,SECTOR(6)                                 INP00440
        COMMON /PR024/ IEMIS                                            INP00450
        COMMON /PR025/ IVERB                                            INP00460
C                                                                       INP00470
        REAL FIELD(6)                                                   INP00480
        CHARACTER*2 INAM,INAMX,I99,IPR                                  IBM00003
        DATA I99/'99'/,IPR/'PR'/,INAM/'PR'/                             INP00490
        INUM1 = 0                                                       INP00500
        INUM2 = 0                                                       INP00510
C                                                                       INP00520
   10   INAMX = INAM                                                    INP00530
        INUM1X = INUM1                                                  INP00540
        INUM2X = INUM2                                                  INP00550
        READ (5,6000,ERR=9500) INAM,INUM1,INUM2,FIELD                   INP00560
 6000   FORMAT (A2,I1,I2,3X,6E8.0)                                      INP00570
        IF (INAM .EQ. I99) GO TO 9000                                   INP00580
C                                                                       INP00590
        IF (INAM.NE.IPR) GO TO 50                                       INP00600
C                                                                       INP00610
        I = INUM2 + INUM1*100                                           INP00620
        IF(I.LT.1.OR.I.GT.25) GO TO 50                                  INP00630
C                                                                       INP00640
C                                                                       INP00650
        GO TO (100,200,300,400,500,600,700,800,900,1000,                INP00660
     X          1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,      INP00670
     X          2100,2200,2300,2400,2500),I                             INP00680
C                                                                       INP00690
   50   WRITE(6,51) INAM,INUM1,INUM2,INAMX,INUM1X,INUM2X                INP00700
   51   FORMAT(' ILLEGAL PARAMETER NAME "',A2,I1,I2,'"',/,              INP00710
     X  ' LAST PARAMETER NAME SUCCESSFULLY PROCESSED WAS: ',A2,I1,I2,/) INP00720
        WRITE (6,52)                                                    INP00730
   52   FORMAT(//,' *FATAL ERROR*',//,' EXECUTION TERMINATING DUE TO ', INP00740
     X  'ERROR IN READING PARAMETER NAME')                              INP00750
C                                                                       INP00760
        STOP                                                            INP00770
C                                                                       INP00780
C       /PR001/                                                         INP00790
C                                                                       INP00800
  100   HSCALE = FIELD(1)                                               INP00810
        GO TO 10                                                        INP00820
C                                                                       INP00830
C       /PR002/                                                         INP00840
C                                                                       INP00850
  200   VSCALE = FIELD(1)                                               INP00860
        GO TO 10                                                        INP00870
C                                                                       INP00880
C       /PR003/                                                         INP00890
C                                                                       INP00900
  300   USCALE = FIELD(1)                                               INP00910
        GO TO 10                                                        INP00920
C                                                                       INP00930
C       /PR004/                                                         INP00940
C                                                                       INP00950
  400   ZWIND1 = FIELD(1)                                               INP00960
        ZWIND2 = FIELD(2)                                               INP00970
        IDILUT = INT(FIELD(3))                                          INP00980
        ZA = FIELD(4)                                                   INP00990
        GO TO 10                                                        INP01000
C                                                                       INP01010
C       /PR005/                                                         INP01020
C                                                                       INP01030
  500   DO 550 J = 1,6                                                  INP01040
  550   EXPON(J) = FIELD(J)                                             INP01050
        GO TO 10                                                        INP01060
C                                                                       INP01070
C       /PR006/                                                         INP01080
C                                                                       INP01090
  600   ICOEF = INT(FIELD(1))                                           INP01100
        IF(ICOEF.LT.1.OR.ICOEF.GT.3) GO TO 9500                         INP01110
        GO TO 10                                                        INP01120
C                                                                       INP01130
C       /PR007/                                                         INP01140
C                                                                       INP01150
  700   XY1 = FIELD(1)                                                  INP01160
        XY2 = FIELD(2)                                                  INP01170
        DO 750 I=1,3                                                    INP01180
  750   READ (5,6100,ERR=9500) (AY(IS,I),IS=1,6)                        INP01190
 6100   FORMAT(8X,6E8.0)                                                INP01200
        DO 760 I=1,3                                                    INP01210
  760   READ (5,6100,ERR=9500) (BY(IS,I),IS=1,6)                        INP01220
        DO 770 I=1,3                                                    INP01230
  770   READ (5,6100,ERR=9500) (CY(IS,I),IS=1,6)                        INP01240
        GO TO 10                                                        INP01250
C                                                                       INP01260
C       /PR008/                                                         INP01270
C                                                                       INP01280
  800   XZ1 = FIELD(1)                                                  INP01290
        XZ2 = FIELD(2)                                                  INP01300
        DO 850 I=1,3                                                    INP01310
  850   READ (5,6100,ERR=9500) (AZ(IS,I),IS=1,6)                        INP01320
        DO 860 I=1,3                                                    INP01330
  860   READ (5,6100,ERR=9500) (BZ(IS,I),IS=1,6)                        INP01340
        DO 870 I=1,3                                                    INP01350
  870   READ (5,6100,ERR=9500) (CZ(IS,I),IS=1,6)                        INP01360
        GO TO 10                                                        INP01370
C                                                                       INP01380
C       /PR009/                                                         INP01390
C                                                                       INP01400
  900   IPPP = INT(FIELD(1))                                            INP01410
        IF(IPPP.NE.0.AND.IPPP.NE.1) GO TO 9500                          INP01420
        PPVPTG = FIELD(2)                                               INP01430
        GO TO 10                                                        INP01440
C                                                                       INP01450
C       /PR010/                                                         INP01460
C                                                                       INP01470
 1000   IBUOY = INT(FIELD(1))                                           INP01480
        IF(IBUOY.NE.0.AND.IBUOY.NE.1) GO TO 9500                        INP01490
        ALPHA = FIELD(2)                                                INP01500
        GO TO 10                                                        INP01510
C                                                                       INP01520
C       /PR011/                                                         INP01530
C                                                                       INP01540
 1100   IDMX = INT(FIELD(1))                                            INP01550
        IF(IDMX.NE.0.AND.IDMX.NE.1) GO TO 9500                          INP01560
        GO TO 10                                                        INP01570
C                                                                       INP01580
C      /PR012/                                                          INP01590
C                                                                       INP01600
 1200   ITRANS = INT(FIELD(1))                                          INP01610
        IF(ITRANS.NE.0.AND.ITRANS.NE.1) GO TO 9500                      INP01620
        GO TO 10                                                        INP01630
C                                                                       INP01640
C       /PR013/                                                         INP01650
C                                                                       INP01660
 1300   DO 1350 J = 1,6                                                 INP01670
 1350   TERCOR(J) = FIELD(J)                                            INP01680
        GO TO 10                                                        INP01690
C                                                                       INP01700
C       /PR014/                                                         INP01710
C                                                                       INP01720
 1400   RVPTG(1) = FIELD(1)                                             INP01730
        RVPTG(2) = FIELD(2)                                             INP01740
        GO TO 10                                                        INP01750
C                                                                       INP01760
C       /PR015/                                                         INP01770
C                                                                       INP01780
 1500   ITIPD = INT(FIELD(1))                                           INP01790
        IF(ITIPD.NE.0.AND.ITIPD.NE.1) GO TO 9500                        INP01800
        GO TO 10                                                        INP01810
C                                                                       INP01820
C       /PR016/                                                         INP01830
C                                                                       INP01840
 1600   IY = INT(FIELD(1))                                              INP01850
        IF(IY.NE.0.AND.IY.NE.1) GO TO 9500                              INP01860
        GO TO 10                                                        INP01870
C                                                                       INP01880
C       /PR017/                                                         INP01890
C                                                                       INP01900
 1700   IZ = INT(FIELD(1))                                              INP01910
        IF(IZ.NE.0.AND.IZ.NE.1) GO TO 9500                              INP01920
        GO TO 10                                                        INP01930
C                                                                       INP01940
C       /PR018/                                                         INP01950
C                                                                       INP01960
 1800   IRVPTG = INT(FIELD(1))                                          INP01970
        IF(IRVPTG.NE.0.AND.IRVPTG.NE.1) GO TO 9500                      INP01980
        GO TO 10                                                        INP01990
C                                                                       INP02000
C       /PR019/                                                         INP02010
C                                                                       INP02020
 1900   IHVPTG = INT(FIELD(1))                                          INP02030
        IF(IHVPTG.NE.0.AND.IHVPTG.NE.1) GO TO 9500                      INP02040
        GO TO 10                                                        INP02050
C                                                                       INP02060
C       /PR020/                                                         INP02070
C                                                                       INP02080
 2000   ISHEAR = INT(FIELD(1))                                          INP02090
        IF(ISHEAR.NE.0.AND.ISHEAR.NE.1) GO TO 9500                      INP02100
        SHCOEF = FIELD(2)                                               INP02110
        GO TO 10                                                        INP02120
C                                                                       INP02130
C       /PR021/                                                         INP02140
C                                                                       INP02150
 2100   IEPS = INT(FIELD(1))                                            INP02160
        IF(IEPS.NE.0.AND.IEPS.NE.1) GO TO 9500                          INP02170
        GO TO 10                                                        INP02180
C                                                                       INP02190
C       /PR022/                                                         INP02200
C                                                                       INP02210
 2200   IREFL = INT(FIELD(1))                                           INP02220
        IF(IREFL.NE.0.AND.IREFL.NE.1) GO TO 9500                        INP02230
        GO TO 10                                                        INP02240
C                                                                       INP02250
C       /PR023/                                                         INP02260
C                                                                       INP02270
 2300   IHORIZ = INT(FIELD(1))                                          INP02280
        IF(IHORIZ.LT.1.OR.IHORIZ.GT.3) GO TO 9500                       INP02290
        READ(5,6100) SECTOR                                             INP02300
        GO TO 10                                                        INP02310
C                                                                       INP02320
C       /PR024/                                                         INP02330
C                                                                       INP02340
 2400   IEMIS = INT(FIELD(1))                                           INP02350
        IF(IEMIS.NE.0.AND.IEMIS.NE.1) GO TO 9500                        INP02360
        GO TO 10                                                        INP02370
C                                                                       INP02380
C       /PR025/                                                         INP02390
C                                                                       INP02400
 2500   IVERB = INT(FIELD(1))                                           INP02410
        IF(IVERB.NE.0.AND.IVERB.NE.1) GO TO 9500                        INP02420
        GO TO 10                                                        INP02430
C                                                                       INP02440
 9000   RETURN                                                          INP02450
C                                                                       INP02460
 9500   WRITE (6,9598) INUM2                                            INP02470
 9598   FORMAT (/,' INPUT UNREADABLE OR OUT OF BOUNDS FOR PARAMETER #', INP02480
     X  I2)                                                             INP02490
        WRITE (6,9599)                                                  INP02500
 9599   FORMAT(//,' *FATAL ERROR*',//,' EXECUTION TERMINATING DUE TO ', INP02510
     X  'ERROR IN READING PARAMETER VALUE(S).')                         INP02520
C                                                                       INP02530
        STOP                                                            INP02540
        END                                                             INP02550

        SUBROUTINE WRPAR                                                WRP00010
C                                                                       WRP00020
C********************************************************************   WRP00030
C                                                                       WRP00040
C       THIS SUBROUTINE PRINTS THE RUN PARAMETERS                       WRP00050
C                                                                       WRP00060
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          WRP00070
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          WRP00080
C                                                                       WRP00090
C       RTDM      VERSION 3.2     LEVEL 861015                          WRP00100
C                                                                       WRP00110
C********************************************************************   WRP00120
C                                                                       WRP00130
C******************************************************                 WRP00140
C                                                                       WRP00150
C      COMMON BLOCK FOR MODEL PARAMETERS AND SWITCHES                   WRP00160
C                                                                       WRP00170
C******************************************************                 WRP00180
C                                                                       WRP00190
        DIMENSION AY(6,3),BY(6,3),CY(6,3),                              WRP00200
     X          AZ(6,3),BZ(6,3),CZ(6,3)                                 WRP00210
        COMMON /PR001/ HSCALE                                           WRP00220
        COMMON /PR002/ VSCALE                                           WRP00230
        COMMON /PR003/ USCALE                                           WRP00240
        COMMON /PR004/ ZWIND1,ZWIND2,IDILUT,ZA                          WRP00250
        COMMON /PR005/ EXPON(6)                                         WRP00260
        COMMON /PR006/ ICOEF                                            WRP00270
        COMMON /PR007/ AY, BY, CY, XY1,XY2                              WRP00280
        COMMON /PR008/ AZ, BZ, CZ, XZ1,XZ2                              WRP00290
        COMMON /PR009/ IPPP,PPVPTG                                      WRP00300
        COMMON /PR010/ IBUOY,ALPHA                                      WRP00310
        COMMON /PR011/ IDMX                                             WRP00320
        COMMON /PR012/ ITRANS                                           WRP00330
        COMMON /PR013/ TERCOR(6)                                        WRP00340
        COMMON /PR014/ RVPTG(2)                                         WRP00350
        COMMON /PR015/ ITIPD                                            WRP00360
        COMMON /PR016/ IY                                               WRP00370
        COMMON /PR017/ IZ                                               WRP00380
        COMMON /PR018/ IRVPTG                                           WRP00390
        COMMON /PR019/ IHVPTG                                           WRP00400
        COMMON /PR020/ ISHEAR,SHCOEF                                    WRP00410
        COMMON /PR021/ IEPS                                             WRP00420
        COMMON /PR022/ IREFL                                            WRP00430
        COMMON /PR023/ IHORIZ, SECTOR(6)                                WRP00440
        COMMON /PR024/ IEMIS                                            WRP00450
        COMMON /PR025/ IVERB                                            WRP00460
C                                                                       WRP00470
        DIMENSION XZ(4)                                                 WRP00480
C                                                                       WRP00490
        XSTARS=1.E20                                                    WRP00500
C                                                                       WRP00510
C                                                                       WRP00520
C       PRINT PARAMETERS                                                WRP00530
C                                                                       WRP00540
         WRITE(6,9000)                                                  WRP00550
 9000    FORMAT(//,10X,'MODEL PARAMETERS:',/)                           WRP00560
        WRITE (6,9001)HSCALE,VSCALE,USCALE,ZWIND1,ZWIND2,IDILUT,ZA,EXPONWRP00570
 9001   FORMAT (/,10X,'PR001:  HORIZONTAL SCALE IS ',F8.3,' METERS PER',WRP00580
     X  ' USER UNIT',//,10X,'PR002:  VERTICAL SCALE IS ',F8.3,' METERS 'WRP00590
     X  ,'PER USER UNIT',//,10X,'PR003:  WIND SPEED SCALE IS ',F8.3,    WRP00600
     X  ' M/SEC PER USER UNIT',//,10X,'PR004:  ANEMOMETER #1 HEIGHT ',  WRP00610
     X  'ABOVE ZA (USED FOR PLUME RISE) IS ',F5.1,' M',/,18X,           WRP00620
     X  'IF AVAILABLE, ANEMOMETER #2 ',                                 WRP00630
     X  'HEIGHT ABOVE ZA (USED FOR PLUME DILUTION) IS ',F5.1,' M',/,    WRP00640
     X  18X,'DILUTION WIND SPEED OPTION IS ',I1,6X,'(IF 0, ONE ',       WRP00650
     X  'WIND SPEED--AT STACK HEIGHT--IS USED FOR PLUME RISE AND ',     WRP00660
     X  'DILUTION',/,18X,'IF 1, WIND SPEED AT LEVEL #1 ',               WRP00670
     X  'IS EXTRAPOLATED TO STACK-TOP HEIGHT FOR PLUME RISE ',          WRP00680
     X  'AND TO PLUME HEIGHT FOR DILUTION',/,18X,'IF 2, WIND SPEED AT ',WRP00690
     X  'LEVEL #1 IS EXTRAPOLATED TO STACK-TOP HEIGHT FOR PLUME',       WRP00700
     X  ' RISE,',/,18X,'AND THE SPEED AT LEVEL #2 IS ',                 WRP00710
     X  'EXTRAPOLATED TO PLUME HEIGHT FOR DILUTION)',                   WRP00720
     X  /,18X,'ZA (HEIGHT IN METERS ABOVE STACK BASE ELEVATION WHERE ', WRP00730
     X  'THE WIND SPEED PROFILE IS ASSUMED TO ORIGINATE) = ',F8.3,      WRP00740
     X  //,10X,'PR005:  DEFAULT WIND SPEED PROFILE EXPONENTS ',         WRP00750
     X  'AS A FUNCTION OF STABILITY CLASS (1-6, RESPECTIVELY)',         WRP00760
     X   ':',/,30X,6F8.4)                                               WRP00770
C                                                                       WRP00780
        IF(ICOEF.EQ.1) WRITE(6,9002)                                    WRP00790
 9002   FORMAT(/,10X,'PR006, PR007, PR008:  DISPERSION COEFFICIENTS ',  WRP00800
     X  'ARE OF THE FORM A * X**B  + C, SPECIFIED BY THE USER AS ',     WRP00810
     X  'LISTED BELOW:',/)                                              WRP00820
        IF(ICOEF.EQ.2) WRITE(6,9003)                                    WRP00830
 9003   FORMAT(/,10X,'PR006:  DISPERSION COEFFICIENTS ARE PASQUILL-',   WRP00840
     X  'GIFFORD (UNLESS REPLACED BY ON-SITE TURBULENCE DATA).')        WRP00850
C                                                                       WRP00860
        IF(ICOEF.EQ.3) WRITE(6,9004)                                    WRP00870
 9004   FORMAT(/,10X,'PR006:  DISPERSION COEFFICIENTS ARE BRIGGS ',     WRP00880
     X  'RURAL/ASME-1979 (UNLESS REPLACED BY ON-SITE TURBULENCE DATA).')WRP00890
C                                                                       WRP00900
        IF(ICOEF.GT.1) GO TO 9100                                       WRP00910
        XZ(1)=0.                                                        WRP00920
        XZ(2)=XY1                                                       WRP00930
        XZ(3)=XY2                                                       WRP00940
        XZ(4)=XSTARS                                                    WRP00950
C                                                                       WRP00960
        WRITE (6,9008)                                                  WRP00970
 9008   FORMAT (10X,'SIGMA-Y:')                                         WRP00980
C                                                                       WRP00990
        DO 9080 IRANGE=1,3                                              WRP01000
 9080   WRITE (6,9009) XZ(IRANGE),XZ(IRANGE+1),                         WRP01010
     X  (AY(I,IRANGE),I=1,6),(BY(I,IRANGE),I=1,6),(CY(I,IRANGE),I=1,6)  WRP01020
 9009   FORMAT(21X,'RANGE OF X FROM ',F6.0,' TO ',F6.0,' METERS:',5X,   WRP01030
     X  'STABILITY CLASS',/,45X,'1',9X,'2',9X,'3',9X,'4',9X,'5',9X,'6', WRP01040
     X  /,21X,'COEFFICIENT A',3X,6F10.3,/,21X,'COEFFICIENT B',3X,       WRP01050
     X  6F10.3,/,21X,'COEFFICIENT C',3X,6F10.3,/)                       WRP01060
C                                                                       WRP01070
        XZ(2)=XZ1                                                       WRP01080
        XZ(3)=XZ2                                                       WRP01090
C                                                                       WRP01100
        WRITE (6,9010)                                                  WRP01110
 9010   FORMAT (10X,'SIGMA-Z:')                                         WRP01120
C                                                                       WRP01130
        DO 9011 IRANGE=1,3                                              WRP01140
 9011   WRITE (6,9009) XZ(IRANGE),XZ(IRANGE+1),                         WRP01150
     X  (AZ(I,IRANGE),I=1,6),(BZ(I,IRANGE),I=1,6),(CZ(I,IRANGE),I=1,6)  WRP01160
C                                                                       WRP01170
         CALL PAGE                                                      WRP01180
C                                                                       WRP01190
 9100   IF(IPPP.EQ.0) WRITE(6,9013)                                     WRP01200
 9013   FORMAT(/,10X,'PR009:  PARTIAL PLUME PENETRATION OF MIXING LIDS',WRP01210
     X  ' IS NOT BEING USED.')                                          WRP01220
        IF(IPPP.EQ.1) WRITE(6,9014) PPVPTG                              WRP01230
 9014   FORMAT(/,10X,'PR009:  PARTIAL PLUME PENETRATION OF MIXING LIDS',WRP01240
     X  ' IS USED; VPTG ABOVE LID IS ',F7.4,' DEG K/M')                 WRP01250
        IF(IBUOY.EQ.0) WRITE(6,9016)                                    WRP01260
 9016   FORMAT(/,10X,'PR010:  BUOYANCY-ENHANCED PLUME DISPERSION IS ',  WRP01270
     X  'NOT BEING USED.')                                              WRP01280
        IF(IBUOY.EQ.1) WRITE(6,9017) ALPHA                              WRP01290
 9017   FORMAT(/,10X,'PR010:  BUOYANCY-ENHANCED PLUME DISPERSION IS ',  WRP01300
     X  'USED; PARAMETER ALPHA IS ',F8.3)                               WRP01310
        IF(IDMX.EQ.0) WRITE(6,9018)                                     WRP01320
 9018   FORMAT(/,10X,'PR011:  ACTUAL OBSERVED MIXING HEIGHTS USED FOR ',WRP01330
     X  'STABLE CONDITIONS')                                            WRP01340
        IF(IDMX.EQ.1) WRITE(6,9019)                                     WRP01350
 9019   FORMAT(/,10X,'PR011:  UNLIMITED MIXING HEIGHT USED FOR STABLE ',WRP01360
     X  'CONDITIONS')                                                   WRP01370
        IF(ITRANS.EQ.0) WRITE(6,9021)                                   WRP01380
 9021   FORMAT(/,10X,'PR012:  FINAL PLUME RISE ONLY IS USED.')          WRP01390
        IF(ITRANS.EQ.1) WRITE(6,9022)                                   WRP01400
 9022   FORMAT(/,10X,'PR012:  TRANSITIONAL PLUME RISE IS USED.')        WRP01410
        WRITE(6,9024) TERCOR                                            WRP01420
 9024   FORMAT(/,10X,'PR013:  PLUME PATH COEFFICIENTS FOR STABILITY ',  WRP01430
     X  'CLASSES 1-6: ',6F6.3)                                          WRP01440
        WRITE(6,9026) RVPTG                                             WRP01450
 9026   FORMAT(/,10X,'PR014:  DEFAULT VERTICAL POTENTIAL TEMPERATURE ', WRP01460
     X  'GRADIENTS USED FOR STABLE PLUME RISE (CLASSES 5 & 6): ',2F7.4) WRP01470
        IF(ITIPD.EQ.0) WRITE(6,9028)                                    WRP01480
 9028   FORMAT(/,10X,'PR015:  STACK-TIP DOWNWASH IS NOT USED.')         WRP01490
        IF(ITIPD.EQ.1) WRITE(6,9029)                                    WRP01500
 9029   FORMAT(/,10X,'PR015:  STACK-TIP DOWNWASH IS USED.')             WRP01510
        IF(IY.EQ.0) WRITE(6,9031)                                       WRP01520
 9031   FORMAT(/,10X,'PR016:  Y-COMPONENT TURBULENCE INTENSITY VALUES ',WRP01530
     X  'ARE NOT PROVIDED; STABILITY CLASS IS USED TO OBTAIN SIGMA-Y.') WRP01540
        IF(IY.EQ.1) WRITE(6,9032)                                       WRP01550
 9032   FORMAT(/,10X,'PR016:  Y-COMPONENT TURBULENCE INTENSITY VALUES ',WRP01560
     X  'ARE PROVIDED AND ARE USED TO OBTAIN SIGMA-Y.')                 WRP01570
        IF(IZ.EQ.0) WRITE(6,9034)                                       WRP01580
 9034   FORMAT(/,10X,'PR017:  Z-COMPONENT TURBULENCE INTENSITY VALUES ',WRP01590
     X  'ARE NOT PROVIDED; STABILITY CLASS IS USED TO OBTAIN SIGMA-Z.') WRP01600
        IF(IZ.EQ.1) WRITE(6,9035)                                       WRP01610
 9035   FORMAT(/,10X,'PR017:  Z-COMPONENT TURBULENCE INTENSITY VALUES ',WRP01620
     X  'ARE PROVIDED AND ARE USED TO OBTAIN SIGMA-Z.')                 WRP01630
        IF(IRVPTG.EQ.0) WRITE(6,9037)                                   WRP01640
 9037   FORMAT(/,10X,'PR018:  HOURLY VERTICAL POTENTIAL TEMPERATURE ',  WRP01650
     X  'GRADIENTS ARE NOT PROVIDED TO DETERMINE STABLE PLUME RISE;',/, WRP01660
     X  30X,'USE DEFAULT VALUES (SEE PR014).')                          WRP01670
        IF(IRVPTG.EQ.1) WRITE(6,9038)                                   WRP01680
 9038   FORMAT(/,10X,'PR018:  HOURLY VERTICAL POTENTIAL TEMPERATURE ',  WRP01690
     X  'GRADIENTS ARE PROVIDED FOR COMPUTING STABLE PLUME RISE.')      WRP01700
        IF(IHVPTG.EQ.0) WRITE(6,9039)                                   WRP01710
 9039   FORMAT(/,10X,'PR019:  HOURLY VERTICAL POTENTIAL TEMPERATURE ',  WRP01720
     X  'GRADIENTS ARE NOT PROVIDED TO DETERMINE HCRIT;',/,             WRP01730
     X  30X,'USE DEFAULT VALUES (SEE PR014).')                          WRP01740
        IF(IHVPTG.EQ.1) WRITE(6,9040)                                   WRP01750
 9040   FORMAT(/,10X,'PR019:  HOURLY VERTICAL POTENTIAL TEMPERATURE ',  WRP01760
     X  'GRADIENTS ARE PROVIDED FOR COMPUTING HCRIT.')                  WRP01770
        IF(ISHEAR.EQ.0) WRITE(6,9042)                                   WRP01780
 9042   FORMAT(/,10X,'PR020:  WIND DIRECTION SHEAR IS NOT USED IN ',    WRP01790
     X  'COMPUTATION OF SIGMA-Y.')                                      WRP01800
        IF(ISHEAR.EQ.1) WRITE(6,9043) SHCOEF                            WRP01810
 9043   FORMAT(/,10X,'PR020:  WIND DIRECTION SHEAR IS USED IN ',        WRP01820
     X  'COMPUTATION OF SIGMA-Y; COEFFICIENT = ',F5.2)                  WRP01830
        IF(IEPS.EQ.0) WRITE(6,9045)                                     WRP01840
 9045   FORMAT(/,10X,'PR021:  HOURLY VALUES OF WIND SPEED PROFILE ',    WRP01850
     X  'EXPONENT ARE NOT PROVIDED; USE DEFAULTS (SEE PR005).')         WRP01860
        IF(IEPS.EQ.1) WRITE(6,9046)                                     WRP01870
 9046   FORMAT(/,10X,'PR021:  HOURLY VALUES OF WIND SPEED PROFILE ',    WRP01880
     X  'EXPONENT ARE PROVIDED.')                                       WRP01890
        IF(IREFL.EQ.0) WRITE(6,9048)                                    WRP01900
 9048   FORMAT(/,10X,'PR022:  PARTIAL REFLECTION ALGORITHM IS NOT ',    WRP01910
     X  'BEING USED; FULL REFLECTION EMPLOYED.')                        WRP01920
        IF(IREFL.EQ.1) WRITE(6,9049)                                    WRP01930
 9049   FORMAT(/,10X,'PR022:  PARTIAL REFLECTION ALGORITHM IS BEING ',  WRP01940
     X  'USED; KEYWORD TERRAIN MUST BE USED TO READ IN TERRAIN.')       WRP01950
        IF(IHORIZ.EQ.1) WRITE(6,9055)                                   WRP01960
 9055   FORMAT(/,10X,'PR023:  OFF-CENTERLINE CONCENTRATIONS ARE ',      WRP01970
     X  'COMPUTED FOR ALL STABILITIES; NO SECTOR AVERAGING IS USED.')   WRP01980
        IF(IHORIZ.EQ.2) WRITE(6,9056) SECTOR                            WRP01990
 9056   FORMAT(/,10X,'PR023:  SECTOR AVERAGING IS USED FOR ALL ',       WRP02000
     X  'STABILITIES',/,15X,'   SECTOR WIDTHS (DEG) FOR STABILITIES ',  WRP02010
     X  '1-6 ARE ',6(F6.2,3X))                                          WRP02020
        IF(IHORIZ.EQ.3) WRITE(6,9057) SECTOR(5),SECTOR(6)               WRP02030
 9057   FORMAT(/,10X,'PR023:  SECTOR AVERAGING IS USED ONLY FOR ',      WRP02040
     X  'STABLE CONDITIONS; SECTOR WIDTHS (DEG) FOR STAB. 5 ',          WRP02050
     X  'AND 6 ARE ',F6.2,3X,F6.2)                                      WRP02060
        IF(IEMIS.EQ.0) WRITE(6,9059)                                    WRP02070
 9059   FORMAT(/,10X,'PR024:  HOURLY EMISSIONS DATA ARE NOT AVAILABLE;',WRP02080
     X  ' CONSTANT VALUES SPECIFIED IN THE STACKS SECTION ARE USED.')   WRP02090
        IF(IEMIS.EQ.1) WRITE(6,9060)                                    WRP02100
 9060   FORMAT(/,10X,'PR024:  HOURLY EMISSIONS DATA ARE AVAILABLE AND ',WRP02110
     X  'WILL REPLACE THE CONSTANT VALUES SPECIFIED IN THE STACKS ',    WRP02120
     X  'SECTION.')                                                     WRP02130
        IF(IVERB.EQ.0) WRITE(6,9062)                                    WRP02140
 9062   FORMAT(/,10X,'PR025:  DETAILED INFORMATION ABOUT EACH CASE ',   WRP02150
     X  'WILL NOT BE PRINTED.')                                         WRP02160
        IF(IVERB.EQ.1) WRITE(6,9063)                                    WRP02170
 9063   FORMAT(/,10X,'PR025:  DETAILED INFORMATION ABOUT EACH CASE ',   WRP02180
     X  'WILL BE PRINTED.')                                             WRP02190
C                                                                       WRP02200
        CALL PAGE                                                       WRP02210
C                                                                       WRP02220
        RETURN                                                          WRP02230
        END                                                             WRP02240

        SUBROUTINE STAKS                                                STK00010
C                                                                       STK00020
C*******************************************************************    STK00030
C                                                                       STK00040
C       THIS SUBROUTINE READS THE STACK PARAMETERS                      STK00050
C                                                                       STK00060
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          STK00070
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          STK00080
C                                                                       STK00090
C       RTDM      VERSION 3.2     LEVEL 861015                          STK00100
C                                                                       STK00110
C********************************************************************   STK00120
C                                                                       STK00130
C******************************************************                 STK00140
C                                                                       STK00150
C      COMMON BLOCK FOR STACK SPECIFIC DATA                             STK00160
C                                                                       STK00170
C******************************************************                 STK00180
C      COMMON /STACKS/                                                  IBM00190
C    X                 SNAME(35),   @STACK NAMES                        IBM00200
C    X                 HEIGHT(35),  @STACK HEIGHT (M)                   IBM00210
C    X                 DIAMTR(35),  @STACK DIAMETER (M)                 IBM00220
C    X                 EXITVL(35),  @STACK EXIT VELOCITY (M/SEC)        IBM00230
C    X                 STEMP(35),   @STACK GAS EXIT TEMP (DEG K)        IBM00240
C    X                 EMISR(35),   @EMISSION RATE  (G/SEC)             IBM00250
C    X                 UTMX,        @X-COORDINATE FOR ALL STACKS        IBM00260
C    X                 UTMY,        @Y-COORDINATE FOR ALL STACKS        IBM00270
C    X                 BASEHT,      @BASE HEIGHT FOR ALL STACKS         IBM00280
C    X                 NS,          @NUMBER OF STACKS                   IBM00290
C    X                 EMSLBL       @LABEL FOR POLLUTANT                IBM00300
       CHARACTER*4 SNAME(35)                                            IBM00004
       COMMON /STACKS/HEIGHT(35),DIAMTR(35),EXITVL(35),STEMP(35),       IBM00005
     X                EMISR(35),UTMX,UTMY,BASEHT,NS,EMSLBL              IBM00006
       COMMON /STACK2/SNAME                                             IBM00007
C                                                                       STK00310
        COMMON /PR001/ HSCALE                                           STK00320
        COMMON /PR002/ VSCALE                                           STK00330
C                                                                       STK00340
        CHARACTER*4 TEST                                                IBM00008
        DATA TEST/'9999'/                                               STK00350
C                                                                       STK00360
        NS = 1                                                          STK00370
C                                                                       STK00380
        READ(5,1,END=5000,ERR=9900) UUTMX,UUTMY,UBASE,EMSLBL            STK00390
    1   FORMAT(3F10.3,A4)                                               STK00400
  100   CONTINUE                                                        STK00410
        READ(5,2,END=5000,ERR=9900) SNAME(NS),HEIGHT(NS),DIAMTR(NS),    STK00420
     X  EXITVL(NS),STEMP(NS),EMISR(NS)                                  STK00430
    2   FORMAT(A4,6X,5F10.3)                                            STK00440
        IF(SNAME(NS) .EQ. TEST) GO TO 5000                              STK00450
        NS = NS + 1                                                     STK00460
        IF (NS.LE.36) GO TO 100                                         STK00470
        WRITE(6,10)                                                     STK00480
   10   FORMAT(//,'   THE NUMBER OF STACKS EXCEEDS THE MAXIMUM OF 35.') STK00490
        WRITE(6,11)                                                     STK00500
   11   FORMAT(//,' *FATAL ERROR*',//,' EXECUTION TERMINATING DUE TO ', STK00510
     X  'ERROR IN NUMBER OF STACKS SPECIFIED.')                         STK00520
        STOP                                                            STK00530
C                                                                       STK00540
 5000   NS = NS - 1                                                     STK00550
C       UTMX = UUTMX * HSCALE      @ CONVERT TO METERS                  IBM00560
        UTMX = UUTMX * HSCALE                                           IBM00009
C       UTMY = UUTMY * HSCALE      @ CONVERT TO METERS                  IBM00570
        UTMY = UUTMY * HSCALE                                           IBM00010
C       BASEHT = UBASE * VSCALE  @ CONVERT TO METERS                    IBM00580
        BASEHT = UBASE * VSCALE                                         IBM00011
        WRITE(6,15)                                                     STK00590
   15   FORMAT(//,10X,'MODELED STACK PARAMETERS:',///)                  STK00600
        WRITE(6,20) UUTMX,UUTMY,UTMX,UTMY,UBASE,BASEHT,EMSLBL           STK00610
   20   FORMAT(10X,'X,Y COORDINATE OF ALL SOURCES (USER UNITS) IS (',   STK00620
     X  F10.2,' , ',F10.2,' )  OR  (',F10.2,' , ',F10.2,') METERS',/,   STK00630
     X  10X,'BASE HEIGHT OF ALL SOURCES ',                              STK00640
     X  '(USER UNITS) IS ',F10.3,' OR ',F10.3,' METERS',/,10X,          STK00650
     X  'POLLUTANT IS ',A4)                                             STK00660
C                                                                       STK00670
C                                                                       STK00680
        WRITE(6,30)                                                     STK00690
   30   FORMAT(///,10X,'CODE NAME   STACK HT (M)   DIAMETER (M)   ',    STK00700
     X  'EXIT VEL (M/SEC)  STACK TEMP (K)  EMISSION RATE (G/SEC)',/)    STK00710
C                                                                       STK00720
        DO 6000 I = 1,NS                                                STK00730
 6000   WRITE(6,40) I,SNAME(I),HEIGHT(I),DIAMTR(I),EXITVL(I),STEMP(I),  STK00740
     X  EMISR(I)                                                        STK00750
   40   FORMAT(8X,I2,3X,A4,5X,F9.3,7X,F7.3,11X,F7.3,10X,F8.3,10X,F10.3) STK00760
C                                                                       STK00770
        RETURN                                                          STK00780
C                                                                       STK00790
 9900  WRITE(6,50)                                                      STK00800
   50  FORMAT(//,' *** READ ERROR IN STACKS ***')                       STK00810
       WRITE(6,60)                                                      STK00820
   60  FORMAT(//,' *FATAL ERROR*',//,' EXECUTION TERMINATING DUE TO ',  STK00830
     X 'FORMAT ERROR IN STACKS SECTION.')                               STK00840
C                                                                       STK00850
       STOP                                                             STK00860
       END                                                              STK00870

        SUBROUTINE POINTS                                               PNT00010
C                                                                       PNT00020
C***********************************************************************PNT00030
C                                                                       PNT00040
C       THIS SUBROUTINE READS THE RECEPTOR LOCATIONS AND HEIGHTS.       PNT00050
C                                                                       PNT00060
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          PNT00070
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          PNT00080
C                                                                       PNT00090
C       RTDM      VERSION 3.2     LEVEL 861015                          PNT00100
C                                                                       PNT00110
C********************************************************************   PNT00120
C                                                                       PNT00130
C******************************************************                 PNT00140
C                                                                       PNT00150
C      COMMON BLOCK FOR STACK SPECIFIC DATA                             PNT00160
C                                                                       PNT00170
C******************************************************                 PNT00180
C      COMMON /STACKS/                                                  IBM00190
C    X                 SNAME(35),   @STACK NAMES                        IBM00200
C    X                 HEIGHT(35),  @STACK HEIGHT (M)                   IBM00210
C    X                 DIAMTR(35),  @STACK DIAMETER (M)                 IBM00220
C    X                 EXITVL(35),  @STACK EXIT VELOCITY (M/SEC)        IBM00230
C    X                 STEMP(35),   @STACK GAS EXIT TEMP (DEG K)        IBM00240
C    X                 EMISR(35),   @EMISSION RATE  (G/SEC)             IBM00250
C    X                 UTMX,        @X-COORDINATE FOR ALL STACKS        IBM00260
C    X                 UTMY,        @Y-COORDINATE FOR ALL STACKS        IBM00270
C    X                 BASEHT,      @BASE HEIGHT FOR ALL STACKS         IBM00280
C    X                 NS,          @NUMBER OF STACKS                   IBM00290
C    X                 EMSLBL       @LABEL FOR POLLUTANT                IBM00300
       CHARACTER*4 SNAME(35)                                            IBM00012
       COMMON /STACKS/HEIGHT(35),DIAMTR(35),EXITVL(35),STEMP(35),       IBM00013
     X                EMISR(35),UTMX,UTMY,BASEHT,NS,EMSLBL              IBM00014
       COMMON /STACK2/SNAME                                             IBM00015
C                                                                       PNT00310
C******************************************************                 PNT00320
C                                                                       PNT00330
C      COMMON BLOCK FOR RECEPTOR SPECIFIC DATA                          PNT00340
C                                                                       PNT00350
C******************************************************                 PNT00360
C                                                                       PNT00370
       COMMON /PLACE/ RX(400),RY(400),RZ(400),NR                        PNT00380
C              RX              @RECEPTOR X-COORDINATES, METERS          PNT00390
C              RY              @RECEPTOR Y-COORDINATES,  METERS         PNT00400
C              RZ              @RECEPTOR TERRAIN HEIGHTS, USER UNITS    PNT00410
C                                                                       PNT00420
        COMMON /PR001/ HSCALE                                           PNT00430
        COMMON /PR002/ VSCALE                                           PNT00440
C                                                                       PNT00450
C       REAL NAME(8)                                                    PNT00460
        CHARACTER*4 TEST,DELIM,IFLAG,NAME(8)                            IBM00016
        DATA TEST/'9999'/                                               PNT00470
C                                                                       PNT00480
        NR = 0                                                          PNT00490
        I1 = 0                                                          PNT00500
        I2 = 0                                                          PNT00510
        CALL PAGE                                                       PNT00520
C                                                                       PNT00530
   10   WRITE (6,601)                                                   PNT00540
  601   FORMAT (//,10X,'RECEPTOR POINT INFORMATION (ALL COORDINATES IN',PNT00550
     X  ' USER UNITS):',///,                                            PNT00560
     X  10X,'RECEPTOR',4X,'X-COORD',4X,'Y-COORD',4X,'HEIGHT',           PNT00570
     X  5X,'NAME',/)                                                    PNT00580
C                                                                       PNT00590
        NL = 10                                                         PNT00600
        CALL LINES(NL,IZ)                                               PNT00610
C                                                                       PNT00620
   30   READ (5,502,END=900,ERR=9000) DELIM, X, Y, Z, NAME              PNT00630
  502   FORMAT (A4,6X,3F10.0,8A4)                                       PNT00640
C                                                                       PNT00650
        IF(DELIM .EQ. TEST) GO TO 900                                   PNT00660
C                                                                       PNT00670
        NR = NR+1                                                       PNT00680
        IFLAG = '    '                                                  PNT00690
        IF (NR .GT. 400) GO TO 550                                      PNT00700
        GO TO 600                                                       PNT00710
  550   WRITE(6,7000)                                                   PNT00720
 7000   FORMAT(//,'   THE NUMBER OF POINTS EXCEEDS THE MAXIMUM OF 400.')PNT00730
        WRITE(6,8000)                                                   PNT00740
 8000   FORMAT(//,' *FATAL ERROR*',//,' EXECUTION TERMINATING DUE TO ', PNT00750
     X  'A USER SPECIFICATION OF MORE TOO MANY RECEPTOR POINTS.')       PNT00760
        STOP                                                            PNT00770
C                                                                       PNT00780
  600   RX(NR) = X * HSCALE                                             PNT00790
        RY(NR) = Y * HSCALE                                             PNT00800
        RZ(NR) = Z * VSCALE                                             PNT00810
        DO 610 I = 1,NS                                                 PNT00820
        IF(RZ(NR).LE.HEIGHT(I)+BASEHT) GO TO 605                        PNT00830
        IFLAG = '**  '                                                  PNT00840
        I2 = 1                                                          PNT00850
        GO TO 650                                                       PNT00860
  605   IF(RZ(NR).GE.BASEHT) GO TO 610                                  PNT00870
        IFLAG = '*   '                                                  PNT00880
        I1 = 1                                                          PNT00890
  610   CONTINUE                                                        PNT00900
C                                                                       PNT00910
  650   WRITE (6,660) NR, X, Y, Z, IFLAG,NAME                           PNT00920
  660   FORMAT (10X,I3,7X,2(F8.2,3X),F8.2,A2,1X,8A4)                    PNT00930
        NL = 1                                                          PNT00940
        CALL LINES(NL,IZ)                                               PNT00950
        IF(IZ.EQ.1) GO TO 10                                            PNT00960
        GO TO 30                                                        PNT00970
C                                                                       PNT00980
  900   IF(I1.EQ.1) WRITE(6,700)                                        PNT00990
  700   FORMAT(/,'  *  THIS RECEPTOR IS LOWER IN ELEVATION THAN THE ',  PNT01000
     X  '(COMMON) STACK BASE.',/,'     CAUTION SHOULD BE USED ',        PNT01010
     X  'IN INTERPRETATION OF CONCENTRATIONS FOR THIS RECEPTOR.')       PNT01020
        IF(I2.EQ.1) WRITE(6,750)                                        PNT01030
  750   FORMAT(/,'  ** THIS RECEPTOR IS HIGHER IN ELEVATION THAN THAT ',PNT01040
     X  'OF THE LOWEST STACK TOP.',/,'     SPECIAL ALGORITHMS ',        PNT01050
     X  'CONCERNING PLUME IMPINGEMENT AND REFLECTION MAY INFLENCE ',    PNT01060
     X  'CONCENTRATIONS MODELED AT THIS RECEPTOR.')                     PNT01070
        RETURN                                                          PNT01080
C                                                                       PNT01090
 9000   WRITE(6,9100)                                                   PNT01100
 9100   FORMAT(//,' **** READ ERROR IN POINTS ****')                    PNT01110
        WRITE(6,9200)                                                   PNT01120
 9200   FORMAT(' * FATAL ERROR *',//,' EXECUTION TERMINATED DUE TO ',   PNT01130
     X  'A FORMAT ERROR IN THE POINTS SECTION.')                        PNT01140
C                                                                       PNT01150
        STOP                                                            PNT01160
        END                                                             PNT01170

        SUBROUTINE TERAIN                                               TER00010
C                                                                       TER00020
C*******************************************************************    TER00030
C                                                                       TER00040
C       THIS SUBROUTINE READS THE TERRAIN INFORMATION                   TER00050
C                                                                       TER00060
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          TER00070
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          TER00080
C                                                                       TER00090
C       RTDM      VERSION 3.2     LEVEL 861015                          TER00100
C                                                                       TER00110
C********************************************************************   TER00120
C                                                                       TER00130
C******************************************************                 TER00140
C                                                                       TER00150
C      COMMON BLOCK FOR TERRAIN DESCRIPTION DATA                        TER00160
C                                                                       TER00170
C******************************************************                 TER00180
C      COMMON /TERR/                                                    IBM00190
C    X                 BEGHT,       @BEGINNING CONTOUR HT, USER UNITS   IBM00200
C    X                 HTINCR,      @CONTOUR INCREMENT, USER UNITS      IBM00210
C    X                 CONTUR(36,21)@DISTANCES FROM SOURCE ALONG EACH   IBM00220
C    X                              @10-DEG RADIAL FOR HEIGHTS STARTING IBM00230
C    X                              @AT BEGHT, IN INCREMENTS OF HTINCR--IBM00240
C    X                              @UP TO 20 DISTANCES (CONTOUR INCRE- IBM00250
C    X                              @MENTS) ALLOWED ALONG EACH RADIAL   IBM00260
       COMMON /TERR/ BEGHT,HTINCR,CONTUR(36,21)                         IBM00017
C                                                                       TER00270
C******************************************************                 TER00280
C                                                                       TER00290
C      COMMON BLOCK FOR RECEPTOR SPECIFIC DATA                          TER00300
C                                                                       TER00310
C******************************************************                 TER00320
C                                                                       TER00330
       COMMON /PLACE/ RX(400),RY(400),RZ(400),NR                        TER00340
C              RX              @RECEPTOR X-COORDINATES, METERS          TER00350
C              RY              @RECEPTOR Y-COORDINATES,  METERS         TER00360
C              RZ              @RECEPTOR TERRAIN HEIGHTS, USER UNITS    TER00370
C                                                                       TER00380
C******************************************************                 TER00390
C                                                                       TER00400
C      COMMON BLOCK FOR STACK SPECIFIC DATA                             TER00410
C                                                                       TER00420
C******************************************************                 TER00430
C      COMMON /STACKS/                                                  IBM00440
C    X                 SNAME(35),   @STACK NAMES                        IBM00450
C    X                 HEIGHT(35),  @STACK HEIGHT (M)                   IBM00460
C    X                 DIAMTR(35),  @STACK DIAMETER (M)                 IBM00470
C    X                 EXITVL(35),  @STACK EXIT VELOCITY (M/SEC)        IBM00480
C    X                 STEMP(35),   @STACK GAS EXIT TEMP (DEG K)        IBM00490
C    X                 EMISR(35),   @EMISSION RATE  (G/SEC)             IBM00500
C    X                 UTMX,        @X-COORDINATE FOR ALL STACKS        IBM00510
C    X                 UTMY,        @Y-COORDINATE FOR ALL STACKS        IBM00520
C    X                 BASEHT,      @BASE HEIGHT FOR ALL STACKS         IBM00530
C    X                 NS,          @NUMBER OF STACKS                   IBM00540
C    X                 EMSLBL       @LABEL FOR POLLUTANT                IBM00550
       CHARACTER*4 SNAME(35)                                            IBM00018
       COMMON /STACKS/ HEIGHT(35),DIAMTR(35),EXITVL(35),STEMP(35),      IBM00019
     X                 EMISR(35),UTMX,UTMY,BASEHT,NS,EMSLBL             IBM00020
       COMMON /STACK2/ SNAME                                            IBM00021
C                                                                       TER00560
        COMMON /PR001/ HSCALE                                           TER00570
        COMMON /HEAD/ NPAGE,NLINE                                       TER00580
C                                                                       TER00590
C       REAL MAP(55,55),CODES(20),CONTHT(20)                            IBM00600
        REAL CONTHT(20)                                                 IBM00022
        CHARACTER*1 MAP(55,55),CODES(20),BLANK,PLUS,STAR                IBM00023
C       DATA CONTUR/756*0./, BEGHT/0./, HTINCR/0./                      IBM00610
        DATA CODES/'A','B','C','D','E','F','G','H','I','J','K','L','M', TER00620
     X  'N','O','P','Q','R','S','T'/                                    TER00630
        DATA BLANK/' '/,PLUS/'+'/,STAR/'*'/                             TER00640
C                                                                       TER00650
        DO 50 I=1,36                                                    IBM00024
           DO 52 J=1,21                                                 IBM00025
              CONTUR(I,J)=0.                                            IBM00026
   52      CONTINUE                                                     IBM00027
   50   CONTINUE                                                        IBM00028
        BEGHT=0.                                                        IBM00029
        HTINCR=0.                                                       IBM00030
C                                                                       IBM00031
        CALL PAGE                                                       TER00660
C                                                                       TER00670
        READ(5,1,ERR=9000) BEGHT, HTINCR                                TER00680
    1   FORMAT(2F10.3)                                                  TER00690
        IF(BEGHT.GT.0.0.AND.HTINCR.GT.0.0) GO TO 500                    TER00700
        WRITE(6,2) BEGHT,HTINCR                                         TER00710
    2   FORMAT(/,'  BEGINNING HEIGHT (',F10.3,') AND/OR CONTOUR ',      TER00720
     X  'INCREMENT (',F10.3,') IS OUT OF BOUNDS.')                      TER00730
  400   WRITE(6,3)                                                      TER00740
    3   FORMAT(//,' *FATAL ERROR*',//,' EXECUTION TERMINATING DUE TO ', TER00750
     X 'UNUSABLE SPECIFICATION IN TERRAIN SECTION.')                    TER00760
        STOP                                                            TER00770
C                                                                       TER00780
  500   CONTHT(1) = BEGHT                                               TER00790
        DO 550 I = 2,20                                                 TER00800
  550   CONTHT(I) = BEGHT + HTINCR * (I-1)                              TER00810
C                                                                       TER00820
        XMAX = 0.0                                                      TER00830
        XMIN = 0.0                                                      TER00840
        YMAX = 0.0                                                      TER00850
        YMIN = 0.0                                                      TER00860
C                                                                       TER00870
        DO 1000 I = 1,36                                                TER00880
        NUM = 0                                                         TER00890
        READ(5,4,ERR=9000,END=2000) IWD,(CONTUR(I,J),J=1,20)            TER00900
    4   FORMAT(I3,7X,10F7.2,/,10X,10F7.2)                               TER00910
        IF(IWD.EQ.I*10) GO TO 600                                       TER00920
        IDIR = I*10                                                     TER00930
        WRITE(6,6) IWD,IDIR                                             TER00940
    6   FORMAT(/,'  WIND DIRECTION SPECIFIED FOR TERRAIN (',I3,') DOES',TER00950
     X  ' NOT AGREE WITH EXPECTED DIRECTION (',I3,').')                 TER00960
        GO TO 400                                                       TER00970
  600   DO 700 J = 1,20                                                 TER00980
C       IF(CONTUR(I,J).LT.0.0) GO TO 800    @  END OF TERRAIN DATA      IBM00990
        IF(CONTUR(I,J).LT.0.0) GO TO 800                                IBM00032
        IF(IWD.EQ.999) GO TO 2000                                       TER01000
        IF(J.EQ.1) GO TO 700                                            TER01010
        IF(CONTUR(I,J).GT.CONTUR(I,J-1)) GO TO 700                      TER01020
        WRITE(6,7) IWD,J                                                TER01030
    7   FORMAT(/,'  FOR WIND DIRECTION OF ',I3,' DEGREES, THE DISTANCE',TER01040
     X  ' TO CONTOUR # ',I2,' DOES NOT INCREASE OVER THE PREVIOUS ',    TER01050
     X  'DISTANCE.')                                                    TER01060
        GO TO 400                                                       TER01070
  700   NUM = NUM + 1                                                   TER01080
C                                                                       TER01090
  800   CONTUR(I,21) = FLOAT(NUM)                                       TER01100
        IF(NUM.EQ.0) GO TO 1000                                         TER01110
        THETA = FLOAT(I*10)                                             TER01120
        R = CONTUR(I,NUM) * HSCALE                                      TER01130
        X = R * COS((270. - THETA)/57.2958)                             TER01140
        Y = R * SIN((270. - THETA)/57.2958)                             TER01150
        IF(XMAX.LT.X) XMAX = X                                          TER01160
        IF(XMIN.GT.X) XMIN = X                                          TER01170
        IF(YMAX.LT.Y) YMAX = Y                                          TER01180
        IF(YMIN.GT.Y) YMIN = Y                                          TER01190
 1000   CONTINUE                                                        TER01200
        GO TO 2500                                                      TER01210
C                                                                       TER01220
 2000   WRITE(6,8)                                                      TER01230
    8   FORMAT(/,'  NUMBER OF DIRECTIONS FOR WHICH TERRAIN IS ',        TER01240
     X  'SPECIFIED IS NOT 36, AS EXPECTED.')                            TER01250
        GO TO 400                                                       TER01260
C                                                                       TER01270
C       PRINT CONTOUR INFORMATION                                       TER01280
C                                                                       TER01290
 2500   WRITE(6,10) CONTHT                                              TER01300
   10   FORMAT(//,10X,'TERRAIN INFORMATION:',///,18X,'TABLE OF ',       TER01310
     X  'DISTANCES (KM) FROM THE SOURCE TO ELEVATION CONTOURS AS A ',   TER01320
     X  'FUNCTION OF WIND DIRECTION',///,60X,'ELEVATION IN USER UNITS:',TER01330
     X  //,' WIND DIR.  ',20F6.0,/)                                     TER01340
        DO 2600 I = 1,36                                                TER01350
        IDIR = I*10                                                     TER01360
        NUM = IFIX(CONTUR(I,21))                                        TER01370
        WRITE(6,12) IDIR,(CONTUR(I,J),J=1,NUM)                          TER01380
   12   FORMAT(3X,I3,6X,20F6.2)                                         TER01390
 2600   CONTINUE                                                        TER01400
C                                                                       TER01410
C       START TO FILL CONTOUR MAP                                       TER01420
C                                                                       TER01430
 3000   DO 3100 I = 1,55                                                TER01440
        DO 3100 J = 1,55                                                TER01450
 3100   MAP(I,J) = BLANK                                                TER01460
        XRANGE = AMAX1(XMAX - XMIN,0.1)                                 TER01470
        YRANGE = AMAX1(YMAX - YMIN,0.1)                                 TER01480
        IX = INT(-XMIN/XRANGE * 54. + 1.5)                              TER01490
        IY = 56 - INT(-YMIN/YRANGE * 54. + 1.5)                         TER01500
C       MAP(IY,IX) = PLUS    @ POSITION FOR SOURCE                      IBM01510
        MAP(IY,IX) = PLUS                                               IBM00033
C                                                                       TER01520
C       ADD TERRAIN POINTS SPECIFIED ABOVE                              TER01530
C                                                                       TER01540
        DO 4000 I = 1,36                                                TER01550
        NMAX = INT(CONTUR(I,21))                                        TER01560
        IF(NMAX.EQ.0) GO TO 4000                                        TER01570
        DO 3500 J = 1,NMAX                                              TER01580
        R = CONTUR(I,J) * HSCALE                                        TER01590
        THETA = FLOAT(I*10)                                             TER01600
        X = R * COS((270. - THETA)/57.2958)                             TER01610
        Y = R * SIN((270. - THETA)/57.2958)                             TER01620
        IX = INT((X-XMIN)/XRANGE * 54. + 1.5)                           TER01630
        IY = 56 - INT((Y-YMIN)/YRANGE * 54. + 1.5)                      TER01640
C       MAP(IY,IX) = CODES(J)    @ POSITION FOR CONTOUR                 IBM01650
        MAP(IY,IX) = CODES(J)                                           IBM00034
 3500   CONTINUE                                                        TER01660
 4000   CONTINUE                                                        TER01670
C                                                                       TER01680
C       ADD RECEPTOR POINTS IF THEY ARE WITHIN THE MAPPED AREA          TER01690
C                                                                       TER01700
        DO 5000 I = 1,NR                                                TER01710
        X = RX(I) - UTMX                                                TER01720
        Y = RY(I) - UTMY                                                TER01730
        IX = INT((X-XMIN)/XRANGE * 54. + 1.5)                           TER01740
        IY = 56 - INT((Y-YMIN)/YRANGE * 54. + 1.5)                      TER01750
        IF(IX.LT.1.OR.IX.GT.55.OR.IY.LT.1.OR.IY.GT.55) GO TO 5000       TER01760
        MAP(IY,IX) = STAR                                               TER01770
 5000   CONTINUE                                                        TER01780
C                                                                       TER01790
C       PRINT THE MAP                                                   TER01800
C                                                                       TER01810
        NPAGE = NPAGE + 1                                               TER01820
        WRITE(6,20) XMIN,XMAX,YMIN,YMAX                                 TER01830
   20   FORMAT('1','  MAP OF USER-SPECIFIED TERRAIN:  LETTERS (SOME ',  TER01840
     X  'OVERWRITTEN) DENOTE ELEVATIONS (SEE TABLE); "+" = SOURCE;',    TER01850
     X  ' "*" = A RECEPTOR POINT',/,6X,'DISTANCES FROM SOURCE ',        TER01860
     X  '(0,0):  XMIN = ',F9.2,                                         TER01870
     X  ' M,     XMAX = ',F9.2,' M,     YMIN = ',F9.2,' M,     ',       TER01880
     X  'YMAX = ',F9.2,' M')                                            TER01890
        WRITE(6,21) ((MAP(I,J),J=1,55),I=1,10)                          TER01900
   21   FORMAT(9(6X,55(A1,1X),/),6X,55(A1,1X))                          TER01910
        WRITE(6,22) ((MAP(I,J),J=1,55),I=11,13)                         TER01920
   22   FORMAT(6X,55(A1,1X),'   CONTOUR',/,6X,55(A1,1X),'   CODES',/,   TER01930
     X  6X,55(A1,1X))                                                   TER01940
        WRITE(6,23) ((MAP(I+13,J),J=1,55),CODES(I),CONTHT(I),I=1,20)    TER01950
   23   FORMAT(19(6X,55(A1,1X),2X,A1,1X,F8.2,/),6X,55(A1,1X),2X,A1,     TER01960
     X  1X,F8.2)                                                        TER01970
        WRITE(6,24) ((MAP(I,J),J=1,55),I=34,55)                         TER01980
   24   FORMAT(22(6X,55(A1,1X),/),6X,55(A1,1X))                         TER01990
C                                                                       TER02000
        RETURN                                                          TER02010
C                                                                       TER02020
 9000   WRITE(6,30)                                                     TER02030
   30  FORMAT(//,' *** READ ERROR IN TERRAIN ***')                      TER02040
       WRITE(6,40)                                                      TER02050
   40  FORMAT(//,' *FATAL ERROR*',//,' EXECUTION TERMINATING DUE TO ',  TER02060
     X 'FORMAT ERROR IN TERRAIN SECTION.')                              TER02070
C                                                                       TER02080
       STOP                                                             TER02090
       END                                                              TER02100

       SUBROUTINE RTXEQ                                                 RTX00010
C******************************************************                 RTX00020
C                                                                       RTX00030
C      THE SUBROUTINE READS THE HOURLY DATA AND PASSES                  RTX00040
C      IT TO THE COMPUTATIONAL ROUTINES.                                RTX00050
C                                                                       RTX00060
C      ENVIRONMENTAL RESEARCH AND TECHNOLOGY                            RTX00070
C      696 VIRGINIA ROAD, CONCORD, MASS 01742                           RTX00080
C                                                                       RTX00090
C      RTDM    VERSION 3.2     LEVEL 861015                             RTX00100
C******************************************************                 RTX00110
C                                                                       RTX00120
C******************************************************                 RTX00130
C                                                                       RTX00140
C      COMMON BLOCK FOR STACK SPECIFIC DATA                             RTX00150
C                                                                       RTX00160
C******************************************************                 RTX00170
C      COMMON /STACKS/                                                  IBM00180
C    X                 SNAME(35),   @STACK NAMES                        IBM00190
C    X                 HEIGHT(35),  @STACK HEIGHT (M)                   IBM00200
C    X                 DIAMTR(35),  @STACK DIAMETER (M)                 IBM00210
C    X                 EXITVL(35),  @STACK EXIT VELOCITY (M/SEC)        IBM00220
C    X                 STEMP(35),   @STACK GAS EXIT TEMP (DEG K)        IBM00230
C    X                 EMISR(35),   @EMISSION RATE  (G/SEC)             IBM00240
C    X                 UTMX,        @X-COORDINATE FOR ALL STACKS        IBM00250
C    X                 UTMY,        @Y-COORDINATE FOR ALL STACKS        IBM00260
C    X                 BASEHT,      @BASE HEIGHT FOR ALL STACKS         IBM00270
C    X                 NS,          @NUMBER OF STACKS                   IBM00280
C    X                 EMSLBL       @LABEL FOR POLLUTANT                IBM00290
       CHARACTER*4  SNAME(35)                                           IBM00035
       COMMON /STACKS/ HEIGHT(35),DIAMTR(35),EXITVL(35),STEMP(35),      IBM00036
     X                 EMISR(35),UTMX,UTMY,BASEHT,NS,EMSLBL             IBM00037
       COMMON /STACK2/ SNAME                                            IBM00038
C                                                                       RTX00300
C******************************************************                 RTX00310
C                                                                       RTX00320
C      COMMON BLOCK FOR HOURLY METEOROLOGICAL DATA                      RTX00330
C                                                                       RTX00340
C******************************************************                 RTX00350
C      COMMON /MET/                                                     IBM00360
C    X                 WDIR,        @ WIND DIRECTION                    IBM00370
C    X                 WSPD,        @ WIND SPEED #1 (M/SEC)             IBM00380
C    X                 DMX,         @ MIXING HEIGHT (M)                 IBM00390
C    X                 IST,         @ STABILITY CLASS                   IBM00400
C    X                 TEMP,        @ AMBIENT TEMPERATURE (DEG K)       IBM00410
C    X                 XIY,         @ TURBULENCE, Y-COMPONENT           IBM00420
C    X                 XIZ,         @ TURBULENCE, Z-COMPONENT           IBM00430
C    X                 PRVPTG,      @ PLUME RISE VPTG                   IBM00440
C    X                 HCVPTG,      @ HCRIT VPTG                        IBM00450
C    X                 DTHETA,      @ HORIZONTAL WIND DIRECTION SHEAR   IBM00460
C    X                 EPS,         @ WIND SPEED PROFILE EXPONENT       IBM00470
C    X                 WSPD2        @ WIND SPEED #2 (M/SEC)             IBM00480
       COMMON/MET/ WDIR,WSPD,DMX,IST,TEMP,XIY,XIZ,PRVPTG,HCVPTG,        IBM00039
     X             DTHETA,EPS,WSPD2                                     IBM00040
C                                                                       RTX00490
C******************************************************                 RTX00500
C                                                                       RTX00510
C      COMMON BLOCK FOR MODEL PARAMETERS AND SWITCHES                   RTX00520
C                                                                       RTX00530
C******************************************************                 RTX00540
C                                                                       RTX00550
        DIMENSION AY(6,3),BY(6,3),CY(6,3),                              RTX00560
     X          AZ(6,3),BZ(6,3),CZ(6,3)                                 RTX00570
        COMMON /PR001/ HSCALE                                           RTX00580
        COMMON /PR002/ VSCALE                                           RTX00590
        COMMON /PR003/ USCALE                                           RTX00600
        COMMON /PR004/ ZWIND1,ZWIND2,IDILUT,ZA                          RTX00610
        COMMON /PR005/ EXPON(6)                                         RTX00620
        COMMON /PR006/ ICOEF                                            RTX00630
        COMMON /PR007/ AY, BY, CY, XY1,XY2                              RTX00640
        COMMON /PR008/ AZ, BZ, CZ, XZ1,XZ2                              RTX00650
        COMMON /PR009/ IPPP,PPVPTG                                      RTX00660
        COMMON /PR010/ IBUOY,ALPHA                                      RTX00670
        COMMON /PR011/ IDMX                                             RTX00680
        COMMON /PR012/ ITRANS                                           RTX00690
        COMMON /PR013/ TERCOR(6)                                        RTX00700
        COMMON /PR014/ RVPTG(2)                                         RTX00710
        COMMON /PR015/ ITIPD                                            RTX00720
        COMMON /PR016/ IY                                               RTX00730
        COMMON /PR017/ IZ                                               RTX00740
        COMMON /PR018/ IRVPTG                                           RTX00750
        COMMON /PR019/ IHVPTG                                           RTX00760
        COMMON /PR020/ ISHEAR,SHCOEF                                    RTX00770
        COMMON /PR021/ IEPS                                             RTX00780
        COMMON /PR022/ IREFL                                            RTX00790
        COMMON /PR023/ IHORIZ,SECTOR(6)                                 RTX00800
        COMMON /PR024/ IEMIS                                            RTX00810
C                                                                       RTX00820
        COMMON /PR025/ IVERB                                            RTX00830
C                                                                       RTX00840
        REAL OEMISR(35),OXVEL(35),OSTEMP(35)                            RTX00850
       INTEGER OIST                                                     RTX00860
       CHARACTER*4 IF1,IF2,IF3,IF4,IF5,IF6,IF7                          IBM00041
C                                                                       RTX00870
        CALL PAGE                                                       RTX00880
C                                                                       RTX00890
C      READ INITIAL PERSISTENCE VALUES FOR METEOROLOGY                  RTX00900
C                                                                       RTX00910
       READ (5,1,ERR=9000) OWDIR,OWSPD,ODMX,OLDIST,OTEMP,OIY,OIZ,       RTX00920
     X OPRDT,OHCDT,ODTH,OEPS,OWSPD2                                     RTX00930
    1  FORMAT(8X,12F6.0)                                                RTX00940
       OIST = INT(OLDIST)                                               RTX00950
       IF(OIST.EQ.7) OIST = 6                                           RTX00960
       OWSPD = OWSPD * USCALE                                           RTX00970
       OWSPD2 = OWSPD2 * USCALE                                         RTX00980
       OTEMP = (OTEMP-32.)/1.8 + 273.15                                 RTX00990
C                                                                       RTX01000
       DO 500 I = 1,NS                                                  RTX01010
       OEMISR(I) = EMISR(I)                                             RTX01020
       OXVEL(I) = EXITVL(I)                                             RTX01030
  500  OSTEMP(I) = STEMP(I)                                             RTX01040
C                                                                       RTX01050
       ICOUNT=0                                                         RTX01060
C                                                                       RTX01070
        WRITE(6,2)                                                      RTX01080
    2   FORMAT(/,' THE CODE FOR MISSING DATA IS PRESENTED AS FOLLOWS:', RTX01090
     X  /,'   A "1" IS INCLUDED IF WIND DIRECTION IS MISSING,',/,       RTX01100
     X  '   A "2" IS INCLUDED IF WIND SPEED IS MISSING,',/,             RTX01110
     X  '   A "3" IS INCLUDED IF MIXING HEIGHT IS MISSING,',/,          RTX01120
     X  '   A "4" IS INCLUDED IF STABILITY CLASS IS MISSING,',/,        RTX01130
     X  '   A "5" IS INCLUDED IF AMBIENT TEMPERATURE IS MISSING.',/,    RTX01140
     X  '   IF THERE IS MISSING DATA FOR ANY OF THESE PARAMETERS, THE ',RTX01150
     X  'PREVIOUS GOOD VALUE IS USED.',/,'   IF THERE IS MISSING DATA', RTX01160
     X  ' FOR OPTIONAL PARAMETERS, THE DEFAULT VALUE (OBTAINED FROM ',  RTX01170
     X  'THE STABILITY CLASS) IS USED.',//)                             RTX01180
C                                                                       RTX01190
C       IF1 = '*   '    @ SET PRINT FLAGS FOR OPTIONAL MET              IBM01200
        IF1 = '*   '                                                    IBM00042
        IF2 = '*   '                                                    RTX01210
        IF3 = '*   '                                                    RTX01220
        IF4 = '*   '                                                    RTX01230
        IF5 = '*   '                                                    RTX01240
        IF6 = '*   '                                                    RTX01250
        IF7 = '*   '                                                    RTX01260
        IPRINT = 1                                                      RTX01270
        K = 0                                                           RTX01280
        IF(IY.EQ.0) GO TO 600                                           RTX01290
        IF1 = '    '                                                    RTX01300
        K = K + 1                                                       RTX01310
  600   IF(IZ.EQ.0) GO TO 610                                           RTX01320
        IF2 = '    '                                                    RTX01330
        K = K + 1                                                       RTX01340
  610   IF(IRVPTG.EQ.0) GO TO 620                                       RTX01350
        IF3 = '    '                                                    RTX01360
        K = K + 1                                                       RTX01370
  620   IF(IHVPTG.EQ.0) GO TO 630                                       RTX01380
        IF4 = '    '                                                    RTX01390
        K = K + 1                                                       RTX01400
  630   IF(ISHEAR.EQ.0) GO TO 640                                       RTX01410
        IF5 = '    '                                                    RTX01420
        K = K + 1                                                       RTX01430
  640   IF(IEPS.EQ.0) GO TO 650                                         RTX01440
        IF6 = '    '                                                    RTX01450
        K = K + 1                                                       RTX01460
  650   IF(IDILUT.LT.2) GO TO 700                                       RTX01470
        IF7 = '    '                                                    RTX01480
        K = K + 1                                                       RTX01490
  700   IF(K.EQ.7) IPRINT = 0                                           RTX01500
C                                                                       RTX01510
C      LOOP OVER ALL DATA IN DATAFILE                                   RTX01520
C                                                                       RTX01530
 1000   ICOUNT = ICOUNT + 1                                             RTX01540
        NMISS = 0                                                       RTX01550
C                                                                       RTX01560
        READ(7,5,ERR=9000,END=5000) IYR,IDAY,IHR,WDIR,WSPD,DMX,STAB,    RTX01570
     X   TEMP,XIY,XIZ,PRVPTG,HCVPTG,DTHETA,EPS,WSPD2                    RTX01580
    5   FORMAT(I2,I3,I2,1X,12F6.0)                                      RTX01590
        WSPD = WSPD * USCALE                                            RTX01600
        WSPD2 = WSPD2 * USCALE                                          RTX01610
        IST = INT(STAB)                                                 RTX01620
        TEMP = (TEMP-32.)/1.8 + 273.15                                  RTX01630
        IF(IST.EQ.7) IST = 6                                            RTX01640
        IF(IEMIS.EQ.0) GO TO 1500                                       RTX01650
C                                                                       RTX01660
        DO 1200 I = 1,NS                                                RTX01670
        READ(8,7,ERR=9500,END=5000) JYR,JDAY,JHR,EMISR(I),EXITVL(I),    RTX01680
     X  STEMP(I)                                                        RTX01690
    7   FORMAT(I2,I3,I2,3X,3F10.0)                                      RTX01700
        IF(JYR.NE.IYR.OR.JDAY.NE.IDAY.OR.JHR.NE.IHR) GO TO 8000         RTX01710
 1200   CONTINUE                                                        RTX01720
C                                                                       RTX01730
 1500   IF(ICOUNT.EQ.1) GO TO 1700                                      RTX01740
        KHR = IHR                                                       RTX01750
        KDAY = IDAY                                                     RTX01760
        IF(KHR.GT.1) GO TO 1600                                         RTX01770
        KHR = 25                                                        RTX01780
        KDAY = KDAY - 1                                                 RTX01790
        IF(KDAY.LT.1) KDAY = 366                                        RTX01800
        IF(LASTYR/4*4.NE.LASTYR.AND.KDAY.EQ.366) KDAY = 365             RTX01810
 1600   IF(KHR - LASTHR.NE.1) GO TO 1650                                RTX01820
        IF(KDAY.NE.LASTDA) GO TO 1650                                   RTX01830
        GO TO 1700                                                      RTX01840
C                                                                       RTX01850
 1650   WRITE(6,10) IYR,IDAY,IHR                                        RTX01860
   10   FORMAT(10X,'WARNING:  HOURLY SEQUENCE OF INPUT DATA HAS BEEN ', RTX01870
     X  'INTERRUPTED AT THE FOLLOWING YEAR/DAY/HOUR: ',I2,1X,I3,1X,I2)  RTX01880
C                                                                       RTX01890
C       CHECK FOR MISSING DATA                                          RTX01900
C                                                                       RTX01910
 1700   IF(WDIR.GE.0.0.AND.WDIR.LE.360.) GO TO 1710                     RTX01920
        WDIR = OWDIR                                                    RTX01930
        NMISS = NMISS + 10000                                           RTX01940
 1710   IF(WSPD.GT.0.0.AND.WSPD.LE.98.9) GO TO 1720                     RTX01950
        WSPD = OWSPD                                                    RTX01960
        NMISS = NMISS + 2000                                            RTX01970
C1720   IF(WSPD.LT.1.0) WSPD = 1.0  @ SET MINIMUM WIND SPEED TO 1 M/SEC IBM01980
 1720   IF(WSPD.LT.1.0) WSPD = 1.0                                      IBM00043
        IF(IST.GE.1.AND.IST.LE.6) GO TO 1730                            RTX01990
        IST = OIST                                                      RTX02000
        NMISS = NMISS + 40                                              RTX02010
 1730   IF(.NOT.(IST.GT.4.AND.IDMX.EQ.1)) GO TO 1735                    RTX02020
        DMX = 9999.                                                     RTX02030
        GO TO 1740                                                      RTX02040
 1735   IF(DMX.GE.0.0) GO TO 1740                                       RTX02050
        DMX = ODMX                                                      RTX02060
        NMISS = NMISS + 300                                             RTX02070
 1740   IF(TEMP.GT.200.0 .AND. TEMP.LT.330.) GO TO 1750                 RTX02080
        TEMP = OTEMP                                                    RTX02090
        NMISS = NMISS + 5                                               RTX02100
 1750   IF(XIY.LT.0.0.OR.XIY.GT.3.14) XIY = OIY                         RTX02110
        IF(XIZ.LT.0.0.OR.XIZ.GT.3.14) XIZ = OIZ                         RTX02120
        IF(PRVPTG.LT.0.0.OR.PRVPTG.GT.8.99) PRVPTG = OPRDT              RTX02130
        IF(HCVPTG.LT.0.0.OR.HCVPTG.GT.8.99) HCVPTG = OHCDT              RTX02140
        IF(DTHETA.LT.0.0.OR.DTHETA.GT.180.) DTHETA = ODTH               RTX02150
        IF(EPS.LT.-8.99.OR.EPS.GT.8.99) EPS = OEPS                      RTX02160
        IF(WSPD2.LE.0.0.OR.WSPD2.GT.98.9) WSPD2 = OWSPD2                RTX02170
C                                                                       RTX02180
        IF(NMISS.GT.0) WRITE(6,12) IYR,IDAY,IHR,NMISS                   RTX02190
   12   FORMAT('  MISSING DATA CODE FOR YEAR, DAY, HOUR ',I2,1X,I3,1X,  RTX02200
     X  I2,' IS ',I5)                                                   RTX02210
C                                                                       RTX02220
        OWDIR = WDIR                                                    RTX02230
        OWSPD = WSPD                                                    RTX02240
        ODMX = DMX                                                      RTX02250
        OIST = IST                                                      RTX02260
        OTEMP = TEMP                                                    RTX02270
        OIY = XIY                                                       RTX02280
        OIZ = XIZ                                                       RTX02290
        OPRDT = PRVPTG                                                  RTX02300
        OHCDT = HCVPTG                                                  RTX02310
        ODTH = DTHETA                                                   RTX02320
        OEPS = EPS                                                      RTX02330
        OWSPD2 = WSPD2                                                  RTX02340
C                                                                       RTX02350
        IF(IEMIS.EQ.0) GO TO 3000                                       RTX02360
C                                                                       RTX02370
        DO 2500 I = 1,NS                                                RTX02380
        IF(EMISR(I).LT.0.0) EMISR(I) = OEMISR(I)                        RTX02390
        IF(EXITVL(I).LT.0.0) EXITVL(I) = OXVEL(I)                       RTX02400
        IF(STEMP(I).LT.0.0) STEMP(I) = OSTEMP(I)                        RTX02410
        OEMISR(I) = EMISR(I)                                            RTX02420
        OXVEL(I) = EXITVL(I)                                            RTX02430
        OSTEMP(I) = STEMP(I)                                            RTX02440
 2500   CONTINUE                                                        RTX02450
C                                                                       RTX02460
 3000   LASTYR = IYR                                                    RTX02470
        LASTDA = IDAY                                                   RTX02480
        LASTHR = IHR                                                    RTX02490
        IF(IVERB.EQ.0) GO TO 3500                                       RTX02500
        WRITE(6,17)                                                     RTX02510
   17   FORMAT(//,1X,130('-'),/)                                        RTX02520
        WRITE(6,18) ICOUNT,IYR,IDAY,IHR,WDIR,WSPD,DMX,IST,TEMP,XIY,     RTX02530
     X  IF1,XIZ,IF2,PRVPTG,IF3,HCVPTG,IF4,DTHETA,IF5,EPS,IF6,WSPD2,IF7  RTX02540
   18   FORMAT('  CASE-HOUR # ',I4,10X,'YEAR, DAY, HOUR: ',I2,1X,I3,    RTX02550
     X  1X,I2,/,2X,'             WIND     MIXING               AIR    ',RTX02560
     X     '  TURB.     TURB.     VPTG,      VPTG,    DELTA   ',        RTX02570
     X     '   WSPD      WIND',/,                                       RTX02580
     X  2X,'   WIND    SPEED #1   HEIGHT   STABILITY   TEMP   ',        RTX02590
     X     'INTENSITY INTENSITY   PLUME      HCRIT    THETA   ',        RTX02600
     X     ' PROFILE   SPEED #2',/,                                     RTX02610
     X  2X,'DIRECTION  (M/SEC)      (M)      CLASS     (K)    ',        RTX02620
     X     '   (Y)       (Z)     (DEG K/M) (DEG K/M) (DEG/M)  ',        RTX02630
     X     ' EXPONENT   (M/SEC)',//,                                    RTX02640
     X  5X,F4.0,5X,F5.2,5X,F5.0,8X,I1,6X,F6.2,4X,6(F6.4,A1,3X),F5.2,A1) RTX02650
        IF(IPRINT.EQ.1) WRITE(6,19)                                     RTX02660
   19   FORMAT(/,' * FLAGGED VALUES REPRESENT OPTIONAL METEOROLOGICAL ',RTX02670
     X  'INPUT NOT USED IN THIS RUN')                                   RTX02680
C3500   CALL CONC    @ DO CONCENTRATION CALCULATIONS                    IBM02690
 3500   CALL CONC                                                       IBM00044
        GO TO 1000                                                      RTX02700
C                                                                       RTX02710
C                                                                       RTX02720
 5000   RETURN                                                          RTX02730
C                                                                       RTX02740
 8000   WRITE(6,20) IYR,IDAY,IHR,JYR,JDAY,JHR                           RTX02750
   20   FORMAT('   HOUR OF METEOROLOGICAL DATA (',I2,1X,I3,1X,I2,       RTX02760
     X  ') DOES NOT AGREE WITH HOUR OF EMISSIONS DATA (',I2,1X,I3,1X,   RTX02770
     X  I2,')')                                                         RTX02780
        WRITE(6,21)                                                     RTX02790
   21   FORMAT(' * FATAL ERROR *',//,' EXECUTION TERMINATED DUE TO ',   RTX02800
     X  'TIME DISAGREEMENT BETWEEN METEOROLOGICAL AND EMISSIONS DATA.') RTX02810
        STOP                                                            RTX02820
C                                                                       RTX02830
 9000   WRITE(6,25) LASTYR,LASTDA,LASTHR                                RTX02840
   25   FORMAT('   FORMAT ERROR IN METEOROLOGICAL DATA.  LAST GOOD ',   RTX02850
     X  'HOUR WAS ',I2,1X,I3,1X,I2)                                     RTX02860
        GO TO 9800                                                      RTX02870
C                                                                       RTX02880
 9500   WRITE(6,27) LASTYR,LASTDA,LASTHR                                RTX02890
   27   FORMAT('   FORMAT ERROR IN EMISSIONS DATA.  LAST GOOD ',        RTX02900
     X  'HOUR WAS ',I2,1X,I3,1X,I2)                                     RTX02910
C                                                                       RTX02920
 9800   WRITE(6,28)                                                     RTX02930
   28   FORMAT(' * FATAL ERROR *',//,' EXECUTION TERMINATED DUE TO ',   RTX02940
     X  'FORMAT ERROR IN INPUT DATA.')                                  RTX02950
        STOP                                                            RTX02960
        END                                                             RTX02970

        SUBROUTINE CONC                                                 CON00010
C                                                                       CON00020
C***********************************************************************CON00030
C                                                                       CON00040
C       THIS SUBROUTINE COMPUTES CONCENTRATIONS AT ALL RECEPTORS        CON00050
C       FOR ALL STACKS, GIVEN A SET OF METEOROLOGICAL CONDITIONS        CON00060
C                                                                       CON00070
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          CON00080
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          CON00090
C                                                                       CON00100
C       RTDM      VERSION 3.2     LEVEL 861015                          CON00110
C********************************************************************   CON00120
C                                                                       CON00130
C******************************************************                 CON00140
C                                                                       CON00150
C      COMMON BLOCK FOR STACK SPECIFIC DATA                             CON00160
C                                                                       CON00170
C******************************************************                 CON00180
C      COMMON /STACKS/                                                  IBM00190
C    X                 SNAME(35),   @STACK NAMES                        IBM00200
C    X                 HEIGHT(35),  @STACK HEIGHT (M)                   IBM00210
C    X                 DIAMTR(35),  @STACK DIAMETER (M)                 IBM00220
C    X                 EXITVL(35),  @STACK EXIT VELOCITY (M/SEC)        IBM00230
C    X                 STEMP(35),   @STACK GAS EXIT TEMP (DEG K)        IBM00240
C    X                 EMISR(35),   @EMISSION RATE  (G/SEC)             IBM00250
C    X                 UTMX,        @X-COORDINATE FOR ALL STACKS        IBM00260
C    X                 UTMY,        @Y-COORDINATE FOR ALL STACKS        IBM00270
C    X                 BASEHT,      @BASE HEIGHT FOR ALL STACKS         IBM00280
C    X                 NS,          @NUMBER OF STACKS                   IBM00290
C    X                 EMSLBL       @LABEL FOR POLLUTANT                IBM00300
       CHARACTER*4 SNAME(35)                                            IBM00045
       COMMON /STACKS/ HEIGHT(35),DIAMTR(35),EXITVL(35),STEMP(35),      IBM00046
     X                 EMISR(35),UTMX,UTMY,BASEHT,NS,EMSLBL             IBM00047
       COMMON /STACK2/ SNAME                                            IBM00048
C                                                                       CON00310
C******************************************************                 CON00320
C                                                                       CON00330
C      COMMON BLOCK FOR RECEPTOR SPECIFIC DATA                          CON00340
C                                                                       CON00350
C******************************************************                 CON00360
C                                                                       CON00370
       COMMON /PLACE/ RX(400),RY(400),RZ(400),NR                        CON00380
C              RX              @RECEPTOR X-COORDINATES, METERS          CON00390
C              RY              @RECEPTOR Y-COORDINATES,  METERS         CON00400
C              RZ              @RECEPTOR TERRAIN HEIGHTS, USER UNITS    CON00410
C                                                                       CON00420
C******************************************************                 CON00430
C                                                                       CON00440
C      COMMON BLOCK FOR HOURLY METEOROLOGICAL DATA                      CON00450
C                                                                       CON00460
C******************************************************                 CON00470
C      COMMON /MET/                                                     IBM00480
C    X                 WDIR,        @ WIND DIRECTION                    IBM00490
C    X                 WSPD,        @ WIND SPEED #1 (M/SEC)             IBM00500
C    X                 DMX,         @ MIXING HEIGHT (M)                 IBM00510
C    X                 IST,         @ STABILITY CLASS                   IBM00520
C    X                 TEMP,        @ AMBIENT TEMPERATURE (DEG K)       IBM00530
C    X                 XIY,         @ TURBULENCE, Y-COMPONENT           IBM00540
C    X                 XIZ,         @ TURBULENCE, Z-COMPONENT           IBM00550
C    X                 PRVPTG,      @ PLUME RISE VPTG                   IBM00560
C    X                 HCVPTG,      @ HCRIT VPTG                        IBM00570
C    X                 DTHETA,      @ HORIZONTAL WIND DIRECTION SHEAR   IBM00580
C    X                 EPS,         @ WIND SPEED PROFILE EXPONENT       IBM00590
C    X                 WSPD2        @ WIND SPEED #2 (M/SEC)             IBM00600
       COMMON/MET/ WDIR,WSPD,DMX,IST,TEMP,XIY,XIZ,PRVPTG,HCVPTG,        IBM00049
     X             DTHETA,EPS,WSPD2                                     IBM00050
C******************************************************                 CON00620
C                                                                       CON00630
C      COMMON BLOCK FOR MODEL PARAMETERS AND SWITCHES                   CON00640
C                                                                       CON00650
C******************************************************                 CON00660
C                                                                       CON00670
        DIMENSION AY(6,3),BY(6,3),CY(6,3),                              CON00680
     X          AZ(6,3),BZ(6,3),CZ(6,3)                                 CON00690
        COMMON /PR001/ HSCALE                                           CON00700
        COMMON /PR002/ VSCALE                                           CON00710
        COMMON /PR003/ USCALE                                           CON00720
        COMMON /PR004/ ZWIND1,ZWIND2,IDILUT,ZA                          CON00730
        COMMON /PR005/ EXPON(6)                                         CON00740
        COMMON /PR006/ ICOEF                                            CON00750
        COMMON /PR007/ AY, BY, CY, XY1,XY2                              CON00760
        COMMON /PR008/ AZ, BZ, CZ, XZ1,XZ2                              CON00770
        COMMON /PR009/ IPPP,PPVPTG                                      CON00780
        COMMON /PR010/ IBUOY,ALPHA                                      CON00790
        COMMON /PR011/ IDMX                                             CON00800
        COMMON /PR012/ ITRANS                                           CON00810
        COMMON /PR013/ TERCOR(6)                                        CON00820
        COMMON /PR014/ RVPTG(2)                                         CON00830
        COMMON /PR015/ ITIPD                                            CON00840
        COMMON /PR016/ IY                                               CON00850
        COMMON /PR017/ IZ                                               CON00860
        COMMON /PR018/ IRVPTG                                           CON00870
        COMMON /PR019/ IHVPTG                                           CON00880
        COMMON /PR020/ ISHEAR,SHCOEF                                    CON00890
        COMMON /PR021/ IEPS                                             CON00900
        COMMON /PR022/ IREFL                                            CON00910
        COMMON /PR023/ IHORIZ,SECTOR(6)                                 CON00920
        COMMON /PR024/ IEMIS                                            CON00930
        COMMON /PR025/ IVERB                                            CON00940
C                                                                       CON00950
       REAL CHI(400)                                                    CON00960
C                                                                       CON00970
       IF(IEPS.EQ.0) EPS = EXPON(IST)                                   CON00980
       IF(IRVPTG.EQ.0.AND.IST.GT.4) PRVPTG = RVPTG(IST-4)               CON00990
       IF(IHVPTG.EQ.0.AND.IST.GT.4) HCVPTG = RVPTG(IST-4)               CON01000
C                                                                       CON01010
       DO 1000 I = 1,400                                                CON01020
 1000  CHI(I) = 0.0                                                     CON01030
C      X=UTMX          @STACK X COORDINATE                              IBM01040
       X=UTMX                                                           IBM00051
C      Y=UTMY          @STACK Y COORDINATE                              IBM01050
       Y=UTMY                                                           IBM00052
C                                                                       CON01060
       DO 4000 IS = 1,NS                                                CON01070
C      HT=HEIGHT(IS)   @HEIGHT OF STACK                                 IBM01080
       HT=HEIGHT(IS)                                                    IBM00053
C      DIA=DIAMTR(IS)        @STACK DIAMETER                            IBM01090
       DIA=DIAMTR(IS)                                                   IBM00054
C      EMIS = EMISR(IS)          @EMISSION RATE IN G/SEC                IBM01100
       EMIS = EMISR(IS)                                                 IBM00055
C      XVEL = EXITVL(IS)         @EXIT VELOCITY IN M/SEC                IBM01110
       XVEL = EXITVL(IS)                                                IBM00056
C       ST = STEMP(IS)        @ STACK TEMPERATURE IN DEG K              IBM01120
        ST = STEMP(IS)                                                  IBM00057
C                                                                       CON01130
C      IF(EMIS.LE.0.0) GO TO 4000                     @ NO EMISSIONS    IBM01140
       IF(EMIS.LE.0.0) GO TO 4000                                       IBM00058
C                                                                       CON01150
C      DETERMINE MAXIMUM HEIGHT TO WHICH WIND SPEED IS ALLOWED TO       CON01160
C      INCREASE ACCORDING TO POWER LAW FORMULA                          CON01170
C                                                                       CON01180
       IF(IST.GE.4) GO TO 1050                                          CON01190
       HMX1 = AMAX1(0.1*DMX,ZWIND1)                                     CON01200
       HMX2 = AMAX1(0.1*DMX,ZWIND2)                                     CON01210
       HMAX1 = AMIN1(HT-ZA,HMX1)                                        CON01220
       GO TO 1100                                                       CON01230
C                                                                       CON01240
 1050  WS10 = WSPD*(10./ZWIND1)**EPS                                    CON01250
       HMX1 = AMAX1(200.*WS10,ZWIND1)                                   CON01260
       HMX2 = AMAX1(200.*WS10,ZWIND2)                                   CON01270
       HMAX1 = AMIN1(HT-ZA,HMX1)                                        CON01280
       IF(HMAX1.LT.1.0) HMAX1 = 1.0                                     CON01290
C                                                                       CON01300
C1100  UTOP = WSPD*(HMAX1/ZWIND1)**EPS     @STACK TOP WIND VEL.         IBM01310
 1100  UTOP = WSPD*(HMAX1/ZWIND1)**EPS                                  IBM00059
       IF(UTOP.LT.1.0) UTOP = 1.0                                       CON01320
       IPRINT = 0                                                       CON01330
C                                                                       CON01340
       DO 3500 IR = 1,NR                                                CON01350
C                                                                       CON01360
        IZZ=0                                                           CON01370
        SN = SIN(0.0174533 * WDIR)                                      CON01380
        CS = COS(0.0174533 * WDIR)                                      CON01390
C       CALL PSRCE(X,Y,RX(IR),RY(IR),SN,CS,IZZ,YCROSS,XP) @ HORIZ GEOM  IBM01400
        CALL PSRCE(X,Y,RX(IR),RY(IR),SN,CS,IZZ,YCROSS,XP)               IBM00060
        IF(IZZ.EQ.1) GO TO 3500                                         CON01410
C                                                                       CON01420
C      COMPUTE PLUME RISE                                               CON01430
C                                                                       CON01440
        CALL PRISE(IST,UTOP,XP,XVEL,DIA,ST,TEMP,PRVPTG,RISE,XFIN,       CON01450
C    X  HFIN,FLUX)     @   CALCULATE PLUME RISE                         IBM01460
     X  HFIN,FLUX)                                                      IBM00061
        DELTAH=RISE                                                     CON01470
        H=DELTAH+HT                                                     CON01480
C                                                                       CON01490
C      COMPUTE DILUTION WIND SPEED                                      CON01500
C                                                                       CON01510
       IF(IDILUT.GE.1) GO TO 1200                                       CON01520
C      UDILUT = UTOP    @ SET DILUTION WIND SPEED TO THAT AT STACK TOP  IBM01530
       UDILUT = UTOP                                                    IBM00062
       GO TO 1500                                                       CON01540
C                                                                       CON01550
 1200  IF(IDILUT.EQ.2) GO TO 1300                                       CON01560
       HMAX2 = AMIN1(HT-ZA+RISE,HMX1)                                   CON01570
       HMAX3 = AMIN1(HT-ZA+HFIN,HMX1)                                   CON01580
       IF(HMAX2.LT.1.0) HMAX2 = 1.0                                     CON01590
       IF(HMAX3.LT.1.0) HMAX3 = 1.0                                     CON01600
C      UDILUT = WSPD*((HMAX2)/ZWIND1)**EPS @ EXTRAPOLATE SPEED #1       IBM01610
       UDILUT = WSPD*((HMAX2)/ZWIND1)**EPS                              IBM00063
C      UDILF = WSPD*((HMAX3)/ZWIND1)**EPS @ EXTRAPOLATE TO FINAL RISE   IBM01620
       UDILF = WSPD*((HMAX3)/ZWIND1)**EPS                               IBM00064
       GO TO 1500                                                       CON01630
C                                                                       CON01640
 1300  HMAX2 = AMIN1(HT-ZA+RISE,HMX2)                                   CON01650
       HMAX3 = AMIN1(HT-ZA+HFIN,HMX2)                                   CON01660
       IF(HMAX2.LT.1.0) HMAX2 = 1.0                                     CON01670
       IF(HMAX3.LT.1.0) HMAX3 = 1.0                                     CON01680
C      UDILUT = WSPD2*((HMAX2)/ZWIND2)**EPS @ EXTRAPOLATE SPEED #2      IBM01690
       UDILUT = WSPD2*((HMAX2)/ZWIND2)**EPS                             IBM00065
C      UDILF = WSPD2*((HMAX3)/ZWIND2)**EPS @ EXTRAPOLATE TO FINAL RISE  IBM01700
       UDILF = WSPD2*((HMAX3)/ZWIND2)**EPS                              IBM00066
C                                                                       CON01710
 1500  IF(UDILUT.LT.1.0) UDILUT = 1.0                                   CON01720
       IF(UDILF.LT.1.0) UDILF = 1.0                                     CON01730
       CALL SIGMAY(IST,DTHETA,XP,YCROSS,RISE,XIY,SIGY,                  CON01740
     X SIGYA,SIGYB,SIGYC,HDF)                                           CON01750
C      IF(HDF.LT.1.0E-20) GO TO 3500   @ ESSENTIALLY ZERO CONCENTRATION IBM01760
       IF(HDF.LT.1.0E-20) GO TO 3500                                    IBM00067
       CALL SIGMAZ(IST,XP,RISE,XIZ,SIGZ,SIGZA,SIGZB)                    CON01770
C                                                                       CON01780
       PESS = PPVPTG * 9.806/TEMP                                       CON01790
       PUNCH = 0.0                                                      CON01800
       P=PENETR(DMX,HT,FLUX,PUNCH,HFIN,UTOP,PESS,IPPP)                  CON01810
C                                                                       CON01820
C      IF(P.LT.1.0) GO TO 1700    @ IF P=1, A COMPLETE PUNCH THROUGH    IBM01830
       IF(P.LT.1.0) GO TO 1700                                          IBM00068
C      HCRIT = 0.0    @   NOT YET DEFINED, BUT NOT IMPORTANT            IBM01840
       HCRIT = 0.0                                                      IBM00069
       IF(IVERB.EQ.1.AND.IPRINT.EQ.0)                                   CON01850
     X WRITE(6,5) IS,EMIS,XVEL,ST,HCRIT,UTOP,FLUX,HFIN,XFIN,P           CON01860
C      GO TO 4000    @ NO GROUND-LEVEL CONCENTRATIONS FOR THIS STACK    IBM01870
       GO TO 4000                                                       IBM00070
C                                                                       CON01880
 1700  THT = RZ(IR) - BASEHT                                            CON01890
C                                                                       CON01900
       CALL VERT(H,THT,XP,RISE,UTOP,SIGZ,HCRIT,HPLUME,V,RVDF,R)         CON01910
C                                                                       CON01920
       IF(IVERB.EQ.0 .OR. IPRINT.EQ.1) GO TO 2000                       CON01930
       WRITE(6,5) IS,EMIS,XVEL,ST,HCRIT,UTOP,FLUX,HFIN,XFIN,P           CON01940
    5  FORMAT(/,' STACK #',I2,':  EMISSIONS = ',F8.2,' G/SEC, ',        CON01950
     X ' EXIT VELOCITY = ',F6.2,' M/SEC,  STACK GAS TEMP = ',F7.2,      CON01960
     X ' DEG K,  HCRIT = ',F8.2,' M',/,' UTOP = ',F5.2,' M/SEC,  ',     CON01970
     X ' FLUX = ',F9.2,' M**4/S**3,  FINAL RISE = ',F7.2,               CON01980
     X ' M,  DISTANCE TO FINAL RISE = ',F8.2,' M,  PEN. FRAC. = ',      CON01990
     X F4.2)                                                            CON02000
C                                                                       CON02010
       IF(IDILUT.GE.1) WRITE(6,6) UDILF                                 CON02020
    6  FORMAT(' DILUTION SPEED (AT FINAL RISE PLUME HEIGHT) = ',F5.2,   CON02030
     X ' M/SEC')                                                        CON02040
C                                                                       CON02050
       WRITE(6,7)                                                       CON02060
    7  FORMAT(//,19X,'HT',77X,'CONVENTIONAL REFLECTION',/,18X,'ABOVE ', CON02070
     X 'PLUME HT  PLUME HT',11X,'SIGMA-Y',16X,'SIGMA-Z',8X,'HORIZ.   ', CON02080
     X 'VERTICAL    VERTICAL',7X,'CHI',/,18X,'STACK  BEFORE  CORRECTED',CON02090
     X 49X,'DIST.      DIST.       DIST.',/,                            CON02100
     X ' REC   X     Y    BASE  TERRAIN  BY TERRAIN  AMBIENT BUOY ',    CON02110
     X 'SHEAR  TOTAL AMBIENT BUOY  TOTAL FACTOR    FACTOR      FACTOR ',CON02120
     X 7X,'(UG/',/,'  #   (KM)  (KM)   (M)    (M)       (M)        ',   CON02130
     X '(M)   (M)   (M)    (M)    (M)   (M)    (M)  (1/M)     (1/M)   ',CON02140
     X '    (1/M)      R  M**3)',/)                                     CON02150
C                                                                       CON02160
       IPRINT = 1                                                       CON02170
C2000  CVDF = V     @ CONVENTIONAL VERTICAL DISTRIBUTION FUNCTION       IBM02180
 2000  CVDF = V                                                         IBM00071
C                                                                       CON02190
       VDF = AMIN1(CVDF,RVDF)                                           CON02200
       SCHI = EMIS * HDF * VDF/UDILUT * (1.0-P)                         CON02210
C                                                                       CON02220
       IF(IVERB.EQ.0) GO TO 2500                                        CON02230
       SCHI6 = SCHI * 1000000.                                          CON02240
       XKM = XP/1000.                                                   CON02250
       YKM = YCROSS/1000.                                               CON02260
C                                                                       CON02270
       WRITE(6,9) IR,XKM,YKM,THT,H,HPLUME,SIGYA,                        CON02280
     X SIGYB,SIGYC,SIGY,SIGZA,SIGZB,SIGZ,HDF,CVDF,RVDF,R,SCHI6          CON02290
    9  FORMAT(1X,I3,1X,F5.2,1X,F6.2,1X,F5.0,1X,F8.2,2X,F8.2,3X,         CON02300
     X F6.1,F6.1,F6.0,F7.1,1X,F6.1,F6.1,F7.1,                           CON02310
     X 1X,F6.5,2X,E10.5,2X,E10.5,1X,F4.2,F6.0)                          CON02320
C                                                                       CON02330
 2500  CHI(IR) = CHI(IR) + SCHI                                         CON02340
C                                                                       CON02350
 3500  CONTINUE                                                         CON02360
 4000  CONTINUE                                                         CON02370
C                                                                       CON02380
C      CALL WRCONC(CHI,NR)     @ OUTPUT CONCENTRATIONS FOR THIS HOUR    IBM02390
       CALL WRCONC(CHI,NR)                                              IBM00072
C                                                                       CON02400
       RETURN                                                           CON02410
       END                                                              CON02420

        SUBROUTINE PRISE(IST,UTOP,XP,XVEL,DIA,STEMP,TEMP,PRVPTG,RISE,   RIS00010
     X  XFIN,HFIN,FLUX)                                                 RIS00020
C                                                                       RIS00030
C********************************************************************   RIS00040
C                                                                       RIS00050
C       BRIGGS PLUME RISE                                               RIS00060
C       TRANSITIONAL AND EQUILIBRIUM PLUME RISE ARE CALCULATED          RIS00070
C                                                                       RIS00080
C********************************************************************   RIS00090
C                                                                       RIS00100
C       UNITS USED ARE METRIC                                           RIS00110
C                                                                       RIS00120
C      IST      STABILITY CLASS                                         RIS00130
C      UTOP     STACK TOP WIND SPEED (M/SEC)                            RIS00140
C      XP       DOWNWIND DISTANCE (M)                                   RIS00150
C      XVEL     STACK EXIT VELOCITY (M/SEC)                             RIS00160
C      DIA      STACK DIAMETER (M)                                      RIS00170
C      STEMP    STACK TEMPERATURE (K)                                   RIS00180
C      TEMP     AMBIENT TEMPERATURE (K)                                 RIS00190
C      PRVPTG   VERT. POTENTIAL TEMP. GRADIENT FOR PLUME RISE (DEG K/M) RIS00200
C      RISE     PLUME RISE AT DISTANCE XP  (M)                          RIS00210
C      XFIN     DISTANCE TO FINAL RISE  (M)                             RIS00220
C      HFIN     FINAL RISE  (M)                                         RIS00230
C      FLUX     BUOYANCY FLUX (M**4/S**3)                               RIS00240
C                                                                       RIS00250
C********************************************************************   RIS00260
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          RIS00270
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          RIS00280
C                                                                       RIS00290
C       RTDM      VERSION 3.2     LEVEL 861015                          RIS00300
C********************************************************************   RIS00310
C                                                                       RIS00320
       COMMON /PR012/ ITRANS                                            RIS00330
       COMMON /PR015/ ITIPD                                             RIS00340
C                                                                       RIS00350
       DELT = STEMP - TEMP                                              RIS00360
       IF(DELT.GE.0.1) GO TO 100                                        RIS00370
       STEMP = TEMP + 0.1                                               RIS00380
       DELT = 0.1                                                       RIS00390
  100  FLUX = 2.4515*XVEL*DIA*DIA*(DELT)/STEMP                          RIS00400
       F33=FLUX**.33333                                                 RIS00410
       ESS = 0.                                                         RIS00420
       IF(IST.GT.4) ESS=PRVPTG * 9.806/TEMP                             RIS00430
C                                                                       RIS00440
C                                                                       RIS00450
C      IF A STABLE STABILITY, COMPUTE NEUTRAL RISE ANYWAY TO CHECK      RIS00460
C      WHETHER OR NOT STABLE RISE IS LESS THAN NEUTRAL RISE             RIS00470
C                                                                       RIS00480
C       UNSTABLE OR NEUTRAL RISE                                        RIS00490
C                                                                       RIS00500
       XSTAR=AMIN1(34.*FLUX**.4,14.*FLUX**.625)                         RIS00510
       XFIN = 3.5*XSTAR                                                 RIS00520
       X = XP                                                           RIS00530
       IF(ITRANS.EQ.0) X = XFIN                                         RIS00540
       RISE = (1.6*F33*AMIN1(X,XFIN)**.66667)/UTOP                      RIS00550
       HFIN = (1.6*F33*XFIN**.66667)/UTOP                               RIS00560
C      IF(IST.LE.4.OR.ESS.LE.0.0) GO TO 600  @ NON-STABLE RISE          IBM00570
       IF(IST.LE.4.OR.ESS.LE.0.0) GO TO 600                             IBM00073
       RISE1 = RISE                                                     RIS00580
       XFIN1 = XFIN                                                     RIS00590
C                                                                       RIS00600
C      STABLE RISE SECTION -- COMPUTE FINAL RISE FIRST                  RIS00610
C                                                                       RIS00620
C      RISE2 = 5.0*FLUX**0.25/(ESS**.375)  @ CALM CASE                  IBM00630
       RISE2 = 5.0*FLUX**0.25/(ESS**.375)                               IBM00074
C                                                                       RIS00640
C      RISE3 = 2.6*(F33/(UTOP*ESS)**.3333) @ NON-CALM CASE              IBM00650
       RISE3 = 2.6*(F33/(UTOP*ESS)**.3333)                              IBM00075
C                                                                       RIS00660
C      IF(RISE3.LE.RISE2) GO TO 300  @ TAKE LOWEST OF THE 3 RESULTS     IBM00670
       IF(RISE3.LE.RISE2) GO TO 300                                     IBM00076
C                                                                       RIS00680
       STRISE = RISE2                                                   RIS00690
       STHFIN = RISE2                                                   RIS00700
       GO TO 400                                                        RIS00710
C                                                                       RIS00720
  300  STRISE = RISE3                                                   RIS00730
       STHFIN = RISE3                                                   RIS00740
C                                                                       RIS00750
C      CHECK FOR TRANSITIONAL RISE                                      RIS00760
C                                                                       RIS00770
  400  STXFIN=2.07*UTOP/SQRT(ESS)                                       RIS00780
       IF(XP.GE.STXFIN .OR. ITRANS.EQ.0) GO TO 500                      RIS00790
C      STRISE = 1.6*F33*XP**.6667/UTOP   @ GRADUAL RISE                 IBM00800
       STRISE = 1.6*F33*XP**.6667/UTOP                                  IBM00077
C                                                                       RIS00810
C 500  IF(RISE1.LE.STRISE) GO TO 600  @ NEUTRAL RISE; HFIN,XFIN,RISE DEFIBM00820
  500  IF(RISE1.LE.STRISE) GO TO 600                                    IBM00078
       RISE = STRISE                                                    RIS00830
       XFIN = STXFIN                                                    RIS00840
       HFIN = STHFIN                                                    RIS00850
C                                                                       RIS00860
C      STACK-TIP DOWNWASH SECTION                                       RIS00870
C                                                                       RIS00880
  600  IF(ITIPD.EQ.0) RETURN                                            RIS00890
       A = 2.0*AMAX1(0.0,1.5-XVEL/UTOP)                                 RIS00900
       RISE = RISE - A*DIA - SQRT(8.*A/3.1459) * DIA                    RIS00910
       HFIN = HFIN - A*DIA - SQRT(8.*A/3.1459) * DIA                    RIS00920
       RETURN                                                           RIS00930
       END                                                              RIS00940

      FUNCTION PENETR(DMX,HT,F,PUNCH,DH,U,S,MODE)                       PEN00010
C******************************************************************     PEN00020
C                                                                       PEN00030
C  THIS SUBROUTINE DETERMINES IF                                        PEN00040
C  1) THE PLUME PUNCHES THRU THE INVERSION (PENETR=1), OR               PEN00050
C  2) THE PLUME IS TRAPPED BELOW THE INVERSION (PENETR=0), OR           PEN00060
C  3) THE PLUME PARTIALLY PENETRATES THE INVERSION (PENETR=FRACTION)    PEN00070
C                                                                       PEN00080
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          PEN00090
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          PEN00100
C                                                                       PEN00110
C       RTDM      VERSION 3.2     LEVEL 861015                          PEN00120
C                                                                       PEN00130
C******************************************************************     PEN00140
C                                                                       PEN00150
      REAL HT                                                           PEN00160
      BETA=0.6                                                          PEN00170
      RMEFM=2.25                                                        PEN00180
      IF(MODE.NE.0) GO TO 3000                                          PEN00190
C                                                                       PEN00200
C     ALL OR ZERO PENETRATION                                           PEN00210
C                                                                       PEN00220
      IF(DMX+PUNCH.GE.HT+DH) GO TO 1000                                 PEN00230
      PENETR=1.                                                         PEN00240
      GO TO 7000                                                        PEN00250
 1000 PENETR=0.                                                         PEN00260
      GO TO 7000                                                        PEN00270
C                                                                       PEN00280
C     PARTIAL PENETRATION POSSIBLE                                      PEN00290
C                                                                       PEN00300
 3000 CONTINUE                                                          PEN00310
      ZB=DMX-HT                                                         PEN00320
      IF(ZB.GE.1.E-15) GO TO 4000                                       PEN00330
      PENETR=1.                                                         PEN00340
      RETURN                                                            PEN00350
 4000 CONTINUE                                                          PEN00360
      C=F*RMEFM*2./(BETA*BETA*U*S*ZB*ZB*ZB)                             PEN00370
      Q=C*(C/4.-1./27.)                                                 PEN00380
      IF(Q.LT.0.) GO TO 5000                                            PEN00390
      SQRTQ=SQRT(Q)                                                     PEN00400
      T=C/2.-1./27.                                                     PEN00410
      X=(T+SQRTQ)**.333333+(T-SQRTQ)**.333333+.333333                   PEN00420
      GO TO 6000                                                        PEN00430
 5000 CONTINUE                                                          PEN00440
      X=.666666*COS(.333333*ACOS(13.5*C-1.))+.333333                    PEN00450
 6000 CONTINUE                                                          PEN00460
      Y=1./AMIN1(X,DH/ZB)                                               PEN00470
      PENETR=AMAX1(0.,AMIN1(1.,1.5-Y))                                  PEN00480
 7000 CONTINUE                                                          PEN00490
      RETURN                                                            PEN00500
      END                                                               PEN00510

        SUBROUTINE PSRCE(SX,SY,RX,RY,SND,CSD,IZZ,Y,XP)                  SRC00010
C                                                                       SRC00020
C******************************************************************     SRC00030
C                                                                       SRC00040
C       POINT SOURCE HORIZONTAL GEOMETRY                                SRC00050
C                                                                       SRC00060
C       SX      -STACK X COORDINATE (M)                                 SRC00070
C       SY      -STACK Y COORDINATE (M)                                 SRC00080
C       RX      -RECEPTOR X COORDINATE (M)                              SRC00090
C       RY      -RECEPTOR Y COORDINATE (M)                              SRC00100
C       SND     -SINE OF WIND DIRECTION ANGLE(DEG. FROM N)              SRC00110
C       CSD     -COSINE OF WIND DIRECTION ANGLE                         SRC00120
C       IZZ     -IZZ = 1 IF AN UPWIND RECEPTOR                          SRC00130
C       Y       - CROSSWIND DISTANCE TO SOURCE                          SRC00140
C       XP      - DOWNWIND DISTANCE (METERS) (RETURNED)                 SRC00150
C                                                                       SRC00160
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          SRC00170
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          SRC00180
C                                                                       SRC00190
C       RTDM      VERSION 3.2     LEVEL 861015                          SRC00200
C********************************************************************   SRC00210
C                                                                       SRC00220
        X1 = (SX-RX)                                                    SRC00230
        Y1 = (SY-RY)                                                    SRC00240
C                                                                       SRC00250
C       XP = Y1*CSD+X1*SND        @DOWNWIND DISTANCE TO RECEPTOR        IBM00260
        XP = Y1*CSD+X1*SND                                              IBM00079
C                                                                       SRC00270
C       IF( XP .GT. 0.) GO TO 10        @RECEPTOR IS UPWIND             IBM00280
        IF( XP .GT. 0.) GO TO 10                                        IBM00080
        IZZ=1                                                           SRC00290
        RETURN                                                          SRC00300
   10   CONTINUE                                                        SRC00310
C                                                                       SRC00320
C       Y = Y1*SND-X1*CSD         @DISTANCE FROM CENTER LINE            IBM00330
        Y = Y1*SND-X1*CSD                                               IBM00081
C                                                                       SRC00340
        IF( XP .LT. 10.) XP = 10.                                       SRC00350
C                                                                       SRC00360
        RETURN                                                          SRC00370
        END                                                             SRC00380

        SUBROUTINE SIGMAY(IST,DTHETA,XP,Y,RISE,XIY,SIGY,SIGYA,SIGYB,    SGY00010
     X  SIGYC,HDF)                                                      SGY00020
C                                                                       SGY00030
C********************************************************************   SGY00040
C                                                                       SGY00050
C       THIS ROUTINE COMPUTES SIGMA-Y AND THE HORIZONTAL DISTRIBUTION   SGY00060
C       FUNCTION.                                                       SGY00070
C                                                                       SGY00080
C       IST     - STABILTY CLASS                                        SGY00090
C       DTHETA  - WIND DIRECTION SHEAR, IN DEGREES PER METER            SGY00100
C       XP      - DOWNWIND DISTANCE (METERS)                            SGY00110
C       Y       - CROSSWIND DISTANCE (METERS)                           SGY00120
C       RISE    - PLUME RISE (METERS)                                   SGY00130
C       XIY     - TURBULENCE INTENSITY (Y-COMPONENT)                    SGY00140
C       SIGY    - SIGMA-Y                                               SGY00150
C       SIGYA   - SIGMA-Y, ATMOSPHERIC DISPERSION COMPONENT             SGY00160
C       SIGYB   - SIGMA-Y, BUOYANCY ENHANCEMENT COMPONENT               SGY00170
C       SIGYC   - SIGMA-Y, SHEAR COMPONENT                              SGY00180
C       HDF     - HORIZONTAL DISTRIBUTION FUNCTION IN GAUSSIAN EQUATION SGY00190
C                                                                       SGY00200
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          SGY00210
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          SGY00220
C                                                                       SGY00230
C       RTDM     VERSION 3.2     LEVEL 861015                           SGY00240
C********************************************************************   SGY00250
C                                                                       SGY00260
        DIMENSION AY(6,3),BY(6,3),CY(6,3)                               SGY00270
        COMMON /PR006/ ICOEF                                            SGY00280
        COMMON /PR007/ AY, BY, CY, XY1,XY2                              SGY00290
        COMMON /PR010/ IBUOY,ALPHA                                      SGY00300
        COMMON /PR016/ IY                                               SGY00310
        COMMON /PR020/ ISHEAR,SHCOEF                                    SGY00320
        COMMON /PR023/ IHORIZ,SECTOR(6)                                 SGY00330
C                                                                       SGY00340
        TMAX = 9.21                                                     SGY00350
C                                                                       SGY00360
        IF(IY.EQ.0) GO TO 200                                           SGY00370
C                                                                       SGY00380
C       USE TURBULENCE INTENSITY VALUE                                  SGY00390
C                                                                       SGY00400
        SIGYA = XIY * XP/SQRT(1.0 + 0.0001*XP)                          SGY00410
        IF(XP.GT.10000.) SIGYA = XIY * XP/1.4142                        SGY00420
        GO TO 300                                                       SGY00430
C                                                                       SGY00440
C       STABILITY CLASS DETERMINATION OF SIGMA-Y                        SGY00450
C                                                                       SGY00460
C 200   IF(ICOEF.EQ.1) GO TO 250   @ SIGY = A * X**B  + C               IBM00470
  200   IF(ICOEF.EQ.1) GO TO 250                                        IBM00082
C       IF(ICOEF.EQ.2) GO TO 220    @ PASQUILL-GIFFORD                  IBM00480
        IF(ICOEF.EQ.2) GO TO 220                                        IBM00083
C       CALL BRIGGS(IST,XP,SIGYA,SIGZA)   @ BRIGGS RURAL                IBM00490
        CALL BRIGGS(IST,XP,SIGYA,SIGZA)                                 IBM00084
        GO TO 300                                                       SGY00500
  220   CALL PGYZ(XP,IST,SIGYA,SIGZA)                                   SGY00510
        GO TO 300                                                       SGY00520
C                                                                       SGY00530
  250   K=3                                                             SGY00540
        IF(XP.LE.XY2)K=2                                                SGY00550
        IF(XP.LE.XY1)K=1                                                SGY00560
        SIGYA=AY(IST,K)*(XP)**BY(IST,K)+CY(IST,K)                       SGY00570
C                                                                       SGY00580
C       CONSIDER BUOYANCY-ENHANCED DISPERSION                           SGY00590
C                                                                       SGY00600
  300   SIGYB = 0.0                                                     SGY00610
        IF(IBUOY.EQ.0) GO TO 400                                        SGY00620
        SIGYB = RISE/ALPHA                                              SGY00630
C                                                                       SGY00640
C       CONSIDER ENHANCEMENT OF SIGMA-Y BY WIND DIRECTION SHEAR         SGY00650
C                                                                       SGY00660
  400   SIGYC = 0.0                                                     SGY00670
        IF(ISHEAR.EQ.0) GO TO 500                                       SGY00680
        SIGYC = SHCOEF * DTHETA * 0.01745 * RISE * XP                   SGY00690
C                                                                       SGY00700
  500   SIGY=SQRT(SIGYA*SIGYA + SIGYB*SIGYB + SIGYC*SIGYC)              SGY00710
C                                                                       SGY00720
C         COMPUTE HDF                                                   SGY00730
C                                                                       SGY00740
        IF(IHORIZ.EQ.1.OR.(IHORIZ.EQ.3.AND.IST.LT.5)) GO TO 700         SGY00750
C                                                                       SGY00760
C        SECTOR AVERAGING                                               SGY00770
C                                                                       SGY00780
C       SW =  2.0 * XP * TAN(SECTOR(IST)/114.59156) @ SECTOR WIDTH IN M IBM00790
        SW =  2.0 * XP * TAN(SECTOR(IST)/114.59156)                     IBM00085
C       HDF = 1.0/SW   @  UNIFORM HORIZONTAL PROFILE WITHIN SECTOR      IBM00800
        HDF = 1.0/SW                                                    IBM00086
C       IF(ABS(Y).GT.SW/2.0) HDF = 0.0  @ IF OUTSIDE SECTOR, ZERO CONC  IBM00810
        IF(ABS(Y).GT.SW/2.0) HDF = 0.0                                  IBM00087
        RETURN                                                          SGY00820
C                                                                       SGY00830
C       OFF-CENTERLINE                                                  SGY00840
C                                                                       SGY00850
C 700   HDF = 0.3989423/SIGY    @  1/ROOT(2*PI) = .3989423              IBM00860
  700   HDF = 0.3989423/SIGY                                            IBM00088
        ARG=.5*(Y/SIGY)**2                                              SGY00870
        IF (ARG.GT.TMAX) HDF = 0.                                       SGY00880
        IF (ARG.LE.TMAX) HDF = HDF*EXP(-ARG)                            SGY00890
C                                                                       SGY00900
        RETURN                                                          SGY00910
        END                                                             SGY00920

        SUBROUTINE SIGMAZ(IST,XP,RISE,XIZ,SIGZ,SIGZA,SIGZB)             SGZ00010
C                                                                       SGZ00020
C********************************************************************   SGZ00030
C                                                                       SGZ00040
C       THIS ROUTINE COMPUTES SIGMA-Z.                                  SGZ00050
C                                                                       SGZ00060
C       IST     - STABILTY CLASS                                        SGZ00070
C       XP      - DOWNWIND DISTANCE (METERS)                            SGZ00080
C       RISE    - PLUME RISE (METERS)                                   SGZ00090
C       XIZ     - TURBULENCE INTENSITY (Z-COMPONENT)                    SGZ00100
C       SIGZ    - SIGMA-Z                                               SGZ00110
C       SIGZA   - SIGMA-Z, ATMOSPHERIC DISPERSION COMPONENT             SGZ00120
C       SIGZB   - SIGMA-Z, BUOYANCY ENHANCEMENT COMPONENT               SGZ00130
C                                                                       SGZ00140
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          SGZ00150
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          SGZ00160
C                                                                       SGZ00170
C       RTDM     VERSION 3.2     LEVEL 861015                           SGZ00180
C********************************************************************   SGZ00190
C                                                                       SGZ00200
        DIMENSION AZ(6,3),BZ(6,3),CZ(6,3)                               SGZ00210
        COMMON /PR006/ ICOEF                                            SGZ00220
        COMMON /PR008/ AZ, BZ, CZ, XZ1,XZ2                              SGZ00230
        COMMON /PR010/ IBUOY,ALPHA                                      SGZ00240
        COMMON /PR017/ IZ                                               SGZ00250
C                                                                       SGZ00260
C                                                                       SGZ00270
        IF(IZ.EQ.0) GO TO 1200                                          SGZ00280
C                                                                       SGZ00290
C       USE TURBULENCE INTENSITY VALUE                                  SGZ00300
C                                                                       SGZ00310
        IF(IST.LE.3) SIGZA = XIZ * XP                                   SGZ00320
        IF(IST.EQ.4) SIGZA = XIZ * XP/SQRT(1.0 + 0.0015*XP)             SGZ00330
        IF(IST.GT.4) SIGZA = XIZ * XP/(1.0 + 0.0003*XP)                 SGZ00340
        GO TO 1300                                                      SGZ00350
C                                                                       SGZ00360
C       STABILITY CLASS DETERMINATION OF SIGMA-Z                        SGZ00370
C                                                                       SGZ00380
C1200   IF(ICOEF.EQ.1) GO TO 1250   @ SIGZ = A * X**B  + C              IBM00390
 1200   IF(ICOEF.EQ.1) GO TO 1250                                       IBM00089
C       IF(ICOEF.EQ.2) GO TO 1220    @ PASQUILL-GIFFORD                 IBM00400
        IF(ICOEF.EQ.2) GO TO 1220                                       IBM00090
C       CALL BRIGGS(IST,XP,SIGYA,SIGZA)   @ BRIGGS RURAL                IBM00410
        CALL BRIGGS(IST,XP,SIGYA,SIGZA)                                 IBM00091
        GO TO 1300                                                      SGZ00420
 1220   CALL PGYZ(XP,IST,SIGYA,SIGZA)                                   SGZ00430
        GO TO 1300                                                      SGZ00440
C                                                                       SGZ00450
 1250   K=3                                                             SGZ00460
        IF(XP.LE.XZ2)K=2                                                SGZ00470
        IF(XP.LE.XZ1)K=1                                                SGZ00480
        SIGZA=AZ(IST,K)*(XP)**BZ(IST,K)+CZ(IST,K)                       SGZ00490
C                                                                       SGZ00500
C       CONSIDER BUOYANCY-ENHANCED DISPERSION                           SGZ00510
C                                                                       SGZ00520
 1300   SIGZB = 0.0                                                     SGZ00530
        IF(IBUOY.EQ.0) GO TO 1400                                       SGZ00540
        SIGZB = RISE/ALPHA                                              SGZ00550
C                                                                       SGZ00560
 1400   SIGZ=SQRT(SIGZA*SIGZA + SIGZB*SIGZB)                            SGZ00570
        RETURN                                                          SGZ00580
        END                                                             SGZ00590

        SUBROUTINE BRIGGS(IST,XP,SIGYA,SIGZA)                           BRG00010
C                                                                       BRG00020
C********************************************************************   BRG00030
C                                                                       BRG00040
C       THIS ROUTINE COMPUTES SIGMA-Y AND SIGMA-Z (ATMOSPHERIC          BRG00050
C       DISPERSION COMPONENTS) USING BRIGGS RURAL FORMULAS (1974)       BRG00060
C                                                                       BRG00070
C       IST     - STABILTY CLASS                                        BRG00080
C       XP      - DOWNWIND DISTANCE (METERS)                            BRG00090
C       SIGYA   - SIGMA-Y, ATMOSPHERIC DISPERSION COMPONENT             BRG00100
C       SIGZA   - SIGMA-Z, ATMOSPHERIC DISPERSION COMPONENT             BRG00110
C                                                                       BRG00120
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          BRG00130
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          BRG00140
C                                                                       BRG00150
C       RTDM     VERSION 3.2     LEVEL 861015                           BRG00160
C********************************************************************   BRG00170
C                                                                       BRG00180
        GO TO (1000,2000,3000,4000,5000,6000), IST                      BRG00190
C                                                                       BRG00200
 1000   SIGYA = 0.22 * XP/SQRT(1.0 + 0.0001*XP)                         BRG00210
        SIGZA = 0.20 * XP                                               BRG00220
        RETURN                                                          BRG00230
C                                                                       BRG00240
 2000   SIGYA = 0.16 * XP/SQRT(1.0 + 0.0001*XP)                         BRG00250
        SIGZA = 0.12 * XP                                               BRG00260
        RETURN                                                          BRG00270
C                                                                       BRG00280
 3000   SIGYA = 0.11 * XP/SQRT(1.0 + 0.0001*XP)                         BRG00290
        SIGZA = 0.08 * XP/SQRT(1.0 + 0.0002*XP)                         BRG00300
        RETURN                                                          BRG00310
C                                                                       BRG00320
 4000   SIGYA = 0.08 * XP/SQRT(1.0 + 0.0001*XP)                         BRG00330
        SIGZA = 0.06 * XP/SQRT(1.0 + 0.0015*XP)                         BRG00340
        RETURN                                                          BRG00350
C                                                                       BRG00360
 5000   SIGYA = 0.06 * XP/SQRT(1.0 + 0.0001*XP)                         BRG00370
        SIGZA = 0.03 * XP/(1.0 + 0.0003*XP)                             BRG00380
        RETURN                                                          BRG00390
C                                                                       BRG00400
 6000   SIGYA = 0.04 * XP/SQRT(1.0 + 0.0001*XP)                         BRG00410
        SIGZA = 0.016 * XP/(1.0 + 0.0003*XP)                            BRG00420
        RETURN                                                          BRG00430
        END                                                             BRG00440

C                                                                       PGT00010
      SUBROUTINE PGYZ(XMETER,KST,SY,SZ)                                 PGT00020
C                                                                       PGT00030
C********************************************************************   PGT00040
C                                                                       PGT00050
C      THIS SUBROUTINE IS PART OF THE EPA MPTER MODEL CODE.             PGT00060
C                                                                       PGT00070
C        VERTICAL DISPERSION PARAMETER VALUE, SZ DETERMINED BY          PGT00080
C         SZ = A * X ** B WHERE A AND B ARE FUNCTIONS OF BOTH STABILITY PGT00090
C         AND RANGE OF X.                                               PGT00100
C                                                                       PGT00110
C       X = XMETER/1000.                                                PGT00120
C       KST = STABILITY CLASS                                           PGT00130
C                                                                       PGT00140
C        HORIZONTAL DISPERSION PARAMETER VALUE, SY DETERMINED BY        PGT00150
C         LOGARITHMIC INTERPOLATION OF PLUME HALF-ANGLE ACCORDING TO    PGT00160
C         DISTANCE AND CALCULATION OF 1/2.15 TIMES HALF-ARC LENGTH.     PGT00170
C                                                                       PGT00180
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          PGT00190
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          PGT00200
C                                                                       PGT00210
C       RTDM      VERSION 3.2     LEVEL 861015                          PGT00220
C                                                                       PGT00230
C********************************************************************   PGT00240
C                                                                       PGT00250
      DIMENSION XA(7), XB(2), XD(5), XE(8), XF(9), AA(8), BA(8), AB(3), PGT00260
     1BB(3), AD(6), BD(6), AE(9), BE(9), AF(10), BF(10)                 PGT00270
      DATA XA /.5,.4,.3,.25,.2,.15,.1/                                  PGT00280
      DATA XB /.4,.2/                                                   PGT00290
      DATA XD /30.,10.,3.,1.,.3/                                        PGT00300
      DATA XE /40.,20.,10.,4.,2.,1.,.3,.1/                              PGT00310
      DATA XF /60.,30.,15.,7.,3.,2.,1.,.7,.2/                           PGT00320
      DATA AA /453.85,346.75,258.89,217.41,179.52,170.22,158.08,122.8/  PGT00330
      DATA BA /2.1166,1.7283,1.4094,1.2644,1.1262,1.0932,1.0542,.9447/  PGT00340
      DATA AB /109.30,98.483,90.673/                                    PGT00350
      DATA BB /1.0971,0.98332,0.93198/                                  PGT00360
      DATA AD /44.053,36.650,33.504,32.093,32.093,34.459/               PGT00370
      DATA BD /0.51179,0.56589,0.60486,0.64403,0.81066,0.86974/         PGT00380
      DATA AE /47.618,35.420,26.970,24.703,22.534,21.628,21.628,23.331, PGT00390
     X 24.26/                                                           PGT00400
      DATA BE /0.29592,0.37615,0.46713,0.50527,0.57154,0.63077,0.75660, PGT00410
     X  0.81956,0.8366/                                                 PGT00420
      DATA AF /34.219,27.074,22.651,17.836,16.187,14.823,13.953,13.953, PGT00430
     X  14.457,15.209/                                                  PGT00440
      DATA BF /0.21716,0.27436,0.32681,0.41507,0.46490,0.54503,0.63227, PGT00450
     X  0.68465,0.78407,0.81558/                                        PGT00460
        X = XMETER/1000.                                                PGT00470
      XY=X                                                              PGT00480
      GO TO (10,40,70,80,110,140), KST                                  PGT00490
C        STABILITY A                                                    PGT00500
   10 TH=(24.167-2.5334*ALOG(XY))/57.2958                               PGT00510
      IF (X.GT.3.11) GO TO 170                                          PGT00520
      DO 20 ID=1,7                                                      PGT00530
      IF (X.GE.XA(ID)) GO TO 30                                         PGT00540
   20 CONTINUE                                                          PGT00550
      ID=8                                                              PGT00560
   30 SZ=AA(ID)*X**BA(ID)                                               PGT00570
      GO TO 190                                                         PGT00580
C        STABILITY B                                                    PGT00590
   40 TH=(18.333-1.8096*ALOG(XY))/57.2958                               PGT00600
      IF (X.GT.35.) GO TO 170                                           PGT00610
      DO 50 ID=1,2                                                      PGT00620
      IF (X.GE.XB(ID)) GO TO 60                                         PGT00630
   50 CONTINUE                                                          PGT00640
      ID=3                                                              PGT00650
   60 SZ=AB(ID)*X**BB(ID)                                               PGT00660
      GO TO 180                                                         PGT00670
C        STABILITY C                                                    PGT00680
   70 TH=(12.5-1.0857*ALOG(XY))/57.2958                                 PGT00690
      SZ=61.141*X**0.91465                                              PGT00700
      GO TO 180                                                         PGT00710
C        STABILITY D                                                    PGT00720
   80 TH=(8.3333-0.72382*ALOG(XY))/57.2958                              PGT00730
      DO 90 ID=1,5                                                      PGT00740
      IF (X.GE.XD(ID)) GO TO 100                                        PGT00750
   90 CONTINUE                                                          PGT00760
      ID=6                                                              PGT00770
  100 SZ=AD(ID)*X**BD(ID)                                               PGT00780
      GO TO 180                                                         PGT00790
C        STABILITY E                                                    PGT00800
  110 TH=(6.25-0.54287*ALOG(XY))/57.2958                                PGT00810
      DO 120 ID=1,8                                                     PGT00820
      IF (X.GE.XE(ID)) GO TO 130                                        PGT00830
  120 CONTINUE                                                          PGT00840
      ID=9                                                              PGT00850
  130 SZ=AE(ID)*X**BE(ID)                                               PGT00860
      GO TO 180                                                         PGT00870
C        STABILITY F                                                    PGT00880
  140 TH=(4.1667-0.36191*ALOG(XY))/57.2958                              PGT00890
      DO 150 ID=1,9                                                     PGT00900
      IF (X.GE.XF(ID)) GO TO 160                                        PGT00910
  150 CONTINUE                                                          PGT00920
      ID=10                                                             PGT00930
  160 SZ=AF(ID)*X**BF(ID)                                               PGT00940
      GO TO 180                                                         PGT00950
  170 SZ=5000.                                                          PGT00960
      GO TO 190                                                         PGT00970
  180 IF (SZ.GT.5000.) SZ=5000.                                         PGT00980
  190 SY=465.116*XY*SIN(TH)/COS(TH)                                     PGT00990
C        465.116 = 1000. (M/KM) / 2.15                                  PGT01000
      RETURN                                                            PGT01010
C                                                                       PGT01020
      END                                                               PGT01030

        SUBROUTINE VERT(H,THT,XP,RISE,UTOP,SIGZ,HCRIT,HPLUME,V,         VRT00010
     X  RVDF,R)                                                         VRT00020
C                                                                       VRT00030
C*****************************************************************      VRT00040
C                                                                       VRT00050
C       THIS ROUTINE COMPUTES THE VERTICAL DISTRIBUTION FUNCTION.       VRT00060
C       THIS CALCULATION INVOLVES COMPUTATION OF THE MODIFIED PLUME     VRT00070
C       HEIGHT AS IT IS AFFECTED BY TERRAIN.  THIS EFFECT IS INFLUENCED VRT00080
C       BY THE VALUE OF HCRIT, WHICH DEPENDS UPON THE HILL HEIGHT, THE  VRT00090
C       WIND SPEED AND THE VERTICAL POTENTIAL TEMPERATURE GRADIENT.     VRT00100
C                                                                       VRT00110
C       A KEY ASPECT OF THIS ROUTINE IS THE CALCULATION OF MAXIMUM      VRT00120
C       CROSSWIND INTEGRATED (MCWI) CONCENTRATION PROFILES AT SEVERAL   VRT00130
C       DOWNWIND DISTANCES, STARTING WHERE REFLECTION FROM THE GROUND ISVRT00140
C       UNIMPORTANT AND ENDING AT THE RECEPTOR LOCATION.  ACCORDING TO  VRT00150
C       THE SECOND LAW OF THERMODYNAMICS, THE MCWI VALUE MUST DECREASE  VRT00160
C       WITH INCREASING DOWNWIND DISTANCE.  IN THIS ROUTINE, THE MINIMUMVRT00170
C       VALUE OF THE MCWI CONCENTRATION IS "FROZEN" FOR USE IN THE      VRT00180
C       CALCULATION OF THE VERTICAL DISTRIBUTION FUNCTION PORTION OF THEVRT00190
C       GAUSSIAN FORMULA.                                               VRT00200
C                                                                       VRT00210
C       H       - PLUME HEIGHT                                          VRT00220
C       THT     - GROUND HT (ABOVE STACK BASE) AT RECEPTOR              VRT00230
C       XP      - DOWNWIND DISTANCE (METERS)                            VRT00240
C       RISE    - PLUME RISE                                            VRT00250
C       UTOP    - WIND SPEED AT STACK TOP                               VRT00260
C       SIGZ    - SIGMA-Z AT RECEPTOR LOCATION                          VRT00270
C       HCRIT   - CRITICAL HEIGHT VALUE, BELOW WHICH THE PLUME WILL     VRT00280
C                 IMPINGE A TERRAIN OBSTACLE                            VRT00290
C       HPLUME  - FINAL PLUME HEIGHT AFTER TERRAIN ADJUSTMENTS          VRT00300
C       V       - VERTICAL DISTRIBUTION FUNCTION (=1.0/(SQRT(2*PI)*SIGZ)VRT00310
C                 SERIES OF REFLECTION TERMS IN Z)                      VRT00320
C       RVDF    - VERTICAL DISTRIBUTION FUNCTION WITH FROZEN R          VRT00330
C       R       - FROZEN REFLECTION FACTOR                              VRT00340
C                                                                       VRT00350
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          VRT00360
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          VRT00370
C                                                                       VRT00380
C       RTDM      VERSION 3.2     LEVEL 861015                          VRT00390
C********************************************************************   VRT00400
C                                                                       VRT00410
C******************************************************                 VRT00420
C                                                                       VRT00430
C      COMMON BLOCK FOR STACK SPECIFIC DATA                             VRT00440
C                                                                       VRT00450
C******************************************************                 VRT00460
C      COMMON /STACKS/                                                  IBM00470
C    X                 SNAME(35),   @STACK NAMES                        IBM00480
C    X                 HEIGHT(35),  @STACK HEIGHT (M)                   IBM00490
C    X                 DIAMTR(35),  @STACK DIAMETER (M)                 IBM00500
C    X                 EXITVL(35),  @STACK EXIT VELOCITY (M/SEC)        IBM00510
C    X                 STEMP(35),   @STACK GAS EXIT TEMP (DEG K)        IBM00520
C    X                 EMISR(35),   @EMISSION RATE  (G/SEC)             IBM00530
C    X                 UTMX,        @X-COORDINATE FOR ALL STACKS        IBM00540
C    X                 UTMY,        @Y-COORDINATE FOR ALL STACKS        IBM00550
C    X                 BASEHT,      @BASE HEIGHT FOR ALL STACKS         IBM00560
C    X                 NS,          @NUMBER OF STACKS                   IBM00570
C    X                 EMSLBL       @LABEL FOR POLLUTANT                IBM00580
       CHARACTER*4  SNAME(35)                                           IBM00092
       COMMON /STACKS/ HEIGHT(35),DIAMTR(35),EXITVL(35),STEMP(35),      IBM00093
     X                 EMISR(35),UTMX,UTMY,BASEHT,NS,EMSLBL             IBM00094
       COMMON /STACK2/ SNAME                                            IBM00095
C                                                                       CON00310
C                                                                       VRT00590
C******************************************************                 VRT00600
C                                                                       VRT00610
C      COMMON BLOCK FOR TERRAIN DESCRIPTION DATA                        VRT00620
C                                                                       VRT00630
C******************************************************                 VRT00640
C      COMMON /TERR/                                                    IBM00650
C    X                 BEGHT,       @BEGINNING CONTOUR HEIGHT, USER UNITIBM00660
C    X                 HTINCR,      @CONTOUR INCREMENT, USER UNITS      IBM00670
C    X                 CONTUR(36,21)@DISTANCES FROM THE SOURCE ALONG EACIBM00680
C    X                              @10-DEG RADIAL FOR HEIGHTS STARTING IBM00690
C    X                              @AT BEGHT, IN INCREMENTS OF HTINCR--IBM00700
C    X                              @UP TO 20 DISTANCES (CONTOUR INCREMEIBM00710
C    X                              @ARE ALLOWED ALONG EACH RADIAL      IBM00720
       COMMON /TERR/ BEGHT,HTINCR,CONTUR(36,21)                         IBM00096
C                                                                       VRT00730
C******************************************************                 VRT00740
C                                                                       VRT00750
C      COMMON BLOCK FOR HOURLY METEOROLOGICAL DATA                      VRT00760
C                                                                       VRT00770
C******************************************************                 VRT00780
C      COMMON /MET/                                                     IBM00790
C    X                 WDIR,        @ WIND DIRECTION                    IBM00800
C    X                 WSPD,        @ WIND SPEED #1 (M/SEC)             IBM00810
C    X                 DMX,         @ MIXING HEIGHT (M)                 IBM00820
C    X                 IST,         @ STABILITY CLASS                   IBM00830
C    X                 TEMP,        @ AMBIENT TEMPERATURE (DEG K)       IBM00840
C    X                 XIY,         @ TURBULENCE, Y-COMPONENT           IBM00850
C    X                 XIZ,         @ TURBULENCE, Z-COMPONENT           IBM00860
C    X                 PRVPTG,      @ PLUME RISE VPTG                   IBM00870
C    X                 HCVPTG,      @ HCRIT VPTG                        IBM00880
C    X                 DTHETA,      @ HORIZONTAL WIND DIRECTION SHEAR   IBM00890
C    X                 EPS,         @ WIND SPEED PROFILE EXPONENT       IBM00900
C    X                 WSPD2        @ WIND SPEED #2 (M/SEC)             IBM00910
       COMMON/MET/ WDIR,WSPD,DMX,IST,TEMP,XIY,XIZ,PRVPTG,HCVPTG,        IBM00097
     X             DTHETA,EPS,WSPD2                                     IBM00098
C                                                                       VRT00920
        COMMON /PR002/ VSCALE                                           VRT00930
        COMMON /PR009/ IPPP,PPVPTG                                      VRT00940
        COMMON /PR013/ TERCOR(6)                                        VRT00950
        COMMON /PR022/ IREFL                                            VRT00960
        COMMON /PR025/ IVERB                                            VRT00970
C                                                                       VRT00980
        REAL MCWI                                                       VRT00990
C                                                                       VRT01000
C       CORRECT PLUME HEIGHT FOR TERRAIN                                VRT01010
C                                                                       VRT01020
C       IF(IPPP.EQ.1.AND.H.GT.DMX) H = DMX @ FOR PARTIAL PENETRATION    IBM01030
        IF(IPPP.EQ.1.AND.H.GT.DMX) H = DMX                              IBM00099
C                                                                       VRT01040
C       CALCULATE FROUDE # TO DETERMINE WHETHER PLUME FLOWS OVER TERRAINVRT01050
C       OR IMPINGES IN STABLE CONDITIONS.  IMPACTION DOES NOT OCCUR IN  VRT01060
C       NEUTRAL OR UNSTABLE CONDITIONS.                                 VRT01070
C                                                                       VRT01080
        IDIR = (INT(WDIR + 4.99))/10                                    VRT01090
        IF(IDIR.EQ.0) IDIR = 36                                         VRT01100
        INDEX = INT(CONTUR(IDIR,21))                                    VRT01110
C                                                                       VRT01120
        FROUDE = 1.0                                                    VRT01130
        HCRIT = 0.0                                                     VRT01140
        IF(IST.LE.4) GO TO 800                                          VRT01150
C                                                                       VRT01160
C       CALCULATE FROUDE # FOR STABLE CONDITIONS                        VRT01170
C                                                                       VRT01180
        HILLTP = (BEGHT + (INDEX-1)*HTINCR) * VSCALE - BASEHT           VRT01190
        ESS = (9.806/TEMP) * HCVPTG                                     VRT01200
        IF(ESS.LE.0.0) GO TO 800                                        VRT01210
        FROUDE = UTOP/(HILLTP*SQRT(ESS))                                VRT01220
        IF(FROUDE.GT.1.0) FROUDE = 1.0                                  VRT01230
        HCRIT = HILLTP * (1.0 - FROUDE)                                 VRT01240
        IF(H.GE.HCRIT) GO TO 800                                        VRT01250
C       TERC = 0.0    @ POSSIBLE IMPINGEMENT                            IBM01260
        TERC = 0.0                                                      IBM00100
        GO TO 1000                                                      VRT01270
  800   TERC = TERCOR(IST)                                              VRT01280
C                                                                       VRT01290
C       HPC IS THE PLUME HEIGHT ABOVE HCRIT; HTC IS THE TERRAIN HEIGHT  VRT01300
C       ABOVE HCRIT                                                     VRT01310
C                                                                       VRT01320
 1000   HPC = H - HCRIT                                                 VRT01330
        HTC = THT - HCRIT                                               VRT01340
C       HPLUME = HPC*TERC    @  FOR TERRAIN ABOVE INITIAL PLUME HEIGHT  IBM01350
        HPLUME = HPC*TERC                                               IBM00101
        IF(THT.LT.H) HPLUME = HPC - (1.0-TERC)*HTC                      VRT01360
C       IF(TERC.LT.0.00001.AND.H.LT.THT) HPLUME = H - THT @ NEGATIVE PLUIBM01370
        IF(TERC.LT.0.00001.AND.H.LT.THT) HPLUME = H - THT               IBM00102
C       IF(HTC.LT.0.0) HPLUME = H - THT  @  LEVEL PLUME, RECEP BELOW HC IBM01375
        IF(HTC.LT.0.0) HPLUME = H - THT                                 IBM00103
C                                                                       VRT01380
C       HPLUME IS THE HEIGHT OF THE PLUME ABOVE THE RECEPTOR (ASSUMED TOVRT01390
C       BE AT THE GROUND).                                              VRT01400
C                                                                       VRT01410
C       DETERMINE CONVENTIONAL VALUE OF VERTICAL DISTRIBUTION FUNCTION. VRT01420
C       VDF INCORPORATES MULTIPLE REFLECTIONS AS DESCRIBED IN TURNER'S  VRT01430
C       WORKBOOK (1970), PAGE 36.                                       VRT01440
C                                                                       VRT01450
C       ADJUST MIXING HEIGHT FOR TERRAIN CORRECTION TO BE CONSISTENT WITVRT01460
C       PLUMES THAT CLEAR THE TERRAIN                                   VRT01470
C                                                                       VRT01480
        DMXC = (DMX-HCRIT)*TERC                                         VRT01490
        IF(THT.LT.DMX) DMXC = (DMX-HCRIT) - (1.0-TERC)*HTC              VRT01500
C                                                                       VRT01510
        CALL VDF(HPLUME,DMXC,SIGZ,V)                                    VRT01520
C                                                                       VRT01530
C       DETERMINE WHETHER DETAILED EXAMINATION OF MCWI CONCENTRATIONS   VRT01540
C       UPWIND OF THE RECEPTOR ARE NECESSARY.  IF SIGMA-Z *2.15 IS LESS VRT01550
C       THAN THE PLUME HEIGHT ABOVE TERRAIN, REFLECTION EFFECTS ARE     VRT01560
C       NEGLIGIBLE.                                                     VRT01570
C                                                                       VRT01580
        IF(IREFL.EQ.0.OR.HPLUME.GT.2.15*SIGZ) GO TO 5000                VRT01590
C                                                                       VRT01600
C       CONSIDER THE EFFECTS OF THE SECOND LAW OF THERMODYNAMICS.  FIND VRT01610
C       A TERRAIN POINT UPWIND OF THE RECEPTOR WHERE SIGMA-Z * 2.15 IS  VRT01620
C       GREATER THAN HPLUME, THEN BACK UP ONE TERRAIN INCREMENT.        VRT01630
C                                                                       VRT01640
        IF(INDEX.LT.2) GO TO 5000                                       VRT01650
        X1 = 0.0                                                        VRT01655
        DO 1500 I=1,INDEX                                               VRT01660
        TERRHT = (BEGHT + (I-1)*HTINCR) * VSCALE - BASEHT               VRT01670
        X2 = CONTUR(IDIR,I) * 1000.                                     VRT01671
        X = X2                                                          VRT01672
C                                                                       VRT01673
C       IF PAST RECEPTOR POINT, CHECK HPL AND SZ AT RECEPTOR POINT      VRT01674
C                                                                       VRT01675
       IF(.NOT.(X2.GT.XP .AND. X1.LT.XP)) GO TO 1300                    VRT01676
       X = XP - 0.1                                                     VRT01677
       TERRHT = THT - 0.1                                               VRT01678
1300    CONTINUE                                                        VRT01679
        HPL = HPC * TERC                                                VRT01680
       HTC = TERRHT - HCRIT                                             VRT01682
        IF(TERRHT.LT.H) HPL = HPC - (1.0-TERC)*HTC                      VRT01684
C       IF(TERC.LT.0.00001.AND.H.LT.TERRHT) HPL = H - TERRHT @ BELOW RECIBM01686
        IF(TERC.LT.0.00001.AND.H.LT.TERRHT) HPL = H - TERRHT            IBM00104
C       IF(HTC.LT.0.0) HPL = H - TERRHT @  LEVEL PLUME, RECEP BELOW HC  IBM01688
        IF(HTC.LT.0.0) HPL = H - TERRHT                                 IBM00105
C       IF(TERRHT.LT.H) HPL = HPC - (1.0-TERC)*(TERRHT-HCRIT)           VRT01690
C       IF(TERC.LT.0.00001.AND.H.LT.TERRHT) HPL = H - TERRHT @ NEGATIVE IBM01700
C       IF(TERC.LT.0.00001.AND.H.LT.TERRHT) HPL = H - TERRHT            IBM00106
C       X = CONTUR(IDIR,I) * 1000.                                      VRT01710
C       IF(HPL.LT.0.0) GO TO 5000  @ DO NOT CONSIDER POINTS ABOVE PLUME IBM01720
        IF(HPL.LT.0.0) GO TO 5000                                       IBM00107
C       IF(X.GE.XP.OR.TERRHT.GT.THT) GO TO 5000  @ NO MORE UPWIND POINTSIBM01730
        IF(X.GE.XP.OR.TERRHT.GT.THT) GO TO 5000                         IBM00108
C                                                                       VRT01740
C       COMPUTE SIGMA-Z                                                 VRT01750
C                                                                       VRT01760
        CALL SIGMAZ(IST,X,RISE,XIZ,SZ,SZA,SZB)                          VRT01770
C       IF(HPL.LT.2.15*SZ) GO TO 1700   @ BEGIN REFLECTION CALCULATIONS IBM01780
        IF(HPL.LT.2.15*SZ) GO TO 1700                                   IBM00109
C       SIGZ1 = SZ   @ SAVE FOR REFLECTION CALCULATIONS                 IBM01790
        SIGZ1 = SZ                                                      IBM00110
       X1 = X2                                                          VRT01795
 1500   CONTINUE                                                        VRT01800
C       GO TO 5000    @ NO UPWIND POINTS OF SIGNIFICANCE                IBM01810
        GO TO 5000                                                      IBM00111
C                                                                       VRT01820
 1700   IF(I.EQ.1) GO TO 1750                                           VRT01830
C       I = I - 1     @  BACK UP ONE TERRAIN INCREMENT                  IBM01840
        I = I - 1                                                       IBM00112
        SZ = SIGZ1                                                      VRT01850
        GO TO 1800                                                      VRT01860
C                                                                       VRT01870
C       AT THIS POINT, PLUME HEIGHT IS AT LEAST 2 STANDARD DEVIATIONS   VRT01880
C       AWAY FROM THE SURFACE, AND TERRAIN REFLECTION EFFECTS ARE       VRT01890
C       NEGLIGIBLE.  DENOTE SIGMA-Z AT THIS POINT AS SIGZ1.  CALCULATE  VRT01900
C       MCWI HERE (NO TERRAIN EFFECTS) AND THEN INCREMENT THE HEIGHT    VRT01910
C       OF THE TERRAIN IN STEPS OF DELTAZ UNTIL THE ELEVATION OF THE    VRT01920
C       RECEPTOR POINT IS ATTAINED OR THE PLUME IMPINGES UPON TERRAIN.  VRT01930
C       DELTAZ IS THE MAXIMUM OF 0.2*SIGZ1 AND (THT-TERRHT)/10  .       VRT01940
C                                                                       VRT01950
 1750   SIGZ1 = SZ                                                      VRT01960
 1800   TERRHT = (BEGHT + (I-1) * HTINCR) * VSCALE - BASEHT             VRT01970
        HPL = HPC * TERC                                                VRT01980
        IF(TERRHT.LT.H) HPL = HPC - (1.0-TERC)*(TERRHT-HCRIT)           VRT01990
C       IF(TERC.LT.0.00001.AND.H.LT.TERRHT) HPL = H - TERRHT @ NEGATIVE IBM02000
        IF(TERC.LT.0.00001.AND.H.LT.TERRHT) HPL = H - TERRHT            IBM00113
        DMXC = (DMX-HCRIT)*TERC                                         VRT02010
        IF(TERRHT.LT.DMX) DMXC = (DMX-HCRIT) - (1.0-TERC)*(TERRHT-HCRIT)VRT02020
        X = CONTUR(IDIR,I) * 1000.                                      VRT02030
        DELTAZ = AMAX1(SIGZ1/5.0,(THT-TERRHT)/10.+0.1)                  VRT02040
C                                                                       VRT02050
C       THE NEXT LOOP DETERMINES THE MCWI AT A FIXED DOWNWIND DISTANCE BVRT02060
C       CALCULATING THE CWI VALUE AT SEVERAL POINTS IN THE HEIGHT INTERVVRT02070
C       BETWEEN THE PLUME LEVEL AND THE GROUND.  THIS HEIGHT INTERVAL ISVRT02080
C       DIVIDED INTO 5 EQUAL SECTIONS, AND THE CWI IS EVALUATED AT EACH VRT02090
C       INTERVAL BETWEEN PLUME CENTERLINE AND THE GROUND, A POSSIBLE TOTVRT02100
C       OF 6 POINTS.  IN FLAT TERRAIN, THE MAXIMUM CWI OCCURS AT OR NEARVRT02110
C       THE PLUME CENTERLINE, AND MOVES CLOSER TO THE GROUND SURFACE AS VRT02120
C       THE TERRAIN RISES TO MEET THE PLUME.  IN THE LOOP BELOW, THE CWIVRT02130
C       IS FIRST EVALUATED AT PLUME HEIGHT, THEN AT SUCCESSIVE HEIGHTS  VRT02140
C       BELOW IT.  IF THE CWI DECREASES FROM THE VALUE AT THE HEIGHT    VRT02150
C       ABOVE IT, THE SEARCH FOR THE MAXIMUM CWI STOPS.                 VRT02160
C                                                                       VRT02170
C       IFINAL = 0    @  SET TO 1 IF MCWI AT RECEPTOR POINT IS EVALUATEDIBM02180
        IFINAL = 0                                                      IBM00114
        K = 0                                                           VRT02190
C       INIT = 1  @   START LOOKING AT CWI AT THIS HEIGHT               IBM02200
        INIT = 1                                                        IBM00115
 2000   MCWI = 0.0                                                      VRT02210
        DO 2500 J = INIT,6                                              VRT02220
        Z = HPL * (6-J)/5.                                              VRT02230
        CALL VDF(HPL-Z,DMXC-Z,SZ,V1)                                    VRT02240
        CALL VDF(HPL+Z,DMXC+Z,SZ,V2)                                    VRT02250
        CWI = (V1 + V2)/2.0                                             VRT02260
C       IF(CWI.LE.MCWI) GO TO 2600    @ HAVE FOUND MCWI AT THIS X       IBM02270
        IF(CWI.LE.MCWI) GO TO 2600                                      IBM00116
        MCWI = CWI                                                      VRT02280
        ZOLD = Z                                                        VRT02290
 2500   CONTINUE                                                        VRT02300
C2600   INIT = MIN(J-2,6)  @ RESET INIT                                 IBM02310
 2600   INIT = MIN(J-2,6)                                               IBM00117
        IF(INIT.LT.1) INIT = 1                                          VRT02320
        IF(K.EQ.0) GO TO 2800                                           VRT02330
        IF(CWIMIN.LT.MCWI) GO TO 2900                                   VRT02340
C2800   CWIMIN = MCWI    @ KEEP TRACK OF MINIMUM MCWI                   IBM02350
 2800   CWIMIN = MCWI                                                   IBM00118
        XMAX = X                                                        VRT02360
        ZMAX = ZOLD                                                     VRT02370
        R = MCWI * 2.506628 * SZ                                        VRT02380
C                                                                       VRT02390
C       INCREMENT TERRAIN HEIGHT BY SIGZ1/5 AND FIND ASSOCIATED         VRT02400
C       DOWNWIND DISTANCE                                               VRT02410
C                                                                       VRT02420
C2900   IF(IFINAL.EQ.1) GO TO 4000    @ FINISHED                        IBM02430
 2900   IF(IFINAL.EQ.1) GO TO 4000                                      IBM00119
C       TERRHT = TERRHT + DELTAZ    @  INCREMENT TERRAIN                IBM02440
        TERRHT = TERRHT + DELTAZ                                        IBM00120
C       IF(TERRHT.GT.THT) GO TO 3000    @ EVALUATE MCWI AT RECEPTOR, THEIBM02450
        IF(TERRHT.GT.THT) GO TO 3000                                    IBM00121
        CALL DOWNWD(IDIR,TERRHT,INDEX,BASEHT,X)                         VRT02460
C       IF(X.GT.999998. .OR. X.GT.XP) GO TO 3000   @ CHECK RECEPTOR MCWIIBM02470
        IF(X.GT.999998. .OR. X.GT.XP) GO TO 3000                        IBM00122
C                                                                       VRT02480
C       COMPUTE SIGMA-Z AT NEW LOCATION                                 VRT02490
C                                                                       VRT02500
        CALL SIGMAZ(IST,X,RISE,XIZ,SZ,SZA,SZB)                          VRT02510
        HPL = HPC * TERC                                                VRT02520
        IF(TERRHT.LT.H) HPL = HPC - (1.0-TERC)*(TERRHT-HCRIT)           VRT02530
C       IF(TERC.LT.0.00001.AND.H.LT.TERRHT) HPL = H - TERRHT @ NEGATIVE IBM02540
        IF(TERC.LT.0.00001.AND.H.LT.TERRHT) HPL = H - TERRHT            IBM00123
        DMXC = (DMX-HCRIT)*TERC                                         VRT02550
        IF(TERRHT.LT.DMX) DMXC = (DMX-HCRIT) - (1.0-TERC)*(TERRHT-HCRIT)VRT02560
C       IF(HPL.LT.0.001) GO TO 3000 @ CHECK MCWI AT RECEPTOR POINT      IBM02570
        IF(HPL.LT.0.001) GO TO 3000                                     IBM00124
        K = K + 1                                                       VRT02580
        GO TO 2000                                                      VRT02590
C                                                                       VRT02600
C3000   HPL = HPLUME     @ EVALUATE MCWI AT RECEPTOR POINT              IBM02610
 3000   HPL = HPLUME                                                    IBM00125
        IF(HPL.LT.0.0) GO TO 4000                                       VRT02620
        SZ = SIGZ                                                       VRT02630
        X = XP                                                          VRT02640
        TERRHT = THT                                                    VRT02650
        IFINAL = 1                                                      VRT02660
        K = K + 1
        GO TO 2000                                                      VRT02670
C                                                                       VRT02680
C       END MCWI LOOP                                                   VRT02690
C                                                                       VRT02700
 4000   RVDF = R/(2.506628 * SIGZ)                                      VRT02710
        RETURN                                                          VRT02720
C                                                                       VRT02730
C       IF NO REFLECTION FOUND, SET R TO 1.00.                          VRT02740
C       IF NO RELECTION CONSIDERED, SET CWIMIN TO V IF R>=1,            VRT02750
C           ELSE SET R TO 1 AND COMPUTE CWIMIN                          VRT02760
C                                                                       VRT02770
 5000   IF(IREFL.EQ.0) GO TO 6000                                       VRT02780
C                                                                       VRT02790
 5500   R = 1.00                                                        VRT02800
        RVDF = R/(2.506628*SIGZ)                                        VRT02810
        CWIMIN = 1.0/(2.506628 * SIGZ)                                  VRT02820
        RETURN                                                          VRT02830
C                                                                       VRT02840
 6000   RVDF = V                                                        VRT02850
        R = RVDF * 2.506628 * SIGZ                                      VRT02860
C       IF(R.LT.1.0) GO TO 5500   @ SET R TO AT LEAST 1.0               IBM02870
        IF(R.LT.1.0) GO TO 5500                                         IBM00126
        RETURN                                                          VRT02880
C                                                                       VRT02890
        END                                                             VRT02900

        SUBROUTINE DOWNWD(IDIR,TERRHT,INDEX,BASEHT,X)                   DWN00010
C                                                                       DWN00020
C********************************************************************   DWN00030
C                                                                       DWN00040
C       THIS ROUTINE DETERMINES THE DOWNWIND DISTANCE ASSOCIATED WITH A DWN00050
C       GIVEN TERRAIN HEIGHT WITH THE USER-SUPPLIED TERRAIN INFORMATION DWN00060
C                                                                       DWN00070
C       IDIR    - WIND DIRECTION (ROUNDED TO NEAREST 10 DEG, DIVIDED BY DWN00080
C       TERRHT  - INPUT TERRAIN HEIGHT (M) FOR WHICH A DOWNWIND DISTANCEDWN00090
C                 IS DESIRED.                                           DWN00100
C       INDEX   - THE MAXIMUM NUMBER OF DISTANCE-TERRAIN HT PAIRS KNOWN DWN00110
C                  FOR THIS WIND DIRECTION                              DWN00120
C       BASEHT  - BASE HT OF THE STACK, A REFERENCE LEVEL FROM WHICH ALLDWN00130
C                 HEIGHTS ARE COMPUTED.                                 DWN00140
C       X       - THE RETURNED DOWNWIND DISTANCE  (METERS)              DWN00150
C                                                                       DWN00160
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          DWN00170
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          DWN00180
C                                                                       DWN00190
C       RTDM     VERSION 3.2     LEVEL 861015                           DWN00200
C********************************************************************   DWN00210
C                                                                       DWN00220
C******************************************************                 DWN00230
C                                                                       DWN00240
C      COMMON BLOCK FOR TERRAIN DESCRIPTION DATA                        DWN00250
C                                                                       DWN00260
C******************************************************                 DWN00270
C      COMMON /TERR/                                                    IBM00280
C    X                 BEGHT,       @BEGINNING CONTOUR HT, USER UNITS   IBM00290
C    X                 HTINCR,      @CONTOUR INCREMENT, USER UNITS      IBM00300
C    X                 CONTUR(36,21)@DISTANCES FROM SOURCE ALONG EACH   IBM00310
C    X                              @10-DEG RADIAL FOR HEIGHTS STARTING IBM00320
C    X                              @AT BEGHT, IN INCREMENTS OF HTINCR--IBM00330
C    X                              @UP TO 20 DISTANCES (CONTOUR INCRE- IBM00340
C    X                              @MENTS) ALLOWED ALONG EACH RADIAL   IBM00350
       COMMON /TERR/ BEGHT,HTINCR,CONTUR(36,21)                         IBM00127
C                                                                       DWN00360
        COMMON /PR002/ VSCALE                                           DWN00370
C                                                                       DWN00380
        DO 1000  I = 1,INDEX                                            DWN00390
C                                                                       DWN00400
        HT = (BEGHT + (I-1)*HTINCR) * VSCALE - BASEHT                   DWN00410
C       IF(I.EQ.1.OR.TERRHT.GT.HT) GO TO 500  @ KEEP SEARCHING          IBM00420
        IF(I.EQ.1.OR.TERRHT.GT.HT) GO TO 500                            IBM00128
        X2 = CONTUR(IDIR,I) * 1000.                                     DWN00430
C                                                                       DWN00440
C       TERRAIN HT IS BETWEEN THIS LEVEL AND THE PREVIOUS ONE           DWN00450
C                                                                       DWN00460
  200   FRAC = (TERRHT - OLDHT)/(HTINCR*VSCALE)                         DWN00470
        X = X1 + (X2-X1) * FRAC                                         DWN00480
        RETURN                                                          DWN00490
C                                                                       DWN00500
  500   X1 = CONTUR(IDIR,I) * 1000.                                     DWN00510
        OLDHT = HT                                                      DWN00520
        IF(I.LT.INDEX) GO TO 1000                                       DWN00530
C                                                                       DWN00540
C       IF ABOVE THE LAST CONTOUR, POSITION AN IMAGINARY TERRAIN CONTOURDWN00550
C       ABOVE THE TOP OF THE HILL, RETAINING THE SAME SLOPE AS THAT IN  DWN00560
C       THE LAST REAL CONTOUR INTERVAL.  THEN, COMPUTE THE DOWNWIND     DWN00570
C       DISTANCE THE USUAL WAY.                                         DWN00580
C                                                                       DWN00590
        X2 = X1 + (X1-CONTUR(IDIR,INDEX-1)*1000.)                       DWN00600
        GO TO 200                                                       DWN00610
C                                                                       DWN00620
 1000   CONTINUE                                                        DWN00630
        END                                                             DWN00640

        SUBROUTINE VDF(HH,D,S,V)                                        VDF00010
C                                                                       VDF00020
C********************************************************************   VDF00030
C                                                                       VDF00040
C       THIS SUBROUTINE CALCULATES THE VERTICAL DISTRIBUTION:           VDF00050
C       MULTIPLY RELECTED GAUSSIAN AS PER ERT MEMO CS 093               VDF00060
C                                                                       VDF00070
C       HH      -RELEASE HEIGHT (M)                                     VDF00080
C       D       -MIXING LID (M)                                         VDF00090
C       S       -VERTICAL DISPERSION COEFFICIENT (SIGMA Z)              VDF00100
C       V       -CALCULATED DISTRIBUTION VALUE (1/M)                    VDF00110
C                                                                       VDF00120
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          VDF00130
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          VDF00140
C                                                                       VDF00150
C       RTDM      VERSION 3.2     LEVEL 861015                          VDF00160
C********************************************************************   VDF00170
C                                                                       VDF00180
C  ACCURACY PARAMETERS                                                  VDF00190
C                                                                       VDF00200
        TMIN=0.0512                                                     VDF00210
        TMAX=9.21                                                       VDF00220
        V=1                                                             VDF00230
        T=(S/D)**2                                                      VDF00240
        H=HH/D                                                          VDF00250
C                                                                       VDF00260
        IF(T.GE. .6) GO TO 500                                          VDF00270
C                                                                       VDF00280
        ARG=2.*(1.-H)/T                                                 VDF00290
C                                                                       VDF00300
        IF (ARG.GE.TMAX) GO TO 400                                      VDF00310
        IF (ARG.LT.TMIN) V=V+1.-ARG                                     VDF00320
        IF (ARG.GE.TMIN) V=V+EXP(-ARG)                                  VDF00330
C                                                                       VDF00340
        ARG=2.*(1.+H)/T                                                 VDF00350
C                                                                       VDF00360
        IF(ARG.GE.TMAX) GO TO 400                                       VDF00370
        V=V+EXP(-ARG)                                                   VDF00380
C                                                                       VDF00390
        ARG=4.*(2-H)/T                                                  VDF00400
C                                                                       VDF00410
        IF(ARG.GE. TMAX) GO TO 400                                      VDF00420
        V=V+EXP(-ARG)                                                   VDF00430
C                                                                       VDF00440
        ARG=4.*(2+H)/T                                                  VDF00450
C                                                                       VDF00460
        IF(ARG.LT.TMAX) V=V+EXP(-ARG)                                   VDF00470
C                                                                       VDF00480
  400   ARG=-.5*H**2/T                                                  VDF00490
        IF (ARG.LT.-90.) V=0.                                           VDF00500
C       IF(ARG.GE.-90.) V=.797885*V*EXP(ARG)/S  @.797885=ROOT 2/PI      IBM00510
        IF(ARG.GE.-90.) V=.797885*V*EXP(ARG)/S                          IBM00129
C                                                                       VDF00520
C      IF (V.LT.1E-30) V=0.    @TO PREVENT UNDERFLOWS                   IBM00530
       IF (V.LT.1E-30) V=0.                                             IBM00130
C                                                                       VDF00540
        RETURN                                                          VDF00550
C                                                                       VDF00560
C 500   ARG=4.934802*T                  @4.934802=PI**2/2               IBM00570
  500   ARG=4.934802*T                                                  IBM00131
C                                                                       VDF00580
        IF (ARG.GE.TMAX) GO TO 900                                      VDF00590
        V=V+2.*EXP(-ARG)*COS(3.141593*H)                                VDF00600
C                                                                       VDF00610
C       ARG=19.739209*T                 @19.739209=2*PI**2              IBM00620
        ARG=19.739209*T                                                 IBM00132
C                                                                       VDF00630
        IF (ARG.LT.TMAX) V=V+2.*EXP(-ARG)*COS(6.283185*H)               VDF00640
C                                                                       VDF00650
  900   V=V/D                                                           VDF00660
C                                                                       VDF00670
C      IF (V.LT.1E-30) V=0.    @TO PREVENT UNDERFLOWS                   IBM00680
       IF (V.LT.1E-30) V=0.                                             IBM00133
C                                                                       VDF00690
        RETURN                                                          VDF00700
C                                                                       VDF00710
        END                                                             VDF00720

        SUBROUTINE WRCONC(CONC,NR)                                      WRC00010
C                                                                       WRC00020
C********************************************************************   WRC00030
C                                                                       WRC00040
C       THIS ROUTINE WRITES ONE CONCENTRATION FILE RECORD, WHICH HAS UP WRC00050
C       TO 400 RECEPTOR CONCENTRATIONS.                                 WRC00060
C                                                                       WRC00070
C       CONC    ARRAY OF SIZE NREC TO RECEIVE CONCENTRATION VALUES      WRC00080
C       NR      # OF CONCENTRATIONS TO BE WRITTEN FOR EACH RECORD       WRC00090
C                                                                       WRC00100
C                                                                       WRC00110
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          WRC00120
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          WRC00130
C                                                                       WRC00140
C                                                                       WRC00150
C       RTDM      VERSION 3.2     LEVEL 861015                          WRC00160
C                                                                       WRC00170
C********************************************************************   WRC00180
C                                                                       WRC00190
C******************************************************                 WRC00200
C                                                                       WRC00210
C      COMMON BLOCK FOR HOURLY METEOROLOGICAL DATA                      WRC00220
C                                                                       WRC00230
C******************************************************                 WRC00240
C      COMMON /MET/                                                     IBM00250
C    X                 WDIR,        @ WIND DIRECTION                    IBM00260
C    X                 WSPD,        @ WIND SPEED (M/SEC)                IBM00270
C    X                 DMX,         @ MIXING HEIGHT (M)                 IBM00280
C    X                 IST,         @ STABILITY CLASS                   IBM00290
C    X                 TEMP,        @ AMBIENT TEMPERATURE (DEG K)       IBM00300
C    X                 XIY,         @ TURBULENCE, Y-COMPONENT           IBM00310
C    X                 XIZ,         @ TURBULENCE, Z-COMPONENT           IBM00320
C    X                 PRVPTG,      @ PLUME RISE VPTG                   IBM00330
C    X                 HCVPTG,      @ HCRIT VPTG                        IBM00340
C    X                 DTHETA,      @ HORIZONTAL WIND DIRECTION SHEAR   IBM00350
C    X                 EPS          @ WIND SPEED PROFILE EXPONENT       IBM00360
       COMMON/MET/ WDIR,WSPD,DMX,IST,TEMP,XIY,XIZ,PRVPTG,HCVPTG,        IBM00134
     X             DTHETA,EPS,WSPD2                                     IBM00135
C                                                                       WRC00370
        DIMENSION CONC(NR)                                              WRC00380
        INTEGER MET(4)                                                  WRC00390
C                                                                       WRC00400
        MET(1) = INT(DMX + 0.5)                                         WRC00410
        MET(2) = INT(WDIR + 0.5)                                        WRC00420
        MET(3) = IST                                                    WRC00430
        MET(4) = INT(WSPD + 0.5)                                        WRC00440
        WRITE(9) MET,CONC                                               WRC00450
        RETURN                                                          WRC00460
        END                                                             WRC00470

C                                                                       PAG00010
       SUBROUTINE PAGE                                                  PAG00020
C                                                                       PAG00030
C********************************************************************   PAG00040
C                                                                       PAG00050
C       THIS ROUTINE SKIPS TO A NEW PAGE AND WRITES A PAGE HEADER       PAG00060
C                                                                       PAG00070
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          PAG00080
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          PAG00090
C                                                                       PAG00100
C       RTDM      VERSION 3.2     LEVEL 861015                          PAG00110
C                                                                       PAG00120
C********************************************************************   PAG00130
C                                                                       PAG00140
       COMMON /HEAD/ NPAGE,NLINE                                        PAG00150
       NPAGE=NPAGE+1                                                    PAG00160
       NLINE=3                                                          PAG00170
       WRITE(6,1000) NPAGE                                              PAG00180
 1000  FORMAT('1','  ROUGH TERRAIN DIFFUSION MODEL   VERSION 3.2   ',   PAG00190
     X 'LEVEL 861130',28X,'PAGE',1X,I3,/)                               PAG00200
       RETURN                                                           PAG00210
       END                                                              PAG00220

        SUBROUTINE LINES(N,IVAL)                                        LIN00010
C                                                                       LIN00020
C***********************************************************************LIN00030
C                                                                       LIN00040
C       N       NUMBER OF NEW LINES                                     LIN00050
C       IVAL    DETERMINES WHETHER OR NOT A NEW PAGE IS STARTED         LIN00060
C                                                                       LIN00070
C      ENVIRONMENTAL RESEARCH AND TECHNOLOGY, INC.                      LIN00080
C      696 VIRGINIA ROAD, CONCORD, MASS. 01742                          LIN00090
C                                                                       LIN00100
C      RTDM      VERSION 3.2           LEVEL 861015                     LIN00110
C                                                                       LIN00120
C***********************************************************************LIN00130
C                                                                       LIN00140
        COMMON /HEAD/ NPAGE,NLINE                                       LIN00150
        IVAL = 0                                                        LIN00160
        NLINE = NLINE + N                                               LIN00170
        IF(NLINE.LE.63) RETURN                                          LIN00180
        CALL PAGE                                                       LIN00190
        NLINE = NLINE + N                                               LIN00200
        IVAL = 1                                                        LIN00210
        RETURN                                                          LIN00220
        END                                                             LIN00230
