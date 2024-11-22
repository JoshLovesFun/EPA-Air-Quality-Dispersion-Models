C***********************************************************************BLS00005
C                                                                       BLS00006
C                        BLPSUM (DATED 99176)                           BLS00010
C                                                                       BLS00060
C             *** SEE BLP MODEL CHANGE BULLETIN MCB#3 ***               BLS00061
C                                                                       BLS00062
C    ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD     BLS00063
C                                                                       BLS00064
C                            919-541-5742                               BLS00065
C                                                                       BLS00066
C***********************************************************************BLS00070
C                                                                       BPS00080
C      BLPSUM -- PROGRAM TO SUM CONCENTRATIONS FROM TWO BLP RUNS        BPS00090
C                                                                       BPS00100
C                                                                       BPS00110
C                                                                       BPS00120
C      DEVELOPED BY:                                                    BPS00130
C                                                                       BPS00140
C      JOE SCIRE                                                        BPS00150
C      ENVIRONMENTAL RESEARCH AND TECHNOLOGY, INC.                      BPS00160
C      696 VIRGINIA ROAD                                                BPS00170
C      CONCORD, MASSACHUSETTS  01742                                    BPS00180
C                                                                       BPS00190
C***********************************************************************BPS00200
C
C      MODIFIED BY:
C
C      ROGER W. BRODE
C      PACIFIC ENVIRONMENTAL SERVICES, INC.
C      5001 S. MIAMI BLVD, SUITE 300
C      P.O. BOX 12077
C      RESEARCH TRIANGLE PARK, NC  27709
C
C      June 25, 1999
C
C      Modified to get filenames from the command line using the Lahey
C      LF90 GETCL function (based on the ISCST3 model code), and to write
C      the model run date and time to the main output file.  Version
C      date used for output is now defined once in BLOCK DATA as
C      CHARACTER*5 VERSN.
C
C***********************************************************************
C                                                                       BPS00210
      CHARACTER*4 TITLE(20),TITLE1(20),TITLE2(20)                       BPS00220
      REAL CHI1(100),CHI2(100)                                          BPS00230
      REAL ASCAL1(62),BSCAL1(62),ASCAL2(62),BSCAL2(62)                  BPS00240
      INTEGER ISCAL1(62),ISCAL2(62)                                     BPS00250
      INTEGER ISCOD1(62),ISCOD2(62)                                     BPS00260
      INTEGER IPCL1(11),IPCL2(11),IPCP1(51),IPCP2(51)                   BPS00270
      INTEGER IPCL(11)/11*0/,IPCP(51)/51*0/                             BPS00280
      DIMENSION IDAYS1(366),IDAYS2(366)                                 BPS00290
      LOGICAL LSUM,LSCALE,LCOMPR,LEND                                   BPS00300
      LOGICAL LCMPR1,LCMPR2                                             BPS00310
      NAMELIST/INPUTS/IJCOD1,IJCOD2,ISCOD1,ISCOD2,NSUM1,NSUM2,LSUM,     BPS00320
     1 LSCALE,LCOMPR                                                    BPS00330
      NAMELIST/ECH1/ISCAL1,ASCAL1,BSCAL1,ISCAL2,ASCAL2,BSCAL2           BPS00340
      DATA IJCOD1/999/,IJCOD2/999/,ISCOD1/62*0/,ISCOD2/62*0/            BPS00350
      DATA NSUM1/0/,NSUM2/0/,NTOT/0/                                    BPS00360
      DATA LSUM/.FALSE./,LSCALE/.FALSE./,LCOMPR/.FALSE./                BPS00370
CPES  Begin PES Code Changes

C     Declare ILEN_FLD Parameter, which controls length of filenames
C     Also declare variables for input and output filenames, version date
C     and model run time and date.
      INTEGER, PARAMETER :: ILEN_FLD = 80
      CHARACTER (LEN=ILEN_FLD) :: INPFIL, OUTFIL, CNCINP1, CNCINP2,
     &                            CNCOUT
      COMMON/IOFILE/ INPFIL, OUTFIL, CNCINP1, CNCINP2, CNCOUT
      CHARACTER RUNDAT*8, RUNTIM*8, VERSN*5
      COMMON/DATETIME/ RUNDAT, RUNTIM, VERSN

C     Get Date and Time using system-specific functions     ---   CALL DATIME
      CALL DATIME (RUNDAT, RUNTIM)

C     Retrieve Input and Output File Names From Command Line,
C                                                           ---   CALL GETCOM
      CALL GETCOM (' BLPSUM ',ILEN_FLD,INPFIL,OUTFIL,CNCINP1,CNCINP2,
     &             CNCOUT)

C     Open Input and Output Files                           ---   CALL FILOPN
      CALL FILOPN (ILEN_FLD,INPFIL,OUTFIL,CNCINP1,CNCINP2,CNCOUT)

      WRITE(6,1234) VERSN, RUNDAT, RUNTIM
1234  FORMAT ('1',21X,'BLPSUM           (DATED ',A5,')',71X,A8/123X,A8/)
      WRITE(6,1400) VERSN, RUNDAT, RUNTIM
1400  FORMAT('1',45X,'BLPSUM  SCRAM VERSION (DATED ',A5,')',
     1 42X,A8,/,123X,A8 /' ',13('**********'))

CPES  End PES Code Changes
      WRITE(6,1402)                                                     BPS00500
1402  FORMAT(//)                                                        BPS00510
C                                                                       BPS00520
C     READ USER INPUTS                                                  BPS00530
C                                                                       BPS00540
      READ(5,2)TITLE                                                    BPS00550
2     FORMAT(20A4)                                                      BPS00560
      READ(5,INPUTS)                                                    BPS00570
      WRITE(6,INPUTS)                                                   BPS00580
C                                                                       BPS00590
C     CHECK RANGE OF NSUM1 & NSUM2 AND VERIFY ISCOD1 & ISCOD2 VALUES    BPS00600
C     HAVE BEEN ENTERED IN CORRECT ORDER                                BPS00610
C                                                                       BPS00620
      IF(.NOT.LSUM)GO TO 18                                             BPS00630
      IF(NSUM1.GT.0.AND.NSUM2.GT.0)GO TO 11                             BPS00640
      WRITE(6,10)NSUM1,NSUM2,LSUM                                       BPS00650
10    FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  BPS00660
     1 'OF NSUM1 OR NSUM2'/'0','NSUM1 AND NSUM2 MUST BE GE 1 WHEN ',    BPS00670
     2 'LSUM IS TRUE'/'0','NSUM1 = ',I3,3X,'NSUM2 = ',I3,3X,            BPS00680
     3 'LSUM = ',L1)                                                    BPS00690
C     CALL WAUDIT
      STOP                                                              BPS00700
11    CONTINUE                                                          BPS00710
      IF(NSUM1.EQ.1)GO TO 14                                            BPS00720
      NSUMM1=NSUM1-1                                                    BPS00730
      DO 12 I=1,NSUMM1                                                  BPS00740
      IF(ISCOD1(I+1).GT.ISCOD1(I))GO TO 12                              BPS00750
      WRITE(6,31)(IN1,ISCOD1(IN1),IN1=1,NSUM1)                          BPS00760
31    FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  BPS00770
     1 'OF ISCOD1'/'0','VALUES OF SOURCE CODES (ISCOD1) MUST BE ',      BPS00780
     2 'ENTERED IN'/'0','ORDER OF INCREASING NUMERICAL VALUE -- FOR ',  BPS00790
     3 'EXAMPLE, ENTER ISCOD1 = 1,2,101, NOT ISCOD1 = 101,2,1 '/'0',    BPS00800
     4 3X,'I',6X,'ISCOD1(I)'//62(3X,I2,9X,I3/))                         BPS00810
C     CALL WAUDIT
      STOP                                                              BPS00820
12    CONTINUE                                                          BPS00830
14    CONTINUE                                                          BPS00840
      IF(NSUM2.EQ.1)GO TO 18                                            BPS00850
      NSUMM1=NSUM2-1                                                    BPS00860
      DO 16 I=1,NSUMM1                                                  BPS00870
      IF(ISCOD2(I+1).GT.ISCOD2(I))GO TO 16                              BPS00880
      WRITE(6,32)(IN2,ISCOD2(IN2),IN2=1,NSUM2)                          BPS00890
32    FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  BPS00900
     1 'OF ISCOD2'/'0','VALUES OF SOURCE CODES (ISCOD2) MUST BE ',      BPS00910
     2 'ENTERED IN'/'0','ORDER OF INCREASING NUMERICAL VALUE -- FOR ',  BPS00920
     3 'EXAMPLE, ENTER ISCOD2 = 1,2,101, NOT ISCOD2 = 101,2,1 '/'0',    BPS00930
     4 3X,'I',6X,'ISCOD2(I)'//62(3X,I2,9X,I3/))                         BPS00940
C     CALL WAUDIT
      STOP                                                              BPS00950
16    CONTINUE                                                          BPS00960
18    CONTINUE                                                          BPS00970
C                                                                       BPS00980
C     IF USING SCALING OPTION, READ SCALING PARAMETERS                  BPS00990
C                                                                       BPS01000
      IF(.NOT.LSCALE)GO TO 29                                           BPS01010
      IF(LSUM)GO TO 5                                                   BPS01020
      NSUM1=1                                                           BPS01030
      NSUM2=1                                                           BPS01040
5     CONTINUE                                                          BPS01050
      DO 7 I=1,NSUM1                                                    BPS01060
      READ(5,6)ISCAL1(I),ASCAL1(I),BSCAL1(I)                            BPS01070
6     FORMAT(I3,7X,2F10.5)                                              BPS01080
7     CONTINUE                                                          BPS01090
      DO 8 I=1,NSUM2                                                    BPS01100
      READ(5,6)ISCAL2(I),ASCAL2(I),BSCAL2(I)                            BPS01110
8     CONTINUE                                                          BPS01120
C                                                                       BPS01130
C     CHECK FOR VALID VALUES IN ISCAL1 & ISCAL2 ARRAYS                  BPS01140
C                                                                       BPS01150
      IF(LSUM)GO TO 20                                                  BPS01160
C     LSUM IS .FALSE.                                                   BPS01170
      IF(ISCAL1(1).EQ.IJCOD1.AND.ISCAL2(1).EQ.IJCOD2)GO TO 29           BPS01180
      WRITE(6,21)LSCALE,LSUM,IJCOD1,ISCAL1(1),IJCOD2,ISCAL2(1)          BPS01190
21    FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  BPS01200
     1 'OF IJCOD1, IJCOD2, ISCAL1, OR ISCAL2'/'0',                      BPS01210
     1 'WHEN LSCALE IS TRUE AND LSUM IS ',                              BPS01220
     2 'FALSE, ISCAL1(1) MUST EQUAL IJCOD1 AND ISCAL2(1) MUST EQUAL ',  BPS01230
     2 'IJCOD2'/'0','LSCALE = ',L1,3X,                                  BPS01240
     3 'LSUM = ',L1,3X,'IJCOD1 = ',I3,3X,'ISCAL1(1) = ',I3,3X,          BPS01250
     4 'IJCOD2 = ',I3,3X,'ISCAL2(1) = ',I3)                             BPS01260
C     CALL WAUDIT
      STOP                                                              BPS01270
20    CONTINUE                                                          BPS01280
C     LSUM IS .TRUE.                                                    BPS01290
      DO 35 I=1,NSUM1                                                   BPS01300
      IF(ISCAL1(I).EQ.ISCOD1(I))GO TO 35                                BPS01310
      WRITE(6,34)LSCALE,LSUM,(IC,ISCOD1(IC),ISCAL1(IC),IC=1,NSUM1)      BPS01320
34    FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  BPS01330
     1 'OF ISCOD1 OR ISCAL1'/'0','WHEN LSCALE AND LSUM ARE TRUE, ',     BPS01340
     2 'SOURCE CODES ARRAYS (ISCAL1 & ISCOD1) MUST BE EQUAL AND ',      BPS01350
     3 'SOURCE CODE VALUES MUST BE ENTERED IN'/'0','ORDER OF ',         BPS01360
     3 'INCREASING ',                                                   BPS01370
     4 'NUMERICAL VALUE -- FOR EXAMPLE, ENTER ISCAL1 = 1,2,101, ',      BPS01380
     5 'NOT ISCAL1 = 101,2,1'/'0','LSCALE = ',L1,3X,'LSUM = ',L1/'0',   BPS01390
     6 3X,'I',6X,'ISCOD1(I)',5X,'ISCAL1(I)'//62(3X,I2,9X,I3,11X,I3/))   BPS01400
C     CALL WAUDIT
      STOP                                                              BPS01410
35    CONTINUE                                                          BPS01420
      DO 19 I=1,NSUM2                                                   BPS01430
      IF(ISCAL2(I).EQ.ISCOD2(I))GO TO 19                                BPS01440
      WRITE(6,36)LSCALE,LSUM,(IC,ISCOD2(IC),ISCAL2(IC),IC=1,NSUM2)      BPS01450
36    FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  BPS01460
     1 'OF ISCOD2 OR ISCAL2'/'0','WHEN LSCALE AND LSUM ARE TRUE, ',     BPS01470
     2 'SOURCE CODES ARRAYS (ISCAL2 & ISCOD2) MUST BE EQUAL AND ',      BPS01480
     3 'SOURCE CODE VALUES MUST BE ENTERED IN'/'0','ORDER OF ',         BPS01490
     3 'INCREASING ',                                                   BPS01500
     4 'NUMERICAL VALUE -- FOR EXAMPLE, ENTER ISCAL1 = 1,2,101, ',      BPS01510
     5 'NOT ISCAL2 = 101,2,1'/'0','LSCALE = ',L1,3X,'LSUM = ',L1/'0',   BPS01520
     6 3X,'I',6X,'ISCOD2(I)',5X,'ISCAL2(I)'//62(3X,I2,9X,I3,11X,I3/))   BPS01530
C     CALL WAUDIT
      STOP                                                              BPS01540
19    CONTINUE                                                          BPS01550
29    CONTINUE                                                          BPS01560
      WRITE(6,1402)                                                     BPS01570
      WRITE(6,ECH1)                                                     BPS01580
C                                                                       BPS01590
C     WRITE SUMMING INFORMATION                                         BPS01600
C                                                                       BPS01610
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,161)                                                      BPS01630
161   FORMAT(/'0','THIS RUN OF BLPSUM SUMS THE FOLLOWING SOURCE ',      BPS01640
     1 'CONTRIBUTION GROUPS: '//'0',5X,'BLP RUN',3X,'SOURCE ',          BPS01650
     2 'CONTRIBUTION GROUP'/)                                           BPS01660
      IF(LSUM)GO TO 165                                                 BPS01670
      CALL XWRT2(1,1,IJCOD1)                                            BPS01680
      CALL XWRT2(2,1,IJCOD2)                                            BPS01690
      GO TO 166                                                         BPS01700
165   CONTINUE                                                          BPS01710
      CALL XWRT2(1,NSUM1,ISCOD1)                                        BPS01720
      CALL XWRT2(2,NSUM2,ISCOD2)                                        BPS01730
166   CONTINUE                                                          BPS01740
      IF(LCOMPR)WRITE(6,168)                                            BPS01750
168   FORMAT(/'0','ARRAY COMPRESSION OPTION REQUESTED')                 BPS01760
      IF(.NOT.LCOMPR)WRITE(6,169)                                       BPS01770
169   FORMAT(/'0','ARRAY COMPRESSION OPTION NOT REQUESTED')             BPS01780
C                                                                       BPS01790
C     IF ANY SCALING PARAMETER < 0, LCOMPR MUST BE .FALSE.              BPS01800
C                                                                       BPS01810
      IF(.NOT.LSCALE.OR..NOT.LCOMPR)GO TO 183                           BPS01820
      DO 181 I=1,NSUM1                                                  BPS01830
      IF(ASCAL1(I).GE.0.0.AND.BSCAL1(I).GE.0.0)GO TO 181                BPS01840
      LCOMPR=.FALSE.                                                    BPS01850
      WRITE(6,180)                                                      BPS01860
180   FORMAT(/'0','LCOMPR MUST BE .FALSE. WHEN USING NEGATIVE ',        BPS01870
     1 'SCALING PARAMTERS -- LCOMPR CHANGED TO .FALSE.')                BPS01880
181   CONTINUE                                                          BPS01890
      DO 182 I=1,NSUM2                                                  BPS01900
      IF(ASCAL2(I).GE.0.0.AND.BSCAL2(I).GE.0.0)GO TO 182                BPS01910
      LCOMPR=.FALSE.                                                    BPS01920
      WRITE(6,180)                                                      BPS01930
182   CONTINUE                                                          BPS01940
183   CONTINUE                                                          BPS01950
C                                                                       BPS01960
C     WRITE SCALING INFORMATION                                         BPS01970
C                                                                       BPS01980
      IF(.NOT.LSCALE)GO TO 55                                           BPS01990
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,51)                                                       BPS02010
51    FORMAT(/'0','CONCENTRATION SCALING OPTION REQUESTED -- ',         BPS02020
     1 'CONCENTRATIONS WILL BE SCALED IN THE FOLLOWING WAY:'/'0',       BPS02030
     2 19X,'CONC.(NEW--BLP FILE 1) = ASCAL1 * CONC.(OLD--BLP FILE 1)',  BPS02040
     3 ' + BSCAL1'/20X,'CONC.(NEW--BLP FILE 2) = ASCAL2 * CONC.(OLD--', BPS02050
     4 'BLP FILE 2) + BSCAL2'/'0','WHERE ASCAL1, BSCAL1, ASCAL2, AND ', BPS02060
     5 'BSCAL2 ARE INPUT BY THE USER FOR EACH SOURCE CONTRIBUTION ',    BPS02070
     5 'GROUP')                                                         BPS02080
      IJ=1                                                              BPS02090
      WRITE(6,52)IJ,IJ,IJ                                               BPS02100
52    FORMAT(/'0','SOURCE',10X,'SOURCE CODE',6X,'ASCAL',I1,7X,          BPS02110
     1 'BSCAL',I1/2X,'GROUP',12X,'(ISCAL',I1,')',19X,'(UG/M**3)'/)      BPS02120
      CALL XWRT(NSUM1,ASCAL1,BSCAL1,ISCAL1)                             BPS02130
      IJ=2                                                              BPS02140
      WRITE(6,52)IJ,IJ,IJ                                               BPS02150
      CALL XWRT(NSUM2,ASCAL2,BSCAL2,ISCAL2)                             BPS02160
      GO TO 57                                                          BPS02170
55    CONTINUE                                                          BPS02180
      WRITE(6,56)                                                       BPS02190
56    FORMAT(//'0','CONCENTRATION SCALING OPTION NOT REQUESTED')        BPS02200
57    CONTINUE                                                          BPS02210
C                                                                       BPS02220
C     READ TITLE AND RUN INFORMATION FROM RECORD #1 OF EACH BLP FILE    BPS02230
C                                                                       BPS02240
      READ(10)TITLE1,NNREC1,NPTS1,NLINE1,IPCL1,IPCP1,IYR1,IDAYS1        BPS02250
      READ(11)TITLE2,NNREC2,NPTS2,NLINE2,IPCL2,IPCP2,IYR2,IDAYS2        BPS02260
C                                                                       BPS02270
      LCMPR1=.FALSE.                                                    BPS02280
      IF(NNREC1.GT.1000)LCMPR1=.TRUE.                                   BPS02290
      NREC1=MOD(NNREC1,1000)                                            BPS02300
      LCMPR2=.FALSE.                                                    BPS02310
      IF(NNREC2.GT.1000)LCMPR2=.TRUE.                                   BPS02320
      NREC2=MOD(NNREC2,1000)                                            BPS02330
C                                                                       BPS02340
C     CHECK TO VERIFY TWO BLP INPUT DATA SETS ARE COMPATIBLE            BPS02350
C                                                                       BPS02360
      IF(NREC1.EQ.NREC2)GO TO 101                                       BPS02370
      WRITE(6,901)NREC1,NREC2                                           BPS02380
901   FORMAT(//'0','EXECUTION TERMINATING -- RECEPTORS IN EACH BLP ',   BPS02390
     1 'INPUT FILE MUST BE THE SAME'/'0','NUMBER OF RECEPTORS IN ',     BPS02400
     2 'INPUT FILE 1 = ',I3/'0','NUMBER OF RECEPTORS IN INPUT FILE 2 ', BPS02410
     3 '= ',I3)                                                         BPS02420
C     CALL WAUDIT
      STOP                                                              BPS02430
101   CONTINUE                                                          BPS02440
      IF(IYR1.EQ.IYR2)GO TO 102                                         BPS02450
      WRITE(6,902)IYR1,IYR2                                             BPS02460
902   FORMAT(//'0','EXECUTION TERMINATING -- TIME PERIOD OF BLP INPUT ',BPS02470
     1 'FILES MUST BE THE SAME'/'0','YEAR OF INPUT FILE 1 = ',I2/'0',   BPS02480
     2 'YEAR OF INPUT FILE 2 = ',I2)                                    BPS02490
C     CALL WAUDIT
      STOP                                                              BPS02500
102   CONTINUE                                                          BPS02510
      DO 103 I=1,366                                                    BPS02520
      IF(IDAYS1(I).EQ.IDAYS2(I))GO TO 103                               BPS02530
      WRITE(6,903)IDAYS1,IDAYS2                                         BPS02540
903   FORMAT(//'0','EXECUTION TERMINATING -- TIME PERIOD OF BLP ',      BPS02550
     1 'INPUT FILES MUST BE THE SAME'/'0','DAYS INCLUDED IN INPUT ',    BPS02560
     2 'FILE 1 (0=NOT INCLUDED; 1=INCLUDED):'/                          BPS02570
     3 3('0',10(10I1,3X)/),'0',6(10I1,3X),6I1//'0','DAYS ',             BPS02580
     4 'INCLUDED IN INPUT FILE 2 (0=NOT INCLUDED; 1=INCLUDED):'/        BPS02590
     5 3('0',10(10I1,3X)/),'0',6(10I1,3X),6I1)                          BPS02600
C     CALL WAUDIT
      STOP                                                              BPS02610
103   CONTINUE                                                          BPS02620
C                                                                       BPS02630
      NTA1=1                                                            BPS02640
      NTA2=1                                                            BPS02650
      DO 110 I=1,11                                                     BPS02660
      NTA1=NTA1+IPCL1(I)                                                BPS02670
      NTA2=NTA2+IPCL2(I)                                                BPS02680
110   CONTINUE                                                          BPS02690
      DO 111 I=1,51                                                     BPS02700
      NTA1=NTA1+IPCP1(I)                                                BPS02710
      NTA2=NTA2+IPCP2(I)                                                BPS02720
111   CONTINUE                                                          BPS02730
C                                                                       BPS02740
C     CALCULATE NO. SOURCE CONTRIBUTIONS IN OUTPUT FILE                 BPS02750
C     WRITE HEADER RECORD                                               BPS02760
C                                                                       BPS02770
      IF(LSUM)GO TO 60                                                  BPS02780
      NPTS=NPTS1+NPTS2                                                  BPS02790
      NLINES=NLINE1+NLINE2                                              BPS02800
      GO TO 69                                                          BPS02810
60    CONTINUE                                                          BPS02820
C     LSUM IS .TRUE. -- CALCULATE NUMBER OF POINT & LINE SOURCES        BPS02830
      NLINES=0                                                          BPS02840
      DO 61 I=1,10                                                      BPS02850
      ISV=I                                                             BPS02860
      IF(ISCOD1(I).LE.0.OR.ISCOD1(I).GT.10)GO TO 62                     BPS02870
      NLINES=NLINES+1                                                   BPS02880
61    CONTINUE                                                          BPS02890
62    CONTINUE                                                          BPS02900
      IF(ISCOD1(ISV).EQ.11)NLINES=NLINES+NLINE1                         BPS02910
      DO 63 J=1,10                                                      BPS02920
      JSV=J                                                             BPS02930
      IF(ISCOD2(J).LE.0.OR.ISCOD2(J).GT.10)GO TO 64                     BPS02940
      NLINES=NLINES+1                                                   BPS02950
63    CONTINUE                                                          BPS02960
64    CONTINUE                                                          BPS02970
      IF(ISCOD2(JSV).EQ.11)NLINES=NLINES+NLINE2                         BPS02980
      NPTS=0                                                            BPS02990
      DO 65 I=ISV,51                                                    BPS03000
      ISV2=I                                                            BPS03010
      IF(ISCOD1(I).LE.11.OR.ISCOD1(I).GT.150)GO TO 66                   BPS03020
      NPTS=NPTS+1                                                       BPS03030
65    CONTINUE                                                          BPS03040
66    CONTINUE                                                          BPS03050
      IF(ISCOD1(ISV2).EQ.151)NPTS=NPTS+NPTS1                            BPS03060
      DO 67 J=JSV,51                                                    BPS03070
      JSV2=J                                                            BPS03080
      IF(ISCOD2(J).LE.11.OR.ISCOD2(J).GT.150)GO TO 68                   BPS03090
      NPTS=NPTS+1                                                       BPS03100
67    CONTINUE                                                          BPS03110
68    CONTINUE                                                          BPS03120
      IF(ISCOD2(JSV2).EQ.151)NPTS=NPTS+NPTS2                            BPS03130
69    CONTINUE                                                          BPS03140
      NNREC=NREC1                                                       BPS03150
      IF(LCOMPR)NNREC=NNREC+1000                                        BPS03160
      WRITE(20)TITLE,NNREC,NPTS,NLINES,IPCL,IPCP,IYR1,IDAYS1            BPS03170
C                                                                       BPS03180
C     WRITE INFORMATION FROM EACH BLP FILE                              BPS03190
      IJ=1                                                              BPS03200
      WRITE(6,170)IJ,TITLE1,NREC1,NPTS1,NLINE1                          BPS03210
170   FORMAT(/'0','BLP FILE ',I1,' INFORMATION'/'0',5X,'TITLE: ',       BPS03220
     1 20A4/7X,'NREC:',I4/7X,'NPTS:',I4/5X,'NLINES:',I4)                BPS03230
      IJ=2                                                              BPS03240
      WRITE(6,170)IJ,TITLE2,NREC2,NPTS2,NLINE2                          BPS03250
      WRITE(6,171)TITLE,NREC1,NPTS,NLINES                               BPS03260
171   FORMAT(/'0','BLP OUTPUT FILE INFORMATION'/'0',5X,'TITLE: ',       BPS03270
     1 20A4/7X,'NREC:',I4/7X,'NPTS:',I4/5X,'NLINES:',I4)                BPS03280
C                                                                       BPS03290
C     MAIN LOOP                                                         BPS03300
C                                                                       BPS03310
      DO 1000 JDAY=1,366                                                BPS03320
      IF(IDAYS1(JDAY).NE.1)GO TO 1000                                   BPS03330
      NTOT=NTOT+1                                                       BPS03340
      DO 900 JHR=1,24                                                   BPS03350
      CALL GETDAT(10,NREC1,NTA1,IJCOD1,LCMPR1,LSUM,NSUM1,ISCOD1,        BPS03360
     1 LSCALE,ASCAL1,BSCAL1,IDAYHR,ICD,IMET2,CHI1,LEND)                 BPS03370
      IF(LEND)GO TO 1001                                                BPS03380
      CALL GETDAT(11,NREC2,NTA2,IJCOD2,LCMPR2,LSUM,NSUM2,ISCOD2,        BPS03390
     1 LSCALE,ASCAL2,BSCAL2,JDAYHR,JCD,JMET2,CHI2,LEND)                 BPS03400
C                                                                       BPS03410
C     CHECK THAT DATE AND MET. DATA IN TWO BLP FILES MATCH              BPS03420
C                                                                       BPS03430
      ICD1=ICD/1000                                                     BPS03440
      JCD1=JCD/1000                                                     BPS03450
      IF(IDAYHR.EQ.JDAYHR.AND.IMET2.EQ.JMET2.AND.ICD1.EQ.JCD1)GO TO 305 BPS03460
      WRITE(6,304)JDAY,JHR,IDAYHR,JDAYHR,IMET2,JMET2,ICD1,JCD1          BPS03470
304   FORMAT(//'0','EXECUTION TERMINATING -- DATE OR MET. DATA DO NOT ',BPS03480
     1 'MATCH IN TWO BLP INPUT FILES'/'0','JDAY = ',I3,3X,'JHR = ',I2/  BPS03490
     2 '0','IDAYHR = ',I5,3X,'JDAYHR = ',                               BPS03500
     2 I5/'0','IMET2 = ',I7,3X,'JMET2 = ',I7/'0','ICD1 = ',I4,3X,       BPS03510
     3 'JCD1 = ',I4)                                                    BPS03520
C     CALL WAUDIT
      STOP                                                              BPS03530
305   CONTINUE                                                          BPS03540
      DO 800 I=1,NREC1                                                  BPS03550
      CHI1(I)=CHI1(I)+CHI2(I)                                           BPS03560
      IF(CHI1(I).GE.0.0.OR..NOT.LCOMPR)GO TO 800                        BPS03570
      WRITE(6,798)                                                      BPS03580
798   FORMAT(/'0','EXECUTION TERMINATING -- NEGATIVE CONCENTRATION ',   BPS03590
     1 'CALCULATED WITH LCOMPR = .TRUE.'/'0','RERUN WITH LCOMPR = ',    BPS03600
     2 '.FALSE.')                                                       BPS03610
C     CALL WAUDIT
      STOP                                                              BPS03620
800   CONTINUE                                                          BPS03630
      CALL OUTPT(IDAYHR,ICD,IMET2,NREC1,CHI1,LCOMPR)                    BPS03640
900   CONTINUE                                                          BPS03650
1000  CONTINUE                                                          BPS03660
1001  CONTINUE                                                          BPS03670
      WRITE(6,1400) VERSN, RUNDAT, RUNTIM
      WRITE(6,1402)                                                     BPS03690
      WRITE(6,70)IDAYS1,NTOT                                            BPS03700
70    FORMAT('0','OUTPUT FILE CONTAINS CONCENTRATIONS FOR THE ',        BPS03710
     1 'FOLLOWING DAYS (0=NOT INCLUDED; 1=INCLUDED):'/                  BPS03720
     2 3('0',10(10I1,3X)/),'0',6(10I1,3X),6I1//'0','TOTAL OF ',I3,      BPS03730
     3 ' DAYS')                                                         BPS03740
C     CALL WAUDIT
      STOP                                                              BPS03750
      END                                                               BPS03760
CPES  Begin PES Code Changes

      SUBROUTINE GETCOM (MODEL,LENGTH,INPFIL,OUTFIL,CNCINP1,CNCINP2,
     &                   CNCOUT)
C***********************************************************************
C     
C        ADAPTED FROM PCCODE Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Retrieving Input and Output File Names From
C                 the Command Line for PCs
C
C        PROGRAMMER: Roger Brode
C        
C        DATE:    March 2, 1992
C
C        MODIFIED:   To use ILEN_FLD (passed in as LENGTH) to define
C                    the length of the INPFIL and OUTFIL variables,
C                    and to specify length of the command line as
C                    a PARAMETER, initially set to 150.  Also set up
C                    conditional compilation statements (commented out)
C                    to facilitate compilation by DEC Visual Fortran.
C                    R.W. Brode, PES, Inc. - 12/2/98
C
C        MODIFIED:   Jayant Hardikar, PES, Inc.
C                    - Length of command line for Lahey version changed
C                      from 80 to 120 characters - 4/19/93
C                    - Adapted for DEPMET/PMERGE - 7/29/94
C
C        INPUTS:  Command Line
C
C        OUTPUTS: Input Runstream File Name
C                 Output Print File Name
C
C        CALLED FROM:   MAIN
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      INTEGER LENGTH
      CHARACTER (LEN=LENGTH) :: INPFIL, OUTFIL, CNCINP1, CNCINP2, CNCOUT
      CHARACTER (LEN=8)      :: MODEL
C     Declare the COMLIN Variable to Hold Contents of Command Line for Lahey
      INTEGER , PARAMETER :: LENCL = 150
      CHARACTER (LEN=LENCL) :: COMLIN
      INTEGER LOCB(LENCL), LOCE(LENCL), I, IFCNT
      LOGICAL INFLD

      COMLIN = ' '

C************************************************************LAHEY START
C     Use Lahey Function GETCL To Retrieve Contents of Command Line.
C     Retrieve Input and Output File Names From the COMLIN Variable.
      CALL GETCL(COMLIN)
      INFLD = .FALSE.
      IFCNT = 0
      DO I = 1, LENCL
         IF (.NOT.INFLD .AND. COMLIN(I:I) .NE. ' ') THEN
            INFLD = .TRUE.
            IFCNT = IFCNT + 1
            LOCB(IFCNT) = I
         ELSE IF (INFLD .AND. COMLIN(I:I) .EQ. ' ') THEN
            INFLD = .FALSE.
            LOCE(IFCNT) = I - 1
         END IF
      END DO
      IF (IFCNT .NE. 5) THEN
C        Error on Command Line.  Write Error Message and STOP
         WRITE(*,660) MODEL
         STOP
      END IF
      INPFIL  = COMLIN(LOCB(1):LOCE(1))
      OUTFIL  = COMLIN(LOCB(2):LOCE(2))
      CNCINP1 = COMLIN(LOCB(3):LOCE(3))
      CNCINP2 = COMLIN(LOCB(4):LOCE(4))
      CNCOUT  = COMLIN(LOCB(5):LOCE(5))

C************************************************************LAHEY STOP

  660 FORMAT (' COMMAND LINE ERROR: ',A8,' input_file output_file',
     &        ' conc1_inp conc2_inp concen_out')

      RETURN
      END


      SUBROUTINE DATIME ( DCALL, TCALL )
C***********************************************************************
C                 DATIME Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:    April 15, 1994
C
C        MODIFIED:   Uses Fortran 90 DATE_AND_TIME routine.
C                    R.W. Brode, PES, 8/14/98
C
C        INPUTS:  none
C
C        OUTPUTS: Date and time in character format
C
C        CALLED FROM:  RUNTIME
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      CHARACTER DCALL*8, TCALL*8
      CHARACTER CDATE*8, CTIME*10, CZONE*5
      INTEGER  :: IDATETIME(8)
      INTEGER  :: IPTYR, IPTMON, IPTDAY, IPTHR, IPTMIN, IPTSEC

      DCALL = ' '
      TCALL = ' '

C     Call date and time routine
      CALL DATE_AND_TIME (CDATE, CTIME, CZONE, IDATETIME)

C     Convert year to two digits and store array variables
      IPTYR  = IDATETIME(1) - 100 * INT(IDATETIME(1)/100)
      IPTMON = IDATETIME(2)
      IPTDAY = IDATETIME(3)
      IPTHR  = IDATETIME(5)
      IPTMIN = IDATETIME(6)
      IPTSEC = IDATETIME(7)

C     Write Date and Time to Character Variables, DCALL & TCALL
      WRITE(DCALL, '(2(I2.2,"/"),I2.2)' ) IPTMON, IPTDAY, IPTYR
      WRITE(TCALL, '(2(I2.2,":"),I2.2)' ) IPTHR, IPTMIN, IPTSEC

      RETURN
      END

      SUBROUTINE FILOPN (LENGTH,INPFIL,OUTFIL,CNCINP1,CNCINP2,CNCOUT)
C***********************************************************************
C                 FILOPN Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    December 6, 1994
C
C        INPUTS:  Input filename, INPFIL
C                 Output filename, OUTFIL
C
C        OUTPUTS: Openned files
C
C        CALLED FROM:  HEADER
C
C        ERROR HANDLING:   Checks errors openning files
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      INTEGER LENGTH
      CHARACTER (LEN=LENGTH) :: INPFIL, OUTFIL, CNCINP1, CNCINP2, CNCOUT
      CHARACTER DUMMY*8

      SAVE

C     OPEN Input Runstream File, Unit = 5
      DUMMY = 'RUN-STRM'
      OPEN (UNIT=5,FILE=INPFIL,ERR=99,STATUS='OLD')

C     OPEN Print Output File, Unit = 6
      DUMMY = 'OUTPUT'
CLF90 The CARRIAGECONTROL specifier in the following statement is a
CLF90 non-standard Lahey language extension (also supported by DEC VF),
CLF90 and may need to be removed for portability of the code.
      OPEN (UNIT=6,FILE=OUTFIL,CARRIAGECONTROL='FORTRAN',
     &      ERR=99,STATUS='UNKNOWN')

C     OPEN First Concentration Input Data File, Unit = 10
      DUMMY = 'CONCINP1'
      OPEN (UNIT=10,FILE=CNCINP1,FORM='UNFORMATTED',ERR=99,
     &      STATUS='OLD')

C     OPEN Second Concentration Input Data File, Unit = 11
      DUMMY = 'CONCINP2'
      OPEN (UNIT=11,FILE=CNCINP2,FORM='UNFORMATTED',ERR=99,
     &      STATUS='OLD')

C     OPEN Concentration Output Data File, Unit = 20
      DUMMY = 'CONCOUT'
      OPEN (UNIT=20,FILE=CNCOUT,FORM='UNFORMATTED',ERR=99,
     &      STATUS='UNKNOWN')

      GO TO 1000

C     WRITE Error Message:  Error Opening File
 99   WRITE(*,*) 'Error Opening File: ', DUMMY
      STOP

 1000 CONTINUE

      RETURN
      END

CPES  End PES Code Changes
C
      SUBROUTINE OUTPT(IDAYHR,ICD,IMET2,NREC,CHIS,LCOMPR)               BPS03770
C                                                                       BPS03780
C                                                                       BPS03790
      REAL CHIS(NREC)                                                   BPS03800
      LOGICAL LCOMPR                                                    BPS03810
C     ICODE IN OUTPUT FILE IS 999                                       BPS03820
      ICD1=ICD/1000                                                     BPS03830
      ICD=ICD1*1000+999                                                 BPS03840
      IF(LCOMPR)GO TO 10                                                BPS03850
      WRITE(20)IDAYHR,ICD,IMET2,CHIS                                    BPS03860
      RETURN                                                            BPS03870
10    CONTINUE                                                          BPS03880
      CALL COMPRS(IDAYHR,ICD,IMET2,NREC,CHIS)                           BPS03890
      RETURN                                                            BPS03900
      END                                                               BPS03910
C
      SUBROUTINE OUT(IDAYHR,ICD,IMET2,II,CHIOUT)                        BPS03920
C                                                                       BPS03930
C                                                                       BPS03940
      REAL CHIOUT(II)                                                   BPS03950
      WRITE(20)IDAYHR,ICD,IMET2,CHIOUT                                  BPS03960
      RETURN                                                            BPS03970
      END                                                               BPS03980
C
      SUBROUTINE GETDAT(IUNIT,NREC,NTA,IJCODE,LCOMPR,LSUM,NSUM,ISCODE,  BPS03990
     1 LSCALE,ASCALE,BSCALE,IDAYHR,ICD,IMET2,CHIS,LEND)                 BPS04000
C                                                                       BPS04010
C                                                                       BPS04020
      REAL CHIA(100),CHIIN(100)                                         BPS04030
      REAL ASCALE(62),BSCALE(62)                                        BPS04040
      REAL CHIS(NREC)                                                   BPS04050
      INTEGER ISCODE(62)                                                BPS04060
      LOGICAL LSUM                                                      BPS04070
      LOGICAL LEND,LTRUE,LCOMPR,LSCALE                                  BPS04080
      DATA LTRUE/.TRUE./                                                BPS04090
      IF(.NOT.LSUM)GO TO 50                                             BPS04100
      INDEX=1                                                           BPS04110
C     NTA IS THE NUMBER OF RECORDS PER HOUR (ASSUMING LCOMPR=.FALSE.)   BPS04120
      DO 10 I=1,NTA                                                     BPS04130
      IF(LCOMPR)GO TO 101                                               BPS04140
C     ARRAY COMPRESSION OPTION NOT USED                                 BPS04150
      READ(IUNIT,END=999)IDAYHR,ICD,IMET2,CHIS                          BPS04160
      GO TO 102                                                         BPS04170
101   CONTINUE                                                          BPS04180
C     ARRAY COMPRESSION OPTION USED                                     BPS04190
      READ(IUNIT,END=999)II                                             BPS04200
      CALL RDXPND(IUNIT,NREC,II,IDAYHR,ICD,IMET2,CHIIN,CHIS)            BPS04210
102   CONTINUE                                                          BPS04220
      ICD1=ICD/1000                                                     BPS04230
      ICODE=ICD-1000*ICD1                                               BPS04240
      IF(ICODE.NE.ISCODE(INDEX))GO TO 10                                BPS04250
      IF(LSCALE)CALL XSCALE(ASCALE,BSCALE,INDEX,NREC,CHIS)              BPS04260
      INDEX=INDEX+1                                                     BPS04270
      IF(INDEX.NE.2)GO TO 8                                             BPS04280
      DO 7 J=1,NREC                                                     BPS04290
7     CHIA(J)=CHIS(J)                                                   BPS04300
      GO TO 10                                                          BPS04310
8     DO 9 J=1,NREC                                                     BPS04320
9     CHIA(J)=CHIA(J)+CHIS(J)                                           BPS04330
10    CONTINUE                                                          BPS04340
      IF(INDEX.EQ.NSUM+1)GO TO 15                                       BPS04350
      WRITE(6,12)NSUM,INDEX,ISCODE                                      BPS04360
12    FORMAT(/////'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION 'BPS04370
     1 ,'OF NSUM OR ISCODE'/'0','NSUM = ',I5,5X,'INDEX = ',I5,5X,       BPS04380
     2 6(/'0',10(I5,2X))/'0',2(I5,2X))                                  BPS04390
C     CALL WAUDIT
      STOP                                                              BPS04400
15    CONTINUE                                                          BPS04410
      DO 17 J=1,NREC                                                    BPS04420
17    CHIS(J)=CHIA(J)                                                   BPS04430
      GO TO 60                                                          BPS04440
50    CONTINUE                                                          BPS04450
      DO 55 I=1,NTA                                                     BPS04460
      IF(LCOMPR)GO TO 105                                               BPS04470
C     ARRAY COMPRESSION OPTION NOT USED                                 BPS04480
      READ(IUNIT,END=999)IDAYHR,ICD,IMET2,CHIS                          BPS04490
      GO TO 106                                                         BPS04500
105   CONTINUE                                                          BPS04510
C     ARRAY COMPRESSION OPTION USED                                     BPS04520
      READ(IUNIT,END=999)II                                             BPS04530
      CALL RDXPND(IUNIT,NREC,II,IDAYHR,ICD,IMET2,CHIIN,CHIS)            BPS04540
106   CONTINUE                                                          BPS04550
      IF(LSCALE)CALL XSCALE(ASCALE,BSCALE,1,NREC,CHIS)                  BPS04560
      ICD1=ICD/1000                                                     BPS04570
      ICODE=ICD-1000*ICD1                                               BPS04580
      IF(ICODE.NE.IJCODE)GO TO 55                                       BPS04590
      NTA2=NTA-I                                                        BPS04600
      IF(NTA2.EQ.0)GO TO 60                                             BPS04610
      DO 54 J=1,NTA2                                                    BPS04620
C     SKIP TWO RECORDS IF USING ARRAY COMPRESSION OPTION                BPS04630
      IF(LCOMPR)READ(IUNIT)                                             BPS04640
      READ(IUNIT)                                                       BPS04650
54    CONTINUE                                                          BPS04660
      GO TO 60                                                          BPS04670
55    CONTINUE                                                          BPS04680
      WRITE(6,56)IJCODE                                                 BPS04690
56    FORMAT(/////'0','EXECUTION TERMINATING -- ERROR IN ',             BPS04700
     1 'THE SPECIFICATION OF IJCODE'/'0','IJCODE = ',I5)                BPS04710
C     CALL WAUDIT
      STOP                                                              BPS04720
60    CONTINUE                                                          BPS04730
      RETURN                                                            BPS04740
999   LEND=LTRUE                                                        BPS04750
      RETURN                                                            BPS04760
      END                                                               BPS04770
C
      SUBROUTINE XSCALE(ASCALE,BSCALE,N,NREC,CHI)                       BPS04780
C                                                                       BPS04790
C                                                                       BPS04800
      REAL ASCALE(62),BSCALE(62)                                        BPS04810
      REAL CHI(NREC)                                                    BPS04820
      A=ASCALE(N)                                                       BPS04830
      B=BSCALE(N)                                                       BPS04840
      DO 100 I=1,NREC                                                   BPS04850
      CHI(I)=A*CHI(I)+B                                                 BPS04860
100   CONTINUE                                                          BPS04870
      RETURN                                                            BPS04880
      END                                                               BPS04890
C
      SUBROUTINE RDXPND(IUNIT,NREC,II,IDAYHR,ICD,IMET2,CHIIN,CHIS)      BPS04900
C                                                                       BPS04910
C                                                                       BPS04920
      REAL CHIS(NREC),CHIIN(II)                                         BPS04930
      READ(IUNIT)IDAYHR,ICD,IMET2,CHIIN                                 BPS04940
      JJ=0                                                              BPS04950
      DO 100 I=1,II                                                     BPS04960
      IF(CHIIN(I).GT.0.0)GO TO 55                                       BPS04970
      NZERO=-CHIIN(I)+0.0001                                            BPS04980
      DO 40 J=1,NZERO                                                   BPS04990
      JJ=JJ+1                                                           BPS05000
      CHIS(JJ)=0.0                                                      BPS05010
40    CONTINUE                                                          BPS05020
      GO TO 100                                                         BPS05030
55    CONTINUE                                                          BPS05040
      JJ=JJ+1                                                           BPS05050
      CHIS(JJ)=CHIIN(I)                                                 BPS05060
100   CONTINUE                                                          BPS05070
      RETURN                                                            BPS05080
      END                                                               BPS05090
C
      SUBROUTINE COMPRS(IDAYHR,ICD,IMET2,NREC,CHIS)                     BPS05100
C                                                                       BPS05110
C                                                                       BPS05120
      REAL CHIS(NREC),CHIOUT(100)                                       BPS05130
C                                                                       BPS05140
C     ARRAY COMPRESSION TECHNIQUE USES NEGATIVE NUMBERS TO FLAG ZEROES  BPS05150
C     FOR EXAMPLE, CHIS=12.5, 12.2, 0.0, 0.0, 0.0, 10.1, 0.0, 15.1,     BPS05160
C     16.7, 0.0, 0.0, 0.0, 0.0, 0.0 IS STORED AS:                       BPS05170
C     CHIOUT=12.5, 12.2, -3., 10.1, -1., 15.1, 16.7, -5.                BPS05180
C     WHERE -3 REPLACES THREE ZEROES, -1 REPLACES ONE ZERO, ETC.        BPS05190
C                                                                       BPS05200
      NZERO=0                                                           BPS05210
      II=0                                                              BPS05220
      DO 100 I=1,NREC                                                   BPS05230
      IF(CHIS(I).NE.0.0)GO TO 50                                        BPS05240
      NZERO=NZERO+1                                                     BPS05250
      GO TO 100                                                         BPS05260
50    CONTINUE                                                          BPS05270
      IF(NZERO.EQ.0)GO TO 70                                            BPS05280
      II=II+1                                                           BPS05290
      CHIOUT(II)=-NZERO                                                 BPS05300
      NZERO=0                                                           BPS05310
70    CONTINUE                                                          BPS05320
      II=II+1                                                           BPS05330
      CHIOUT(II)=CHIS(I)                                                BPS05340
100   CONTINUE                                                          BPS05350
      IF(NZERO.EQ.0)GO TO 105                                           BPS05360
      II=II+1                                                           BPS05370
      CHIOUT(II)=-NZERO                                                 BPS05380
105   CONTINUE                                                          BPS05390
      WRITE(20)II                                                       BPS05400
      CALL OUT(IDAYHR,ICD,IMET2,II,CHIOUT)                              BPS05410
      RETURN                                                            BPS05420
      END                                                               BPS05430
C
      SUBROUTINE XWRT2(IRUN,NSCALE,ISCODE)                              BPS05440
C                                                                       BPS05450
C                                                                       BPS05460
      INTEGER ISCODE(1)                                                 BPS05470
      DO 100 I=1,NSCALE                                                 BPS05480
      ITMP=ISCODE(I)                                                    BPS05490
      IF(ITMP.LE.10)WRITE(6,10)IRUN,ITMP                                BPS05500
10    FORMAT(9X,I1,6X,'LINE ',I2)                                       BPS05510
      IF(ITMP.EQ.11)WRITE(6,11)IRUN                                     BPS05520
11    FORMAT(9X,I1,6X,'ALL LINES')                                      BPS05530
      JTMP=ITMP-100                                                     BPS05540
      IF(ITMP.GE.101.AND.ITMP.LE.150)WRITE(6,12)IRUN,JTMP               BPS05550
12    FORMAT(9X,I1,6X,'POINT ',I2)                                      BPS05560
      IF(ITMP.EQ.151)WRITE(6,13)IRUN                                    BPS05570
13    FORMAT(9X,I1,6X,'ALL POINTS')                                     BPS05580
      IF(ITMP.EQ.999)WRITE(6,14)IRUN                                    BPS05590
14    FORMAT(9X,I1,6X,'ALL SOURCES')                                    BPS05600
100   CONTINUE                                                          BPS05610
      RETURN                                                            BPS05620
      END                                                               BPS05630
C
      SUBROUTINE XWRT(NSCALE,ASCALE,BSCALE,ISCALE)                      BPS05640
C                                                                       BPS05650
C                                                                       BPS05660
      REAL ASCALE(62),BSCALE(62)                                        BPS05670
      INTEGER ISCALE(62)                                                BPS05680
      DO 100 IA=1,NSCALE                                                BPS05690
      ITMP=ISCALE(IA)                                                   BPS05700
      IF(ITMP.LE.10)WRITE(6,3224)ITMP,ITMP,ASCALE(IA),BSCALE(IA)        BPS05710
3224  FORMAT(1X,'LINE ',I2,12X,I3,8X,F9.4,6X,F9.4)                      BPS05720
      IF(ITMP.EQ.11)WRITE(6,3225)ITMP,ASCALE(IA),BSCALE(IA)             BPS05730
3225  FORMAT(1X,'ALL LINES',10X,I3,8X,F9.4,6X,F9.4)                     BPS05740
      JTMP=ITMP-100                                                     BPS05750
      IF(ITMP.GE.101.AND.ITMP.LE.150)WRITE(6,3226)JTMP,ITMP,ASCALE(IA), BPS05760
     1 BSCALE(IA)                                                       BPS05770
3226  FORMAT(1X,'POINT ',I2,11X,I3,8X,F9.4,6X,F9.4)                     BPS05780
      IF(ITMP.EQ.151)WRITE(6,3227)ITMP,ASCALE(IA),BSCALE(IA)            BPS05790
3227  FORMAT(1X,'ALL POINTS',9X,I3,8X,F9.4,6X,F9.4)                     BPS05800
      IF(ITMP.EQ.999)WRITE(6,3228)ITMP,ASCALE(IA),BSCALE(IA)            BPS05810
3228  FORMAT(1X,'ALL SOURCES',8X,I3,8X,F9.4,6X,F9.4)                    BPS05820
100   CONTINUE                                                          BPS05830
      RETURN                                                            BPS05840
      END                                                               BPS05850
CPES  Begin PES Code Changes

C
      BLOCK DATA
C
C
      CHARACTER RUNDAT*8, RUNTIM*8, VERSN*5
      COMMON/DATETIME/ RUNDAT, RUNTIM, VERSN
      DATA RUNDAT/' '/, RUNTIM/' '/, VERSN/'99176'/
      END

CPES  End PES Code Changes
