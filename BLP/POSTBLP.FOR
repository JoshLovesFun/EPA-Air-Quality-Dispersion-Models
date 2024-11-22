C***********************************************************************PBP00005
C                                                                       PBP00006
C                        POSTBLP (DATED 99176)                          PBP00010
C                                                                       PBP00060
C             *** SEE BLP MODEL CHANGE BULLETIN MCB#3 ***               PBP00061
C                                                                       PBP00062
C    ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD     PBP00063
C                                                                       PBP00064
C                             919-541-5742                              PBP00065
C                                                                       PBP00066
C***********************************************************************PBP00070
C                                                                       PBP00080
C      POSTBLP -- MULTIPLE BUOYANT LINE AND POINT SOURCE                PBP00090
C                 DISPERSION MODEL (BLP) POST-PROCESSOR                 PBP00100
C                                                                       PBP00110
C                                                                       PBP00130
C      DEVELOPED BY:                                                    PBP00140
C                                                                       PBP00150
C      JOE SCIRE AND LLOYD SCHULMAN                                     PBP00160
C      ENVIRONMENTAL RESEARCH AND TECHNOLOGY                            PBP00170
C      696 VIRGINIA ROAD                                                PBP00180
C      CONCORD, MASSACHUSETTS  01742                                    PBP00190
C                                                                       PBP00200
C***********************************************************************PBP00210
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
C                                                                       PBP00220
      REAL CHIS(100),CHI3(100),CHI24(100),CHIANN(100),CHIN(100)         PBP00230
      REAL T50C1(50),T5RC1(100,5)                                       PBP00240
      REAL T50C3(50),T5RC3(100,5)                                       PBP00250
      REAL T50C24(50),T5RC24(100,5)                                     PBP00260
      REAL T50CN(50),T5RCN(100,5)                                       PBP00270
      REAL THRE(3)                                                      PBP00280
      CHARACTER*4 TITLE(20)                                             PBP00290
      CHARACTER*4 ALPYES,ALPAR
      CHARACTER*4 ALP1,ALP2,ALP3,ALP4,ALP5,ALP6,ALP7,ALP8,ALP9,ALP10
      INTEGER I50C1(50),J50C1(50),K50C1(50)                             PBP00300
      INTEGER I5RC1(100,5)                                              PBP00310
      INTEGER I50C3(50),I5RC3(100,5)                                    PBP00320
      INTEGER I50C24(50),I5RC24(100,5)                                  PBP00330
      INTEGER I50CN(50),I5RCN(100,5)                                    PBP00340
      INTEGER IPCL(11),IPCP(51)                                         PBP00350
      INTEGER KDAY(5),KHR(5)                                            PBP00360
      INTEGER IDM(12,2),IFM(12)                                         PBP00370
      INTEGER ITHRE(3)                                                  PBP00380
      DIMENSION IDAYS(366),IECHO(366)                                   PBP00390
      LOGICAL LFRQ1,LFRQ3,LFRQ24,LFRQN                                  PBP00400
      LOGICAL LECH1,LECH3,LECH24,LECHN                                  PBP00410
      LOGICAL LSUM                                                      PBP00420
      LOGICAL LEND/.FALSE./                                             PBP00430
      LOGICAL LFRQ/.FALSE./,LTRUE/.TRUE./                               PBP00440
      LOGICAL LDCALM/.FALSE./,LPRT/.FALSE./,LCALM/.FALSE./,LT/.TRUE./,  PBP00450
     1 LF/.FALSE./                                                      PBP00460
      LOGICAL LSCALE,LCOMPR                                             PBP00470
      EQUIVALENCE (CHILOW,T50C1(50)),(CHIL3,T50C3(50))                  PBP00480
      EQUIVALENCE (CHIL24,T50C24(50))                                   PBP00490
      EQUIVALENCE (CHILN,T50CN(50))                                     PBP00500
      COMMON/ACODE/IYR,IDAY,IHOUR,ICODE,ISTAB,WS,IWD,IDPBL              PBP00510
      COMMON/BCODE/ICD,IMET2,ICD1,ICD3,IDAYHR                           PBP00520
      COMMON/FRDIST/XINT(25),XINT3(25),XINT24(25),XINTN(25),            PBP00530
     1 NINT,NINT3,NINT24,NINTN,                                         PBP00540
     1 INTM(25,12),INTY(25),INTM3(25,12),INTY3(25),INTM24(25,12),       PBP00550
     2 INTY24(25),INTMN(25,12),INTYN(25),IRECEP                         PBP00560
      COMMON/CHIPRT/NTA,NSUM,ISCODE(62),LSUM,IJCODE                     PBP00570
C     COMMON/QA/VERSON,LEVEL                                            PBP00580
      COMMON/SCALE/ASCALE(62),BSCALE(62),ISCALE(62),LSCALE              PBP00590
      NAMELIST/OPTS/XINT,XINT3,XINT24,XINTN,NINT,NINT3,NINT24,NINTN,    PBP00600
     1 LFRQ1,LFRQ3,LFRQ24,LFRQN,IRECEP,IECHO,LECH1,LECH3,LECH24,LECHN,  PBP00610
     2 NSUM,ISCODE,LSUM,IJCODE                                          PBP00620
     3,LDCALM,LPRT,MIN3,MIN24,NAVG,MINN,LSCALE                          PBP00630
      NAMELIST/ECH1/ISCALE,ASCALE,BSCALE                                PBP00640
      DATA T50C1/50*0.0/,T5RC1/500*0.0/                                 PBP00650
      DATA I50C1/50*0/,J50C1/50*0/,K50C1/50*0/                          PBP00660
      DATA T50C3/50*0.0/,T5RC3/500*0.0/                                 PBP00670
      DATA I50C3/50*0/,I5RC3/500*0/                                     PBP00680
      DATA T50C24/50*0.0/,T5RC24/500*0.0/                               PBP00690
      DATA I50C24/50*0/,I5RC24/500*0/                                   PBP00700
      DATA T50CN/50*0.0/,T5RCN/500*0.0/                                 PBP00710
      DATA CHIANN/100*0.0/,IDANN/0/                                     PBP00720
      DATA IDM/31,60,91,121,152,182,213,244,274,305,335,366,            PBP00730
     1 31,59,90,120,151,181,212,243,273,304,334,365/                    PBP00740
      DATA IECHO/366*0/,IFM/12*0/                                       PBP00750
      DATA LFRQ1/.FALSE./,LFRQ3/.FALSE./,LFRQ24/.FALSE./,LFRQN/.FALSE./ PBP00760
      DATA LECH1/.FALSE./,LECH3/.FALSE./,LECH24/.FALSE./,LECHN/.FALSE./ PBP00770
      DATA ALPYES/'YES'/,ALP1/'NO'/,ALP2/'NO'/,ALP3/'NO'/               PBP00780
      DATA ALP4/'NO'/,ALP5/'NO'/,ALP6/'NO'/                             PBP00790
      DATA ALP7/'NO'/,ALP8/'NO'/,ALP9/'NO'/,ALP10/'NO'/                 PBP00800
      DATA ALPAR/'('/                                                   PBP00810
      DATA MIN3/3/,MIN24/18/,NL/100/,NGANN/0/                           PBP00820
      DATA NAVG/0/,MINN/0/,NHRN/0/                                      PBP00830
CPES  Begin PES Code Changes

C     Declare ILEN_FLD Parameter, which controls length of filenames
C     Also declare variables for input and output filenames, version date
C     and model run time and date.
      INTEGER, PARAMETER :: ILEN_FLD = 80
      CHARACTER (LEN=ILEN_FLD) :: INPFIL, OUTFIL, CNCFIL
      COMMON/IOFILE/ INPFIL, OUTFIL, CNCFIL
      CHARACTER RUNDAT*8, RUNTIM*8, VERSN*5
      COMMON/DATETIME/ RUNDAT, RUNTIM, VERSN

C     Get Date and Time using system-specific functions     ---   CALL DATIME
      CALL DATIME (RUNDAT, RUNTIM)

C     Retrieve Input and Output File Names From Command Line,
C                                                           ---   CALL GETCOM
      CALL GETCOM ('POSTBLP ',ILEN_FLD,INPFIL,OUTFIL,CNCFIL)

C     Open Input and Output Files                           ---   CALL FILOPN
      CALL FILOPN (ILEN_FLD,INPFIL,OUTFIL,CNCFIL)

      WRITE(6,1250) VERSN, RUNDAT, RUNTIM
1250  FORMAT ('1',21X,'POSTBLP          (DATED ',A5,')',71X,A8/123X,A8/)

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM
1400  FORMAT('1',43X,'BLP POSTPROCESSOR  SCRAM VERSION (DATED ',A5,')',
     1 33X,A8,/,123X,A8 /' ',13('**********'))

CPES  End PES Code Changes
C                                                                       PBP00980
C     READ BLP POSTPROCESSOR USER INPUTS                                PBP00990
C                                                                       PBP01000
      READ(5,OPTS)                                                      PBP01010
      WRITE(6,1402)                                                     PBP01020
1402  FORMAT(///)                                                       PBP01030
      WRITE(6,OPTS)                                                     PBP01040
C                                                                       PBP01050
C     IF USING SCALING OPTION, READ SCALING PARAMETERS                  PBP01060
C                                                                       PBP01070
      IF(.NOT.LSCALE)GO TO 3240                                         PBP01080
      NSCALE=1                                                          PBP01090
      IF(LSUM)NSCALE=NSUM                                               PBP01100
      DO 3239 IA=1,NSCALE                                               PBP01110
      READ(5,3238)ISCALE(IA),ASCALE(IA),BSCALE(IA)                      PBP01120
3238  FORMAT(I3,7X,2F10.5)                                              PBP01130
3239  CONTINUE                                                          PBP01140
3240  CONTINUE                                                          PBP01150
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1402)                                                     PBP01170
      WRITE(6,ECH1)                                                     PBP01180
C                                                                       PBP01190
C     IF NOT USING DECALM OPTION, SET VALUES FOR RELATED PARAMETERS     PBP01200
C                                                                       PBP01210
      IF(LDCALM)GO TO 3004                                              PBP01220
      LPRT=LF                                                           PBP01230
      MIN3=3                                                            PBP01240
      MIN24=24                                                          PBP01250
3004  CONTINUE                                                          PBP01260
C                                                                       PBP01270
C     CHECK RANGE OF NSUM AND VERIFY ISCODE VALUES HAVE BEEN            PBP01280
C     ENTERED IN CORRECT ORDER                                          PBP01290
C                                                                       PBP01300
      IF(.NOT.LSUM)GO TO 3206                                           PBP01310
      IF(NSUM.LE.1)GO TO 3203                                           PBP01320
      NSUMM1=NSUM-1                                                     PBP01330
      DO 3202 IA=1,NSUMM1                                               PBP01340
      IF(ISCODE(IA+1).GT.ISCODE(IA))GO TO 3202                          PBP01350
      WRITE(6,3201)(IN1,ISCODE(IN1),IN1=1,NSUM)                         PBP01360
3201  FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  PBP01370
     1 'OF ISCODE'/'0','VALUES OF SOURCE CODES (ISCODE) MUST BE ',      PBP01380
     2 'ENTERED IN ORDER OF INCREASING NUMERICAL VALUE'/'0','FOR ',     PBP01390
     3 'EXAMPLE, ENTER ISCODE = 1,2,101, NOT ISCODE = 101,2,1 '/'0',    PBP01400
     4 4X,'I',6X,'ISCODE(I)'//62(3X,I2,9X,I3/))                         PBP01410
C     CALL WAUDIT
      STOP                                                              PBP01420
3202  CONTINUE                                                          PBP01430
      GO TO 3206                                                        PBP01440
3203  CONTINUE                                                          PBP01450
      WRITE(6,3204)NSUM,LSUM                                            PBP01460
3204  FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  PBP01470
     1 'OF NSUM'/'0','NSUM MUST BE GE 2 WHEN LSUM IS TRUE'/'0',         PBP01480
     2 'NSUM = ',I3,3X,'LSUM = ',L1)                                    PBP01490
C     CALL WAUDIT
      STOP                                                              PBP01500
3206  CONTINUE                                                          PBP01510
C                                                                       PBP01520
C     CHECK FOR VALID VALUES IN ISCALE ARRAY                            PBP01530
C                                                                       PBP01540
      IF(.NOT.LSCALE)GO TO 3220                                         PBP01550
      IF(LSUM)GO TO 3215                                                PBP01560
C     LSUM = .FALSE.                                                    PBP01570
      IF(ISCALE(1).EQ.IJCODE)GO TO 3220                                 PBP01580
      WRITE(6,3211)LSCALE,LSUM,IJCODE,ISCALE(1)                         PBP01590
3211  FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  PBP01600
     1 'OF IJCODE OR ISCALE'/'0','WHEN LSCALE IS TRUE AND LSUM IS ',    PBP01610
     2 'FALSE, ISCALE(1) MUST EQUAL IJCODE'/'0','LSCALE = ',L1,3X,      PBP01620
     3 'LSUM = ',L1,3X,'IJCODE = ',I3,3X,'ISCALE(1) = ',I3)             PBP01630
C     CALL WAUDIT
      STOP                                                              PBP01640
3215  CONTINUE                                                          PBP01650
C     LSUM = .TRUE.                                                     PBP01660
      DO 3218 IB=1,NSCALE                                               PBP01670
      IF(ISCALE(IB).EQ.ISCODE(IB))GO TO 3218                            PBP01680
      WRITE(6,3216)LSCALE,LSUM,(IC,ISCODE(IC),ISCALE(IC),IC=1,NSCALE)   PBP01690
3216  FORMAT(//'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION ',  PBP01700
     1 'OF ISCODE OR ISCALE'/'0','WHEN LSCALE AND LSUM ARE TRUE, ',     PBP01710
     2 'SOURCE CODES ARRAYS (ISCALE & ISCODE) MUST BE EQUAL AND ',      PBP01720
     3 'SOURCE CODE VALUES MUST BE ENTERED IN'/'0','ORDER OF ',         PBP01730
     3 'INCREASING ',                                                   PBP01740
     4 'NUMERICAL VALUE -- FOR EXAMPLE, ENTER ISCALE = 1,2,101, ',      PBP01750
     5 'NOT ISCALE = 101,2,1'/'0','LSCALE = ',L1,3X,'LSUM = ',L1/'0',   PBP01760
     6 3X,'I',6X,'ISCODE(I)',5X,'ISCALE(I)'//62(3X,I2,9X,I3,11X,I3/))   PBP01770
C     CALL WAUDIT
      STOP                                                              PBP01780
3218  CONTINUE                                                          PBP01790
3220  CONTINUE                                                          PBP01800
C                                                                       PBP01810
C     CHECK RANGES FOR MIN3, MIN24, AND MINN                            PBP01820
C                                                                       PBP01830
      IF(.NOT.LDCALM)GO TO 4002                                         PBP01840
      IF(MIN3.LE.3.AND.MIN3.GE.1)GO TO 3006                             PBP01850
      WRITE(6,3005)MIN3                                                 PBP01860
3005  FORMAT(//'0','EXECUTION TERMINATING - INVALID VALUE OF MIN3 ',    PBP01870
     1 'SPECIFIED'/'0','(MIN3 MUST BE GE 1 AND LE 3) - MIN3 = ',I3)     PBP01880
C     CALL WAUDIT
      STOP                                                              PBP01890
3006  CONTINUE                                                          PBP01900
      IF(MIN24.LE.24.AND.MIN24.GE.1)GO TO 3008                          PBP01910
      WRITE(6,3007)MIN24                                                PBP01920
3007  FORMAT(//'0','EXECUTION TERMINATING - INVALID VALUE OF MIN24 ',   PBP01930
     1 'SPECIFIED'/'0','(MIN24 MUST BE GE 1 AND LE 24) - MIN24 = ',I3)  PBP01940
C     CALL WAUDIT
      STOP                                                              PBP01950
3008  CONTINUE                                                          PBP01960
C     IF NAVG = 0, NO 'NAVG'-HOUR AVERAGE IS CALCULATED                 PBP01970
      IF(NAVG.LE.0)GO TO 4002                                           PBP01980
      IF(MINN.GE.1.AND.MINN.LE.NAVG)GO TO 4002                          PBP01990
      WRITE(6,4001)MINN,NAVG                                            PBP02000
4001  FORMAT(//'0','EXECUTION TERMINATING - INVALID VALUE OF MINN ',    PBP02010
     1 'SPECIFIED'/'0','(MINN MUST BE GE 1 AND LE NAVG) - MINN = ',I5,  PBP02020
     2 3X,'NAVG = ',I5)                                                 PBP02030
C     CALL WAUDIT
      STOP                                                              PBP02040
4002  CONTINUE                                                          PBP02050
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
C                                                                       PBP02070
C     READ TITLE CARD AND OTHER GENERAL INFORMATION                     PBP02080
C     ON RECORD #1 OF THE OUTPUT FILE (20)                              PBP02090
C                                                                       PBP02100
      READ(20)TITLE,NNREC,NPTS,NLINES,IPCL,IPCP,IYR,IDAYS               PBP02110
C     THOUSANDS PLACE OF NNREC IS CODED TO INDICATE IF ARRAY            PBP02120
C     COMPRESSION OPTION IS USED                                        PBP02130
C     IF NNREC > 1000, OUTPUT ARRAYS ARE COMPRESSED                     PBP02140
C     IF NNREC < 1000, OUTPUT ARRAYS ARE NOT COMPRESSED                 PBP02150
      LCOMPR=.FALSE.                                                    PBP02160
      IF(NNREC.GT.1000)LCOMPR=.TRUE.                                    PBP02170
      NREC=MOD(NNREC,1000)                                              PBP02180
      WRITE(6,8)TITLE,IYR                                               PBP02190
8     FORMAT(/'0',20A4,5X,'YEAR: ',I2)                                  PBP02200
C     ILEAP = 1 (LEAP YEAR); ILEAP = 2 (NON-LEAP YEAR)                  PBP02210
      ILEAP=2                                                           PBP02220
      IF(MOD(IYR,4).EQ.0)ILEAP=1                                        PBP02230
      NPH=MIN0(NREC,50)                                                 PBP02240
      NR2=NREC/2                                                        PBP02250
      NPL=0                                                             PBP02260
      DO 121 I=1,11                                                     PBP02270
121   NPL=NPL+IPCL(I)                                                   PBP02280
      NPP=0                                                             PBP02290
      DO 122 I=1,51                                                     PBP02300
122   NPP=NPP+IPCP(I)                                                   PBP02310
C     NTA IS THE NUMBER OF RECORDS FOR EACH HOUR                        PBP02320
      NTA=1+NPL+NPP                                                     PBP02330
      NDYS=0                                                            PBP02340
      DO 135 I=1,366                                                    PBP02350
135   NDYS=NDYS+IDAYS(I)                                                PBP02360
      WRITE(6,136)NDYS,IDAYS                                            PBP02370
136   FORMAT(//'0','TOTAL NUMBER OF DAYS INCLUDED IN THIS RUN: ',I3//   PBP02380
     1 1X,'(0=NOT INCLUDED; 1=INCLUDED)'//                              PBP02390
     2 3('0',10(10I1,3X)/),'0',6(10I1,3X),6I1)                          PBP02400
      NT=NPTS+NLINES                                                    PBP02410
      WRITE(6,112)NT,NLINES,NPTS,NREC                                   PBP02420
112   FORMAT(//'0','TOTAL NUMBER OF SOURCES: ',I3//12X,'LINE SOURCES: ',PBP02430
     1I3/11X,'POINT SOURCES: ',I3///1X,'TOTAL NUMBER OF RECEPTORS: ',I3)PBP02440
      IF(NLINES.LE.0)GO TO 57                                           PBP02450
      WRITE(6,212)                                                      PBP02460
212   FORMAT(//'0','SOURCE CONTRIBUTIONS FROM THE FOLLOWING LINE ',     PBP02470
     1 'SOURCES ARE AVAILABLE: '/'0','(0=NOT AVAILABLE; 1=AVAILABLE)'/  PBP02480
     2 '0','LINE SOURCE NUMBER',5X,'AVAILABILITY')                      PBP02490
      DO 219 I=1,NLINES                                                 PBP02500
      WRITE(6,215)I,IPCL(I)                                             PBP02510
215   FORMAT('0',7X,I2,19X,I1)                                          PBP02520
219   CONTINUE                                                          PBP02530
      WRITE(6,216)NLINES,IPCL(11)                                       PBP02540
216   FORMAT('0',5X,'1 - ',I2,17X,I1)                                   PBP02550
57    CONTINUE                                                          PBP02560
      IF(NPTS.LE.0)GO TO 58                                             PBP02570
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,222)                                                      PBP02590
222   FORMAT(//'0','SOURCE CONTRIBUTIONS FROM THE FOLLOWING ',          PBP02600
     1 'POINT SOURCES ARE AVAILABLE: '/'0','(0=NOT AVAILABLE; ',        PBP02610
     2 '1=AVAILABLE)'/'0','POINT SOURCE NUMBER',5X,'AVAILABILITY')      PBP02620
      DO 239 I=1,NPTS                                                   PBP02630
      WRITE(6,235)I,IPCP(I)                                             PBP02640
235   FORMAT('0',8X,I2,19X,I1)                                          PBP02650
239   CONTINUE                                                          PBP02660
      WRITE(6,236)NPTS,IPCP(51)                                         PBP02670
236   FORMAT('0',6X,'1 - ',I2,17X,I1)                                   PBP02680
58    CONTINUE                                                          PBP02690
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      IF(.NOT.LSUM)WRITE(6,2000)IJCODE                                  PBP02710
2000  FORMAT(//'0',9X,'IJCODE = ',I3)                                   PBP02720
      IF(.NOT.LSUM)WRITE(6,2001)                                        PBP02730
2001  FORMAT('0','(IJCODE SPECIFIES THE BLP CONCENTRATION ',            PBP02740
     1 'OUTPUT DATA USED IN THIS RUN OF POSTBLP.'/2X,'THE ',            PBP02750
     1 'CONCENTRATION DATA ASSOCIATED WITH IJCODE IS AS FOLLOWS: '/     PBP02760
     2 '0',9X,'IJCODE',                                                 PBP02770
     2 18X,'CONCENTRATION DATA'/'0',10X,'1-10',5X,'SOURCE CONTRIBUTION 'PBP02780
     3,'- LINE SOURCE NUMBER "IJCODE"'/12X,'11',6X,'SOURCE ',           PBP02790
     4 'CONTRIBUTION - ALL LINE SOURCES'/10X,'101-150',3X,'SOURCE ',    PBP02800
     5 'CONTRIBUTION - POINT SOURCE NUMBER "IJCODE - 100"'/             PBP02810
     6 11X,'151',6X,'SOURCE CONTRIBUTION - ALL POINT SOURCES'/          PBP02820
     7 11X,'999',6X,'TOTAL CONCENTRATION)')                             PBP02830
      IF(LSUM)WRITE(6,2012)NSUM,(ALPAR,NXX,ISCODE(NXX),NXX=1,NSUM)      PBP02840
2012  FORMAT(//'0','THE BLP CONCENTRATION DATA USED IN THIS RUN OF ',   PBP02850
     1 'POSTBLP IS THE SUM OF THE FOLLOWING ',I3,' SETS'/2X,'OF ',      PBP02860
     2 'SOURCE CONTRIBUTION DATA: '/'0','ISCODE = ',                    PBP02870
     3 9(A1,I1,')',I3,3X),6(/9X,10(A1,I2,')',I3,2X)))                   PBP02880
      IF(LSUM)WRITE(6,2013)                                             PBP02890
2013  FORMAT(/9X,'WHERE THE CONCENTRATION DATA ASSOCIATED WITH EACH ',  PBP02900
     1 'ISCODE IS IDENTIFIED AS FOLLOWS: '/'0',9X,'ISCODE',             PBP02910
     2 18X,'CONCENTRATION DATA'/'0',10X,'1-10',5X,'SOURCE CONTRIBUTION 'PBP02920
     3,'- LINE SOURCE NUMBER "ISCODE"'/12X,'11',6X,'SOURCE ',           PBP02930
     4 'CONTRIBUTION - ALL LINE SOURCES'/10X,'101-150',3X,'SOURCE ',    PBP02940
     5 'CONTRIBUTION - POINT SOURCE NUMBER "ISCODE - 100"'/             PBP02950
     6 11X,'151',6X,'SOURCE CONTRIBUTION - ALL POINT SOURCES'/          PBP02960
     7 11X,'999',6X,'TOTAL CONCENTRATION)')                             PBP02970
      IF(LFRQ1)ALP1=ALPYES                                              PBP02980
      IF(LFRQ3)ALP2=ALPYES                                              PBP02990
      IF(LFRQ24)ALP3=ALPYES                                             PBP03000
      IF(LFRQN)ALP9=ALPYES                                              PBP03010
      IF(NAVG.EQ.0)WRITE(6,2005)ALP1,ALP2,ALP3                          PBP03020
2005  FORMAT(////'0','FREQUENCY DISTRIBUTIONS REQUESTED IN THIS',       PBP03030
     1 ' POSTBLP RUN: '/'0',4X,'1-HOUR AVERAGES ? ',A3/5X,              PBP03040
     2 '3-HOUR AVERAGES ? ',A3/4X,'24-HOUR AVERAGES ? ',A3/1X,I5,       PBP03050
     3 '-HOUR AVERAGES ? ',A3)                                          PBP03060
      IF(NAVG.NE.0)WRITE(6,2005)ALP1,ALP2,ALP3,NAVG,ALP9                PBP03070
      IF(LFRQ1.OR.LFRQ3.OR.LFRQ24.OR.LFRQN)LFRQ=LTRUE                   PBP03080
      IF(LFRQ)WRITE(6,2010)IRECEP                                       PBP03090
2010  FORMAT('0','FREQUENCY DISTRIBUTION(S) FOR RECEPTOR NUMBER ',      PBP03100
     1 I3)                                                              PBP03110
C                                                                       PBP03120
C     WRITE SCALING INFORMATION                                         PBP03130
C                                                                       PBP03140
      IF(.NOT.LSCALE)GO TO 3230                                         PBP03150
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,3221)                                                     PBP03170
3221  FORMAT(/'0','CONCENTRATION SCALING OPTION REQUESTED -- ',         PBP03180
     1 'CONCENTRATIONS WILL BE SCALED IN THE FOLLOWING WAY: '/'0',19X,  PBP03190
     2 'CONC.(NEW) = ASCALE * CONC.(OLD) + BSCALE'/'0','WHERE ASCALE ', PBP03200
     3 'AND BSCALE ARE INPUT BY THE USER FOR EACH SOURCE CONTRIBUTION ',PBP03210
     4 'GROUP'//'0','SOURCE',10X,'SOURCE CODE',6X,'ASCALE',7X,'BSCALE', PBP03220
     5 /2X,'GROUP',12X,'(ISCALE)',19X,'(UG/M**3)'/)                     PBP03230
      DO 3229 IA=1,NSCALE                                               PBP03240
      ITMP=ISCALE(IA)                                                   PBP03250
      IF(ITMP.LE.10)WRITE(6,3224)ITMP,ITMP,ASCALE(IA),BSCALE(IA)        PBP03260
3224  FORMAT(1X,'LINE ',I2,12X,I3,8X,F9.4,6X,F9.4)                      PBP03270
      IF(ITMP.EQ.11)WRITE(6,3225)ITMP,ASCALE(IA),BSCALE(IA)             PBP03280
3225  FORMAT(1X,'ALL LINES',10X,I3,8X,F9.4,6X,F9.4)                     PBP03290
      JTMP=ITMP-100                                                     PBP03300
      IF(ITMP.GE.101.AND.ITMP.LE.150)WRITE(6,3226)JTMP,ITMP,ASCALE(IA), PBP03310
     1 BSCALE(IA)                                                       PBP03320
3226  FORMAT(1X,'POINT ',I2,11X,I3,8X,F9.4,6X,F9.4)                     PBP03330
      IF(ITMP.EQ.151)WRITE(6,3227)ITMP,ASCALE(IA),BSCALE(IA)            PBP03340
3227  FORMAT(1X,'ALL POINTS',9X,I3,8X,F9.4,6X,F9.4)                     PBP03350
      IF(ITMP.EQ.999)WRITE(6,3228)ITMP,ASCALE(IA),BSCALE(IA)            PBP03360
3228  FORMAT(1X,'ALL SOURCES',8X,I3,8X,F9.4,6X,F9.4)                    PBP03370
3229  CONTINUE                                                          PBP03380
3230  CONTINUE                                                          PBP03390
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      IF(LECH1)ALP4=ALPYES                                              PBP03410
      IF(LECH3)ALP5=ALPYES                                              PBP03420
      IF(LECH24)ALP6=ALPYES                                             PBP03430
      IF(LECHN)ALP10=ALPYES                                             PBP03440
      WRITE(6,2015)IECHO                                                PBP03450
2015  FORMAT(//'0','CONCENTRATIONS AT EACH RECEPTOR ARE PRINTED ',      PBP03460
     1 'FOR THE FOLLOWING DAYS: '/'0','(0=NOT PRINTED; 1=PRINTED)'      PBP03470
     2 //3('0',10(10I1,3X)/),'0',6(10I1,3X),6I1)                        PBP03480
      IF(NAVG.EQ.0)WRITE(6,2016)ALP4,ALP5,ALP6                          PBP03490
2016  FORMAT(/'0','AND FOR THE FOLLOWING AVERAGING TIMES: '/            PBP03500
     1 '0',4X,'1-HOUR AVERAGES ? ',A3/5X,'3-HOUR AVERAGES ? ',A3/       PBP03510
     2 4X,'24-HOUR AVERAGES ? ',A3/1X,I5,'-HOUR AVERAGES ? ',A3)        PBP03520
      IF(NAVG.NE.0)WRITE(6,2016)ALP4,ALP5,ALP6,NAVG,ALP10               PBP03530
      IF(NAVG.EQ.0)WRITE(6,5601)                                        PBP03540
5601  FORMAT(/'0','USER-SPECIFIED AVERAGING TIME OPTION NOT USED')      PBP03550
      IF(NAVG.NE.0)WRITE(6,5602)NAVG                                    PBP03560
5602  FORMAT(/'0','USER-SPECIFIED AVERAGING TIME OPTION USED -- ',      PBP03570
     1 'AVERAGING TIME = ',I5,' HOURS')                                 PBP03580
      IF(LDCALM)ALP7=ALPYES                                             PBP03590
      IF(LPRT)ALP8=ALPYES                                               PBP03600
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,3145)ALP7,ALP8,MIN3,MIN24,NAVG,MINN                       PBP03620
3145  FORMAT('0','IF THE DECALM OPTION IS USED, CONCENTRATIONS DURING ',PBP03630
     1 'HOURS WITH WIND SPEEDS LE 1.0 M/S ARE '/                        PBP03640
     2 1X,'TREATED AS MISSING - I.E., THEY ARE NOT USED IN THE ',       PBP03650
     3 'CALCULATION OF 3-HOUR, 24-HOUR, "NAVG"-HOUR, AND'/1X,'ANNUAL ', PBP03660
     3 'AVERAGES. ',                                                    PBP03670
     4 'THESE "MISSING" CONCENTRATIONS ARE ALSO EXCLUDED FROM THE ',    PBP03680
     5 'TABLES OF'/1X,'HIGHEST 1-HOUR AVERAGE CONCENTRATIONS. AT ',     PBP03690
     6 'LEAST "MIN3","MIN24", AND "MINN" NONMISSING HOURS ARE '/1X,     PBP03700
     7 'NECESSARY FOR ANY 3-,24-, AND "NAVG"-HOUR AVERAGE ',            PBP03710
     7 'CONCENTRATION TO BE ',                                          PBP03720
     8 'INCLUDED IN THE TABLES OF HIGHEST'/1X,'CONCENTRATIONS.'/'0',    PBP03730
     9 15X,'OPTIONS USED IN THIS RUN:'/19X,'DECALM OPTION ? ',A3/       PBP03740
     * 19X,'TABLE OF HOURS WITH WS LE 1.0 M/S ? ',A3/'0',18X,           PBP03750
     1 'MINIMUM NUMBER OF HOURS OF DATA NEEDED IN CALCULATION OF ',     PBP03760
     2 '3-HOUR AVERAGES = ',I2/19X,'MINIMUM NUMBER OF HOURS OF DATA ',  PBP03770
     3 'NEEDED IN CALCULATION OF 24-HOUR AVERAGES = ',I2/19X,'MINIMUM ',PBP03780
     4 'NUMBER OF HOURS OF DATA NEEDED IN CALCULATION OF ',I5,          PBP03790
     5 '-HOUR AVERAGES = ',I5)                                          PBP03800
      IMOLST=1                                                          PBP03810
      IMO=1                                                             PBP03820
      DO 1000 JDAY=1,366                                                PBP03830
      IF(IDAYS(JDAY).NE.1)GO TO 1000                                    PBP03840
      IF(JDAY.LE.IDM(IMOLST,ILEAP))GO TO 285                            PBP03850
      IMOLST=IMOLST+1                                                   PBP03860
      DO 282 IJ=IMOLST,12                                               PBP03870
      IF(JDAY.LE.IDM(IMOLST,ILEAP))GO TO 283                            PBP03880
      IMOLST=IMOLST+1                                                   PBP03890
282   CONTINUE                                                          PBP03900
283   IMO=IMOLST                                                        PBP03910
285   CONTINUE                                                          PBP03920
      IFM(IMO)=1                                                        PBP03930
      DO 900 JHR=1,24                                                   PBP03940
      CALL GETDAT(NREC,CHIS,LEND,LCOMPR)                                PBP03950
      IF(LEND)GO TO 1001                                                PBP03960
C                                                                       PBP03970
C     IF DECALM OPTION REQUESTED & WS LE 1.0 M/S, FLAG THIS HOUR        PBP03980
C                                                                       PBP03990
      IF(.NOT.LDCALM)GO TO 3099                                         PBP04000
      LCALM=LF                                                          PBP04010
      IF(WS.GT.1.0)GO TO 3099                                           PBP04020
      LCALM=LT                                                          PBP04030
C                                                                       PBP04040
C     IF LPRT = .TRUE., WRITE HOURS ELIMINATED DUE TO WS LE 1.0 M/S     PBP04050
C                                                                       PBP04060
      IF(.NOT.LPRT)GO TO 3099                                           PBP04070
C     NL INITIALIZED TO 100, SO 1ST TIME THROUGH, WILL WRITE HEADER     PBP04080
      IF(NL.LE.55)GO TO 3101                                            PBP04090
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,3140)                                                     PBP04110
3140  FORMAT('0','DECALM OPTION REQUESTED - THE FOLLOWING HOURS WITH ', PBP04120
     1 'WIND SPEEDS LE 1.0 M/S HAVE BEEN ELIMINATED'/'0','YEAR  DAY  ', PBP04130
     2 'HOUR',4X,'WIND SPEED',3X,'WIND DIR.',3X,'STABILITY',3X,         PBP04140
     3 'MIXING HT.',3X,12('-'),'HIGHEST CONCENTRATIONS - (UG/M**3)',    PBP04150
     4 14('-')/22X,'(M/S)',6X,'(DEGREES)',18X,'(M)',7X,'HIGHEST  RECPT',PBP04160
     5 5X,'2ND HIGHEST  RECPT',5X,'3RD HIGHEST  RECPT'/)                PBP04170
      NL=7                                                              PBP04180
3101  CONTINUE                                                          PBP04190
C                                                                       PBP04200
C     FIND THREE HIGHEST CONCENTRATIONS THIS HOUR                       PBP04210
C                                                                       PBP04220
      DO 3207 IH=1,3                                                    PBP04230
C                                                                       PBP04240
      IRSV=0                                                            PBP04250
      XMAX=0.0                                                          PBP04260
C     THRE ARRAY CONTAINS THE 3 HIGHEST CONCENTRATIONS THIS HOUR        PBP04270
C     ITHRE ARRAY CONTAINS THE RECEPTOR NUMBERS OF THE 3 HIGHEST CONCS. PBP04280
      THRE(IH)=0.0                                                      PBP04290
      ITHRE(IH)=0                                                       PBP04300
C                                                                       PBP04310
      DO 3205 IR=1,NREC                                                 PBP04320
      IF(CHIS(IR).LE.XMAX)GO TO 3205                                    PBP04330
      XMAX=CHIS(IR)                                                     PBP04340
      IRSV=IR                                                           PBP04350
3205  CONTINUE                                                          PBP04360
      IF(IRSV.EQ.0)GO TO 3208                                           PBP04370
      THRE(IH)=XMAX                                                     PBP04380
      ITHRE(IH)=IRSV                                                    PBP04390
      CHIS(IRSV)=-1.                                                    PBP04400
3207  CONTINUE                                                          PBP04410
3208  CONTINUE                                                          PBP04420
C     RESTORE 3 HIGHEST VALUES IN CHIS ARRAY                            PBP04430
      DO 3209 JH=1,3                                                    PBP04440
      IRP=ITHRE(JH)                                                     PBP04450
      IF(IRP.NE.0)CHIS(IRP)=THRE(JH)                                    PBP04460
3209  CONTINUE                                                          PBP04470
      WRITE(6,3141)IYR,IDAY,IHOUR,WS,IWD,ISTAB,IDPBL,(THRE(N),ITHRE(N), PBP04480
     1 N=1,3)                                                           PBP04490
3141  FORMAT(2X,I2,3X,I3,3X,I2,6X,F5.1,9X,I3,11X,I1,9X,I5,6X,           PBP04500
     1 F7.1,3X,I3,8X,F7.1,5X,I3,8X,F7.1,5X,I3)                          PBP04510
      NL=NL+1                                                           PBP04520
3099  CONTINUE                                                          PBP04530
C                                                                       PBP04540
C     ONE HOUR AVERAGES                                                 PBP04550
C                                                                       PBP04560
      IF(LCALM)GO TO 3010                                               PBP04570
      IF(LFRQ1)CALL FREQ(CHIS,IMO,1,0)                                  PBP04580
      IF(LECH1.AND.IECHO(JDAY).EQ.1)CALL OUTDAT(CHIS,IYR,IDAY,JHR,1,    PBP04590
     1 NREC,NR2,1,LDCALM)                                               PBP04600
C                                                                       PBP04610
C     FIND THE TOP 50 ONE HOUR CONCENTRATIONS                           PBP04620
C                                                                       PBP04630
      DO 310 J=1,NREC                                                   PBP04640
C     CHILOW HAS BEEN EQUIVALENCED TO T50C1(50)                         PBP04650
      IF(CHIS(J).LE.CHILOW)GO TO 310                                    PBP04660
      CHIV=CHIS(J)                                                      PBP04670
      ICD2=ICD1*1000+J                                                  PBP04680
C     T50C1 IS THE ARRAY OF THE TOP 50 HOURLY CONCENTRATIONS            PBP04690
C     I50C1 IS A CORRESPONDING ARRAY CONTAINING THE WS, STAB., AND      PBP04700
C     RECEPTOR NUMBER                                                   PBP04710
C     J50C1 IS ANOTHER ARRAY CONTAINING THE WIND DIR. AND DPBL          PBP04720
C     INSERT THE NEW VALUE IN THE ARRAY OF TOP 50 VALUES (T50C1) AND    PBP04730
C     INCREMENT THE APPROPRIATE PORTION OF THE ARRAY UP BY ONE          PBP04740
C     DO THE SAME FOR THE ARRAYS I50C1 AND J50C1                        PBP04750
      IP=24                                                             PBP04760
      MAG=24                                                            PBP04770
      DO 350 I=1,3                                                      PBP04780
      MAG=MAG/2                                                         PBP04790
      ISGN=1                                                            PBP04800
      IF(CHIV.GT.T50C1(IP))ISGN=-1                                      PBP04810
      IP=IP+ISGN*MAG                                                    PBP04820
350   CONTINUE                                                          PBP04830
      IL=IP-2                                                           PBP04840
      IH=IP+3                                                           PBP04850
      DO 360 I=IL,IH                                                    PBP04860
      IF(CHIV.LT.T50C1(I))GO TO 360                                     PBP04870
      IRPLAC=I                                                          PBP04880
      GO TO 361                                                         PBP04890
360   CONTINUE                                                          PBP04900
      IRPLAC=50                                                         PBP04910
      IF(CHIV.GE.T50C1(49))IRPLAC=49                                    PBP04920
361   CONTINUE                                                          PBP04930
      INDEX=50-IRPLAC                                                   PBP04940
      IF(INDEX.EQ.0)GO TO 375                                           PBP04950
      DO 370 I=1,INDEX                                                  PBP04960
      II=50-I                                                           PBP04970
      T50C1(II+1)=T50C1(II)                                             PBP04980
      I50C1(II+1)=I50C1(II)                                             PBP04990
      J50C1(II+1)=J50C1(II)                                             PBP05000
      K50C1(II+1)=K50C1(II)                                             PBP05010
370   CONTINUE                                                          PBP05020
375   CONTINUE                                                          PBP05030
      T50C1(IRPLAC)=CHIV                                                PBP05040
      I50C1(IRPLAC)=ICD2                                                PBP05050
      J50C1(IRPLAC)=IMET2                                               PBP05060
      K50C1(IRPLAC)=IDAYHR                                              PBP05070
310   CONTINUE                                                          PBP05080
C                                                                       PBP05090
C     FIND THE 5 HIGHEST HOURLY CONCENTRATIONS AT EACH RECEPTOR         PBP05100
C                                                                       PBP05110
      DO 410 J=1,NREC                                                   PBP05120
      IF(CHIS(J).LE.T5RC1(J,5))GO TO 410                                PBP05130
      CHIV=CHIS(J)                                                      PBP05140
      DO 415 I=1,4                                                      PBP05150
      IF(CHIV.LT.T5RC1(J,I))GO TO 415                                   PBP05160
      IRPLAC=I                                                          PBP05170
      GO TO 420                                                         PBP05180
415   CONTINUE                                                          PBP05190
      IRPLAC=5                                                          PBP05200
420   CONTINUE                                                          PBP05210
      INDEX=5-IRPLAC                                                    PBP05220
      IF(INDEX.EQ.0)GO TO 435                                           PBP05230
      DO 430 I=1,INDEX                                                  PBP05240
      II=5-I                                                            PBP05250
      T5RC1(J,II+1)=T5RC1(J,II)                                         PBP05260
      I5RC1(J,II+1)=I5RC1(J,II)                                         PBP05270
430   CONTINUE                                                          PBP05280
435   CONTINUE                                                          PBP05290
      T5RC1(J,IRPLAC)=CHIV                                              PBP05300
      I5RC1(J,IRPLAC)=IDAYHR                                            PBP05310
410   CONTINUE                                                          PBP05320
3010  CONTINUE                                                          PBP05330
C                                                                       PBP05340
C     3-HOUR AVERAGES                                                   PBP05350
C                                                                       PBP05360
      IF(MOD(JHR,3).NE.1)GO TO 3502                                     PBP05370
C     FIRST HOUR OF 3-HOUR AVERAGING PERIOD                             PBP05380
      NG3=0                                                             PBP05390
      DO 3500 IR=1,NREC                                                 PBP05400
3500  CHI3(IR)=0.0                                                      PBP05410
3502  CONTINUE                                                          PBP05420
C     IF DECALM OPTION SPECIFIED AND THIS HOUR CALM,                    PBP05430
C     DO NOT INCLUDE IT IN CALCULATION OF 3-HR AVE.                     PBP05440
C     (LCALM CAN BE .TRUE. ONLY IF LDCALM IS ALSO .TRUE.)               PBP05450
      IF(LCALM)GO TO 3503                                               PBP05460
      NG3=NG3+1                                                         PBP05470
      DO 3501 IR=1,NREC                                                 PBP05480
3501  CHI3(IR)=CHI3(IR)+CHIS(IR)                                        PBP05490
3503  CONTINUE                                                          PBP05500
      IF(MOD(JHR,3).NE.0)GO TO 599                                      PBP05510
C     AT LEAST MIN3 HOURS NEEDED TO CALCULATE 3-HR AVE.                 PBP05520
      IF(NG3.LT.MIN3)GO TO 599                                          PBP05530
      XNG3=FLOAT(NG3)                                                   PBP05540
      DO 3505 IR=1,NREC                                                 PBP05550
3505  CHI3(IR)=CHI3(IR)/XNG3                                            PBP05560
      ICD4=IDAY*100+JHR                                                 PBP05570
      IF(LFRQ3)CALL FREQ(CHI3,IMO,3,0)                                  PBP05580
      IF(LECH3.AND.IECHO(JDAY).EQ.1)CALL OUTDAT(CHI3,IYR,IDAY,JHR,3,    PBP05590
     1 NREC,NR2,NG3,LDCALM)                                             PBP05600
C                                                                       PBP05610
C     FIND THE TOP 50 3-HOUR AVERAGE CONCENTRATIONS                     PBP05620
C                                                                       PBP05630
      DO 510 J=1,NREC                                                   PBP05640
C     CHIL3 HAS BEEN EQUIVALENCED TO T50C3(50)                          PBP05650
      IF(CHI3(J).LE.CHIL3)GO TO 510                                     PBP05660
      CHIV=CHI3(J)                                                      PBP05670
      IPDATE=ICD4*1000+J                                                PBP05680
      IP=24                                                             PBP05690
      MAG=24                                                            PBP05700
      DO 550 I=1,3                                                      PBP05710
      MAG=MAG/2                                                         PBP05720
      ISGN=1                                                            PBP05730
      IF(CHIV.GT.T50C3(IP))ISGN=-1                                      PBP05740
      IP=IP+ISGN*MAG                                                    PBP05750
550   CONTINUE                                                          PBP05760
      IL=IP-2                                                           PBP05770
      IH=IP+3                                                           PBP05780
      DO 560 I=IL,IH                                                    PBP05790
      IF(CHIV.LT.T50C3(I))GO TO 560                                     PBP05800
      IRPLAC=I                                                          PBP05810
      GO TO 561                                                         PBP05820
560   CONTINUE                                                          PBP05830
      IRPLAC=50                                                         PBP05840
      IF(CHIV.GE.T50C3(49))IRPLAC=49                                    PBP05850
561   CONTINUE                                                          PBP05860
      INDEX=50-IRPLAC                                                   PBP05870
      IF(INDEX.EQ.0)GO TO 575                                           PBP05880
      DO 570 I=1,INDEX                                                  PBP05890
      II=50-I                                                           PBP05900
      T50C3(II+1)=T50C3(II)                                             PBP05910
      I50C3(II+1)=I50C3(II)                                             PBP05920
570   CONTINUE                                                          PBP05930
575   CONTINUE                                                          PBP05940
      T50C3(IRPLAC)=CHIV                                                PBP05950
      I50C3(IRPLAC)=IPDATE                                              PBP05960
510   CONTINUE                                                          PBP05970
C                                                                       PBP05980
C     FIND THE 5 HIGHEST 3-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR PBP05990
C                                                                       PBP06000
      DO 590 J=1,NREC                                                   PBP06010
      IF(CHI3(J).LE.T5RC3(J,5))GO TO 590                                PBP06020
      CHIV=CHI3(J)                                                      PBP06030
      DO 515 I=1,4                                                      PBP06040
      IF(CHIV.LT.T5RC3(J,I))GO TO 515                                   PBP06050
      IRPLAC=I                                                          PBP06060
      GO TO 520                                                         PBP06070
515   CONTINUE                                                          PBP06080
      IRPLAC=5                                                          PBP06090
520   CONTINUE                                                          PBP06100
      INDEX=5-IRPLAC                                                    PBP06110
      IF(INDEX.EQ.0)GO TO 535                                           PBP06120
      DO 530 I=1,INDEX                                                  PBP06130
      II=5-I                                                            PBP06140
      T5RC3(J,II+1)=T5RC3(J,II)                                         PBP06150
      I5RC3(J,II+1)=I5RC3(J,II)                                         PBP06160
530   CONTINUE                                                          PBP06170
535   CONTINUE                                                          PBP06180
      T5RC3(J,IRPLAC)=CHIV                                              PBP06190
      I5RC3(J,IRPLAC)=ICD4                                              PBP06200
590   CONTINUE                                                          PBP06210
599   CONTINUE                                                          PBP06220
C                                                                       PBP06230
C     24-HOUR AVERAGES                                                  PBP06240
C                                                                       PBP06250
      IF(JHR.NE.1)GO TO 3605                                            PBP06260
C     FIRST HOUR OF 24-HOUR AVERAGING PERIOD                            PBP06270
      NG24=0                                                            PBP06280
      DO 3602 NR=1,NREC                                                 PBP06290
3602  CHI24(NR)=0.0                                                     PBP06300
3605  CONTINUE                                                          PBP06310
C     IF DECALM OPTION SPECIFIED AND THIS HOUR CALM,                    PBP06320
C     DO NOT INCLUDE IT IN CALCULATION OF 24-HOUR AVE.                  PBP06330
C     (LCALM CAN BE .TRUE. ONLY IF LDCALM IS ALSO .TRUE.)               PBP06340
      IF(LCALM)GO TO 3603                                               PBP06350
      NG24=NG24+1                                                       PBP06360
      DO 3604 NR=1,NREC                                                 PBP06370
3604  CHI24(NR)=CHI24(NR)+CHIS(NR)                                      PBP06380
3603  CONTINUE                                                          PBP06390
      IF(JHR.NE.24)GO TO 699                                            PBP06400
C     AT LEAST MIN24 HOURS NEEDED TO CALCULATE 24-HR AVE.               PBP06410
      IF(NG24.LT.MIN24)GO TO 699                                        PBP06420
      XNG24=FLOAT(NG24)                                                 PBP06430
      DO 3606 NR=1,NREC                                                 PBP06440
3606  CHI24(NR)=CHI24(NR)/XNG24                                         PBP06450
      IF(LFRQ24)CALL FREQ(CHI24,IMO,24,0)                               PBP06460
      IF(LECH24.AND.IECHO(JDAY).EQ.1)CALL OUTDAT(CHI24,IYR,IDAY,0,24,   PBP06470
     1 NREC,NR2,NG24,LDCALM)                                            PBP06480
C                                                                       PBP06490
C     FIND THE TOP 50 24-HOUR AVERAGE CONCENTRATIONS                    PBP06500
C                                                                       PBP06510
      DO 610 J=1,NREC                                                   PBP06520
C     CHIL24 HAS BEEN EQUIVALENCED TO T50C24(50)                        PBP06530
      IF(CHI24(J).LE.CHIL24)GO TO 610                                   PBP06540
      CHIV=CHI24(J)                                                     PBP06550
      IPDAY=IDAY*1000+J                                                 PBP06560
      IP=24                                                             PBP06570
      MAG=24                                                            PBP06580
      DO 650 I=1,3                                                      PBP06590
      MAG=MAG/2                                                         PBP06600
      ISGN=1                                                            PBP06610
      IF(CHIV.GT.T50C24(IP))ISGN=-1                                     PBP06620
      IP=IP+ISGN*MAG                                                    PBP06630
650   CONTINUE                                                          PBP06640
      IL=IP-2                                                           PBP06650
      IH=IP+3                                                           PBP06660
      DO 660 I=IL,IH                                                    PBP06670
      IF(CHIV.LT.T50C24(I))GO TO 660                                    PBP06680
      IRPLAC=I                                                          PBP06690
      GO TO 651                                                         PBP06700
660   CONTINUE                                                          PBP06710
      IRPLAC=50                                                         PBP06720
      IF(CHIV.GE.T50C24(49))IRPLAC=49                                   PBP06730
651   CONTINUE                                                          PBP06740
      INDEX=50-IRPLAC                                                   PBP06750
      IF(INDEX.EQ.0)GO TO 675                                           PBP06760
      DO 670 I=1,INDEX                                                  PBP06770
      II=50-I                                                           PBP06780
      T50C24(II+1)=T50C24(II)                                           PBP06790
      I50C24(II+1)=I50C24(II)                                           PBP06800
670   CONTINUE                                                          PBP06810
675   CONTINUE                                                          PBP06820
      T50C24(IRPLAC)=CHIV                                               PBP06830
      I50C24(IRPLAC)=IPDAY                                              PBP06840
610   CONTINUE                                                          PBP06850
C                                                                       PBP06860
C     FIND THE 5 HIGHEST 24-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTORPBP06870
C                                                                       PBP06880
      DO 690 J=1,NREC                                                   PBP06890
      IF(CHI24(J).LE.T5RC24(J,5))GO TO 690                              PBP06900
      CHIV=CHI24(J)                                                     PBP06910
      DO 615 I=1,4                                                      PBP06920
      IF(CHIV.LT.T5RC24(J,I))GO TO 615                                  PBP06930
      IRPLAC=I                                                          PBP06940
      GO TO 620                                                         PBP06950
615   CONTINUE                                                          PBP06960
      IRPLAC=5                                                          PBP06970
620   CONTINUE                                                          PBP06980
      INDEX=5-IRPLAC                                                    PBP06990
      IF(INDEX.EQ.0)GO TO 635                                           PBP07000
      DO 630 I=1,INDEX                                                  PBP07010
      II=5-I                                                            PBP07020
      T5RC24(J,II+1)=T5RC24(J,II)                                       PBP07030
      I5RC24(J,II+1)=I5RC24(J,II)                                       PBP07040
630   CONTINUE                                                          PBP07050
635   CONTINUE                                                          PBP07060
      T5RC24(J,IRPLAC)=CHIV                                             PBP07070
      I5RC24(J,IRPLAC)=IDAY                                             PBP07080
690   CONTINUE                                                          PBP07090
699   CONTINUE                                                          PBP07100
C                                                                       PBP07110
C     'NAVG'-HOUR AVERAGES                                              PBP07120
C                                                                       PBP07130
      IF(NAVG.EQ.0)GO TO 5599                                           PBP07140
      NHRN=NHRN+1                                                       PBP07150
      IF(MOD(NHRN,NAVG).NE.1)GO TO 4502                                 PBP07160
C     FIRST HOUR OF 'NAVG'-HOUR AVERAGING PERIOD                        PBP07170
      NGNAVG=0                                                          PBP07180
      DO 4500 IR=1,NREC                                                 PBP07190
4500  CHIN(IR)=0.0                                                      PBP07200
4502  CONTINUE                                                          PBP07210
C     IF DECALM OPTION SPECIFIED AND THIS HOUR CALM, DO NOT INCLUDE IT  PBP07220
C     IN CALCULATION OF 'NAVG'-HOUR AVERAGE                             PBP07230
      IF(LCALM)GO TO 4503                                               PBP07240
      NGNAVG=NGNAVG+1                                                   PBP07250
      DO 4501 IR=1,NREC                                                 PBP07260
4501  CHIN(IR)=CHIN(IR)+CHIS(IR)                                        PBP07270
4503  CONTINUE                                                          PBP07280
      IF(MOD(NHRN,NAVG).NE.0)GO TO 5599                                 PBP07290
C     AT LEAST 'MINN' HOURS NEEDED TO CALCULATE 'NAVG'-HOUR AVERAGE     PBP07300
      IF(NGNAVG.LT.MINN)GO TO 5599                                      PBP07310
      XNGN=FLOAT(NGNAVG)                                                PBP07320
      DO 4505 IR=1,NREC                                                 PBP07330
4505  CHIN(IR)=CHIN(IR)/XNGN                                            PBP07340
      ICDN=IDAY*100+JHR                                                 PBP07350
      IF(LFRQN)CALL FREQ(CHIN,IMO,NAVG,1)                               PBP07360
      IF(.NOT.LECHN)GO TO 4506                                          PBP07370
C     IF NAVG > 24 HOURS, IECHO ARRAY IS MEANINGLESS                    PBP07380
      IF(NAVG.GT.24)CALL OUTDAT(CHIN,IYR,IDAY,JHR,NAVG,NREC,NR2,        PBP07390
     1 NGNAVG,LDCALM)                                                   PBP07400
C     IF NAVG LE 24 HOURS, PRINT 'NAVG'-HR AVERAGES ONLY ON JULIAN      PBP07410
C     DAYS WITH '1' IN CORRESPONDING ELEMENTS OF IECHO ARRAY            PBP07420
      IF(NAVG.LE.24.AND.IECHO(JDAY).EQ.1)CALL OUTDAT(CHIN,IYR,IDAY,     PBP07430
     1 JHR,NAVG,NREC,NR2,NGNAVG,LDCALM)                                 PBP07440
4506  CONTINUE                                                          PBP07450
C                                                                       PBP07460
C     FIND THE TOP 50 'NAVG'-HOUR AVERAGE CONCENTRATIONS                PBP07470
C                                                                       PBP07480
      DO 5510 J=1,NREC                                                  PBP07490
C     CHILN HAS BEEN EQUIVALENCED TO T50CN(50)                          PBP07500
      IF(CHIN(J).LE.CHILN)GO TO 5510                                    PBP07510
      CHIV=CHIN(J)                                                      PBP07520
      IPDATE=ICDN*1000+J                                                PBP07530
      IP=24                                                             PBP07540
      MAG=24                                                            PBP07550
      DO 5550 I=1,3                                                     PBP07560
      MAG=MAG/2                                                         PBP07570
      ISGN=1                                                            PBP07580
      IF(CHIV.GT.T50CN(IP))ISGN=-1                                      PBP07590
      IP=IP+ISGN*MAG                                                    PBP07600
5550  CONTINUE                                                          PBP07610
      IL=IP-2                                                           PBP07620
      IH=IP+3                                                           PBP07630
      DO 5560 I=IL,IH                                                   PBP07640
      IF(CHIV.LT.T50CN(I))GO TO 5560                                    PBP07650
      IRPLAC=I                                                          PBP07660
      GO TO 5561                                                        PBP07670
5560  CONTINUE                                                          PBP07680
      IRPLAC=50                                                         PBP07690
      IF(CHIV.GE.T50CN(49))IRPLAC=49                                    PBP07700
5561  CONTINUE                                                          PBP07710
      INDEX=50-IRPLAC                                                   PBP07720
      IF(INDEX.EQ.0)GO TO 5575                                          PBP07730
      DO 5570 I=1,INDEX                                                 PBP07740
      II=50-I                                                           PBP07750
      T50CN(II+1)=T50CN(II)                                             PBP07760
      I50CN(II+1)=I50CN(II)                                             PBP07770
5570  CONTINUE                                                          PBP07780
5575  CONTINUE                                                          PBP07790
      T50CN(IRPLAC)=CHIV                                                PBP07800
      I50CN(IRPLAC)=IPDATE                                              PBP07810
5510  CONTINUE                                                          PBP07820
C                                                                       PBP07830
C     FIND THE 5 HIGHEST 'NAVG'-HOUR AVERAGE CONCENTRATIONS AT          PBP07840
C     EACH RECEPTOR                                                     PBP07850
C                                                                       PBP07860
      DO 5590 J=1,NREC                                                  PBP07870
      IF(CHIN(J).LE.T5RCN(J,5))GO TO 5590                               PBP07880
      CHIV=CHIN(J)                                                      PBP07890
      DO 5515 I=1,4                                                     PBP07900
      IF(CHIV.LT.T5RCN(J,I))GO TO 5515                                  PBP07910
      IRPLAC=I                                                          PBP07920
      GO TO 5520                                                        PBP07930
5515  CONTINUE                                                          PBP07940
      IRPLAC=5                                                          PBP07950
5520  CONTINUE                                                          PBP07960
      INDEX=5-IRPLAC                                                    PBP07970
      IF(INDEX.EQ.0)GO TO 5535                                          PBP07980
      DO 5530 I=1,INDEX                                                 PBP07990
      II=5-I                                                            PBP08000
      T5RCN(J,II+1)=T5RCN(J,II)                                         PBP08010
      I5RCN(J,II+1)=I5RCN(J,II)                                         PBP08020
5530  CONTINUE                                                          PBP08030
5535  CONTINUE                                                          PBP08040
      T5RCN(J,IRPLAC)=CHIV                                              PBP08050
      I5RCN(J,IRPLAC)=ICDN                                              PBP08060
5590  CONTINUE                                                          PBP08070
5599  CONTINUE                                                          PBP08080
C                                                                       PBP08090
C     ANNUAL AVERAGES                                                   PBP08100
C                                                                       PBP08110
      IDANN=IDANN+1                                                     PBP08120
C     IF DECALM OPTION SPECIFIED AND THIS HOUR CALM,                    PBP08130
C     DO NOT INCLUDE IT IN CALCULATION OF ANNUAL AVE.                   PBP08140
C     (LCALM CAN BE .TRUE. ONLY IF LDCALM IS ALSO .TRUE.)               PBP08150
      IF(LCALM)GO TO 3706                                               PBP08160
      NGANN=NGANN+1                                                     PBP08170
      DO 705 J=1,NREC                                                   PBP08180
705   CHIANN(J)=CHIANN(J)+CHIS(J)                                       PBP08190
3706  CONTINUE                                                          PBP08200
900   CONTINUE                                                          PBP08210
1000  CONTINUE                                                          PBP08220
1001  CONTINUE                                                          PBP08230
C                                                                       PBP08240
C     WRITE THE ANNUAL AVERAGE CONCENTRATIONS AT EACH RECEPTOR          PBP08250
C                                                                       PBP08260
      XHRS=NGANN                                                        PBP08270
      XDAYS=XHRS/24.                                                    PBP08280
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1020)XDAYS,NGANN                                          PBP08300
1020  FORMAT('0',35X,F5.1,' DAY (',I4,' HR) AVERAGE CONCENTRATIONS ',   PBP08310
     1 'AT EACH RECEPTOR')                                              PBP08320
      IF(NREC.EQ.1)GO TO 1030                                           PBP08330
      WRITE(6,1021)XDAYS,XDAYS                                          PBP08340
1021  FORMAT(/'0',14X,'RECEPTOR',5X,F5.1,' DAY AVERAGE CONCENTRATION',  PBP08350
     1 15X,'RECEPTOR',5X,F5.1,' DAY AVERAGE CONCENTRATION'/             PBP08360
     2 39X,'(UG/M**3)',50X,'(UG/M**3)')                                 PBP08370
      DO 1025 I=1,NR2                                                   PBP08380
      I2=NR2+I                                                          PBP08390
      CHI1=CHIANN(I)/XHRS                                               PBP08400
      CHI2=CHIANN(I2)/XHRS                                              PBP08410
      WRITE(6,1024)I,CHI1,I2,CHI2                                       PBP08420
1024  FORMAT(17X,I3,18X,F9.1,29X,I3,18X,F9.1)                           PBP08430
1025  CONTINUE                                                          PBP08440
      IF(MOD(NREC,2).EQ.0)GO TO 1040                                    PBP08450
      CHI1=CHIANN(NREC)/XHRS                                            PBP08460
      WRITE(6,1026)NREC,CHI1                                            PBP08470
1026  FORMAT(76X,I3,18X,F9.1)                                           PBP08480
      GO TO 1040                                                        PBP08490
1030  CONTINUE                                                          PBP08500
      WRITE(6,1031)XDAYS                                                PBP08510
1031  FORMAT(/'0',44X,'RECEPTOR',5X,F5.1,' DAY AVERAGE ',               PBP08520
     1 'CONCENTRATION'/69X,'(UG/M**3)')                                 PBP08530
      CHI1=CHIANN(1)/XHRS                                               PBP08540
      WRITE(6,1046)NREC,CHI1                                            PBP08550
1046  FORMAT(47X,I3,18X,F9.1)                                           PBP08560
1040  CONTINUE                                                          PBP08570
      NMISS=IDANN-NGANN                                                 PBP08580
      IF(LDCALM)WRITE(6,3134)IDANN,NMISS,NGANN                          PBP08590
3134  FORMAT('0','TOTAL NUMBER OF HOURS: ',I4/                          PBP08600
     1 1X,'NUMBER OF HOURS WITH WS LE 1.0 M/S: ',I4/                    PBP08610
     2 1X,'NUMBER OF "GOOD" HOURS: ',I4)                                PBP08620
C                                                                       PBP08630
C     WRITE THE TOP 5 1-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR    PBP08640
C                                                                       PBP08650
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1120)                                                     PBP08670
1120  FORMAT('0',26X,'5 HIGHEST 1-HOUR AVERAGE CONCENTRATIONS AT EACH ',PBP08680
     1 'RECEPTOR',2X,'(JULIAN DAY,ENDING HR)'/'0',16X,'RECEPTOR',8X,    PBP08690
     1 'HIGHEST',12X,                                                   PBP08700
     2 '2ND HIGHEST',10X,'3RD HIGHEST',10X,'4TH HIGHEST',10X,           PBP08710
     3 '5TH HIGHEST'/                                                   PBP08720
     4 32X,'(UG/M**3)',12X,3('(UG/M**3)',12X),'(UG/M**3)')              PBP08730
      DO 1126 I=1,NPH                                                   PBP08740
      DO 1124 NN=1,5                                                    PBP08750
      JDAYHR=I5RC1(I,NN)                                                PBP08760
      KDAY(NN)=JDAYHR/100                                               PBP08770
      KHR(NN)=JDAYHR-KDAY(NN)*100                                       PBP08780
1124  CONTINUE                                                          PBP08790
      WRITE(6,1125)I,(T5RC1(I,N),KDAY(N),KHR(N),N=1,5)                  PBP08800
1125  FORMAT(19X,I3,4X,5(F9.1,1X,'(',I3,',',I2,')',3X))                 PBP08810
1126  CONTINUE                                                          PBP08820
      IF(NREC.LE.50)GO TO 1140                                          PBP08830
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1120)                                                     PBP08850
      DO 1136 I=51,NREC                                                 PBP08860
      DO 1134 NN=1,5                                                    PBP08870
      JDAYHR=I5RC1(I,NN)                                                PBP08880
      KDAY(NN)=JDAYHR/100                                               PBP08890
      KHR(NN)=JDAYHR-KDAY(NN)*100                                       PBP08900
1134  CONTINUE                                                          PBP08910
      WRITE(6,1125)I,(T5RC1(I,N),KDAY(N),KHR(N),N=1,5)                  PBP08920
1136  CONTINUE                                                          PBP08930
1140  CONTINUE                                                          PBP08940
C                                                                       PBP08950
C     WRITE THE TOP 5 3-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR    PBP08960
C                                                                       PBP08970
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1220)                                                     PBP08990
1220  FORMAT('0',26X,'5 HIGHEST 3-HOUR AVERAGE CONCENTRATIONS AT EACH ',PBP09000
     1 'RECEPTOR',2X,'(JULIAN DAY,ENDING HR)'/'0',16X,'RECEPTOR',8X,    PBP09010
     1 'HIGHEST',11X,                                                   PBP09020
     2 '2ND HIGHEST',9X,'3RD HIGHEST',9X,'4TH HIGHEST',9X,'5TH HIGHEST'/PBP09030
     3 32X,'(UG/M**3)',11X,3('(UG/M**3)',11X),'(UG/M**3)')              PBP09040
      DO 1226 I=1,NPH                                                   PBP09050
      DO 1224 NN=1,5                                                    PBP09060
      JPDATE=I5RC3(I,NN)                                                PBP09070
      KDAY(NN)=JPDATE/100                                               PBP09080
C     MPERID IS STORED IN THE KHR ARRAY                                 PBP09090
      KHR(NN)=JPDATE-KDAY(NN)*100                                       PBP09100
1224  CONTINUE                                                          PBP09110
      WRITE(6,1225)I,(T5RC3(I,N),KDAY(N),KHR(N),N=1,5)                  PBP09120
1225  FORMAT(19X,I3,4X,5(F9.1,1X,'(',I3,',',I2,')',2X))                 PBP09130
1226  CONTINUE                                                          PBP09140
      IF(NREC.LE.50)GO TO 1240                                          PBP09150
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1220)                                                     PBP09170
      DO 1236 I=51,NREC                                                 PBP09180
      DO 1234 NN=1,5                                                    PBP09190
      JPDATE=I5RC3(I,NN)                                                PBP09200
      KDAY(NN)=JPDATE/100                                               PBP09210
      KHR(NN)=JPDATE-KDAY(NN)*100                                       PBP09220
1234  CONTINUE                                                          PBP09230
      WRITE(6,1225)I,(T5RC3(I,N),KDAY(N),KHR(N),N=1,5)                  PBP09240
1236  CONTINUE                                                          PBP09250
1240  CONTINUE                                                          PBP09260
C                                                                       PBP09270
C     WRITE THE TOP 5 24-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR   PBP09280
C                                                                       PBP09290
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1320)                                                     PBP09310
1320  FORMAT('0',30X,'5 HIGHEST 24-HOUR AVERAGE CONCENTRATIONS AT EACH',PBP09320
     1' RECEPTOR',2X,'(JULIAN DAY)'/'0',16X,'RECEPTOR',8X,'HIGHEST',11X,PBP09330
     2 '2ND HIGHEST',9X,'3RD HIGHEST',9X,'4TH HIGHEST',9X,'5TH HIGHEST'/PBP09340
     3 32X,'(UG/M**3)',11X,3('(UG/M**3)',11X),'(UG/M**3)')              PBP09350
      DO 1326 I=1,NPH                                                   PBP09360
      WRITE(6,1325)I,(T5RC24(I,N),I5RC24(I,N),N=1,5)                    PBP09370
1325  FORMAT(19X,I3,4X,5(F9.1,1X,'(',I3,')',5X))                        PBP09380
1326  CONTINUE                                                          PBP09390
      IF(NREC.LE.50)GO TO 1340                                          PBP09400
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1320)                                                     PBP09420
      DO 1336 I=51,NREC                                                 PBP09430
      WRITE(6,1325)I,(T5RC24(I,N),I5RC24(I,N),N=1,5)                    PBP09440
1336  CONTINUE                                                          PBP09450
1340  CONTINUE                                                          PBP09460
C                                                                       PBP09470
C     WRITE THE TOP 5 'NAVG'-HOUR AVERAGE CONCENTRATIONS AT             PBP09480
C     EACH RECEPTOR                                                     PBP09490
C                                                                       PBP09500
      IF(NAVG.EQ.0)GO TO 5240                                           PBP09510
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,5122)NAVG                                                 PBP09530
5122  FORMAT('0',24X,'5 HIGHEST ',I5,'-HOUR AVERAGE CONCENTRATIONS AT ',PBP09540
     1'EACH RECEPTOR',2X,'(JULIAN DAY,ENDING HR)'/                      PBP09550
     1 '0',16X,'RECEPTOR',8X,'HIGHEST',11X,'2ND HIGHEST',               PBP09560
     2 9X,'3RD HIGHEST',9X,'4TH HIGHEST',9X,'5TH HIGHEST'/32X,          PBP09570
     3 '(UG/M**3)',11X,3('(UG/M**3)',11X),'(UG/M**3)')                  PBP09580
      DO 5226 I=1,NPH                                                   PBP09590
      DO 5224 NN=1,5                                                    PBP09600
      JPDATE=I5RCN(I,NN)                                                PBP09610
      KDAY(NN)=JPDATE/100                                               PBP09620
      KHR(NN)=JPDATE-KDAY(NN)*100                                       PBP09630
5224  CONTINUE                                                          PBP09640
      WRITE(6,1225)I,(T5RCN(I,N),KDAY(N),KHR(N),N=1,5)                  PBP09650
5226  CONTINUE                                                          PBP09660
      IF(NREC.LE.50)GO TO 5240                                          PBP09670
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,5122)NAVG                                                 PBP09690
      DO 5236 I=51,NREC                                                 PBP09700
      DO 5234 NN=1,5                                                    PBP09710
      JPDATE=I5RCN(I,NN)                                                PBP09720
      KDAY(NN)=JPDATE/100                                               PBP09730
      KHR(NN)=JPDATE-KDAY(NN)*100                                       PBP09740
5234  CONTINUE                                                          PBP09750
      WRITE(6,1225)I,(T5RCN(I,N),KDAY(N),KHR(N),N=1,5)                  PBP09760
5236  CONTINUE                                                          PBP09770
5240  CONTINUE                                                          PBP09780
C                                                                       PBP09790
C     WRITE THE TOP 50 1-HOUR CONCENTRATIONS                            PBP09800
C                                                                       PBP09810
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1100)                                                     PBP09830
1100  FORMAT('0',45X,'TOP 50 1-HOUR AVERAGE CONCENTRATIONS')            PBP09840
      WRITE(6,1101)                                                     PBP09850
1101  FORMAT(/'0',10X,'DAY',3X,'HOUR',3X,'WIND SPEED',3X,'WIND DIRECTIONPBP09860
     1',3X,'STABILITY',3X,'MIXING HEIGHT',3X,'RECEPTOR',4X,'1-HOUR ',   PBP09870
     2 'CONCENTRATION'/26X,'(M/S)',9X,'(DEGREES)',19X,'(METERS)',       PBP09880
     3 22X,'(UG/M**3)')                                                 PBP09890
      DO 1110 I=1,50                                                    PBP09900
      JCD2=I50C1(I)                                                     PBP09910
      JMET2=J50C1(I)                                                    PBP09920
      JDAYHR=K50C1(I)                                                   PBP09930
      JWS=JCD2/10000                                                    PBP09940
      JSTAB=JCD2/1000-10*JWS                                            PBP09950
      JRECEP=JCD2-JSTAB*1000-JWS*10000                                  PBP09960
      XWS=FLOAT(JWS)/10.                                                PBP09970
      JWD=JMET2/10000                                                   PBP09980
      JDPBL=JMET2-10000*JWD                                             PBP09990
      JDAY=JDAYHR/100                                                   PBP10000
      JHR=JDAYHR-JDAY*100                                               PBP10010
      WRITE(6,1105)JDAY,JHR,XWS,JWD,JSTAB,JDPBL,JRECEP,T50C1(I)         PBP10020
1105  FORMAT(11X,I3,4X,I2,6X,F4.1,13X,I3,12X,I1,10X,I5,10X,I3,10X,F9.1) PBP10030
1110  CONTINUE                                                          PBP10040
C                                                                       PBP10050
C     WRITE THE TOP 50 3-HOUR CONCENTRATIONS                            PBP10060
C                                                                       PBP10070
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1200)                                                     PBP10090
1200  FORMAT('0',48X,'TOP 50 3-HOUR AVERAGE CONCENTRATIONS')            PBP10100
      WRITE(6,1201)                                                     PBP10110
1201  FORMAT(/'0',40X,'DAY',3X,'ENDING HOUR',3X,'RECEPTOR',3X,          PBP10120
     1 '3-HOUR CONCENTRATION'/75X,'(UG/M**3)')                          PBP10130
      DO 1210 I=1,50                                                    PBP10140
      JPDATE=I50C3(I)                                                   PBP10150
      JDAY=JPDATE/100000                                                PBP10160
      JTIME=JPDATE/1000-JDAY*100                                        PBP10170
      JRECEP=JPDATE-JDAY*100000-JTIME*1000                              PBP10180
      WRITE(6,1215)JDAY,JTIME,JRECEP,T50C3(I)                           PBP10190
1215  FORMAT(41X,I3,7X,I2,10X,I3,8X,F9.1)                               PBP10200
1210  CONTINUE                                                          PBP10210
C                                                                       PBP10220
C     WRITE THE TOP 50 24-HOUR CONCENTRATIONS                           PBP10230
C                                                                       PBP10240
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1300)                                                     PBP10260
1300  FORMAT('0',41X,'TOP 50 24-HOUR AVERAGE CONCENTRATIONS')           PBP10270
      WRITE(6,1301)                                                     PBP10280
1301  FORMAT(/'0',40X,'DAY',3X,'RECEPTOR',3X,'24-HOUR ',                PBP10290
     1 'CONCENTRATION'/63X,'(UG/M**3)')                                 PBP10300
      DO 1310 I=1,50                                                    PBP10310
      JPDAY=I50C24(I)                                                   PBP10320
      JDAY=JPDAY/1000                                                   PBP10330
      JRECEP=JPDAY-JDAY*1000                                            PBP10340
      WRITE(6,1315)JDAY,JRECEP,T50C24(I)                                PBP10350
1315  FORMAT(41X,I3,5X,I3,8X,F9.1)                                      PBP10360
1310  CONTINUE                                                          PBP10370
C                                                                       PBP10380
C     WRITE THE TOP 50 'NAVG'-HOUR CONCENTRATIONS                       PBP10390
C                                                                       PBP10400
      IF(NAVG.EQ.0)GO TO 5211                                           PBP10410
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,5200)NAVG                                                 PBP10430
5200  FORMAT('0',45X,'TOP 50 ',I5,'-HOUR AVERAGE CONCENTRATIONS')       PBP10440
      WRITE(6,5201)NAVG                                                 PBP10450
5201  FORMAT(/'0',40X,'DAY',3X,'ENDING HOUR',3X,'RECEPTOR',1X,          PBP10460
     1 I5,'-HOUR CONCENTRATION'/77X,'(UG/M**3)')                        PBP10470
      DO 5210 I=1,50                                                    PBP10480
      JPDATE=I50CN(I)                                                   PBP10490
      JDAY=JPDATE/100000                                                PBP10500
      JTIME=JPDATE/1000-JDAY*100                                        PBP10510
      JRECEP=JPDATE-JDAY*100000-JTIME*1000                              PBP10520
      WRITE(6,5215)JDAY,JTIME,JRECEP,T50CN(I)                           PBP10530
5215  FORMAT(41X,I3,7X,I2,10X,I3,10X,F9.1)                              PBP10540
5210  CONTINUE                                                          PBP10550
5211  CONTINUE                                                          PBP10560
C                                                                       PBP10570
C     WRITE THE MONTHLY FREQUENCY DISTRIBUTIONS                         PBP10580
C                                                                       PBP10590
      IF(.NOT.LFRQ1)GO TO 1491                                          PBP10600
C     WRITE THE MONTHLY SUMMARIES FOR THE 1-HOUR AVERAGES               PBP10610
      DO 1490 IJ=1,12                                                   PBP10620
      IF(IFM(IJ).NE.1)GO TO 1490                                        PBP10630
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1480)IRECEP,IJ                                            PBP10650
1480  FORMAT('0',15X,'RECEPTOR: ',I3,6X,'FREQUENCY DISTRIBUTION FOR ',  PBP10660
     1 'MONTH: ',I2,6X,                                                 PBP10670
     1 '1-HOUR AVERAGE CONCENTRATIONS (UG/M**3)'///                     PBP10680
     2 '0',24X,'INTERVAL NO.',38X,'HOURS',5X,'PERCENT',5X,'CUMULATIVE') PBP10690
      XHRM=0.0                                                          PBP10700
      DO 3142 INV=1,NINT                                                PBP10710
      XHRM=XHRM+INTM(INV,IJ)                                            PBP10720
3142  CONTINUE                                                          PBP10730
      CALL FRQOUT(NINT,IJ,XHRM,INTM,XINT)                               PBP10740
1490  CONTINUE                                                          PBP10750
1491  CONTINUE                                                          PBP10760
      IF(.NOT.LFRQ3)GO TO 1591                                          PBP10770
C     WRITE THE MONTHLY SUMMARIES FOR THE 3-HOUR AVERAGES               PBP10780
      DO 1590 IJ=1,12                                                   PBP10790
      IF(IFM(IJ).NE.1)GO TO 1590                                        PBP10800
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1580)IRECEP,IJ                                            PBP10820
1580  FORMAT('0',15X,'RECEPTOR: ',I3,6X,'FREQUENCY DISTRIBUTION FOR ',  PBP10830
     1 'MONTH: ',I2,6X,                                                 PBP10840
     1 '3-HOUR AVERAGE CONCENTRATIONS (UG/M**3)'///                     PBP10850
     2'0',24X,'INTERVAL NO.',37X,'PERIODS',4X,'PERCENT',5X,'CUMULATIVE')PBP10860
      XHRM=0.0                                                          PBP10870
      DO 3152 INV=1,NINT3                                               PBP10880
      XHRM=XHRM+INTM3(INV,IJ)                                           PBP10890
3152  CONTINUE                                                          PBP10900
      CALL FRQOUT(NINT3,IJ,XHRM,INTM3,XINT3)                            PBP10910
1590  CONTINUE                                                          PBP10920
1591  CONTINUE                                                          PBP10930
C     WRITE THE MONTHLY SUMMARIES FOR THE 24-HOUR AVERAGES              PBP10940
      IF(.NOT.LFRQ24)GO TO 1691                                         PBP10950
C     WRITE THE MONTHLY SUMMARIES FOR THE 24-HOUR AVERAGES              PBP10960
      DO 1690 IJ=1,12                                                   PBP10970
      IF(IFM(IJ).NE.1)GO TO 1690                                        PBP10980
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1680)IRECEP,IJ                                            PBP11000
1680  FORMAT('0',15X,'RECEPTOR: ',I3,6X,'FREQUENCY DISTRIBUTION FOR ',  PBP11010
     1 'MONTH: ',I2,6X,                                                 PBP11020
     1 '24-HOUR AVERAGE CONCENTRATIONS (UG/M**3)'///                    PBP11030
     2 '0',24X,'INTERVAL NO.',39X,'DAYS',5X,'PERCENT',5X,'CUMULATIVE')  PBP11040
      XHRM=0.0                                                          PBP11050
      DO 3162 INV=1,NINT24                                              PBP11060
      XHRM=XHRM+INTM24(INV,IJ)                                          PBP11070
3162  CONTINUE                                                          PBP11080
      CALL FRQOUT(NINT24,IJ,XHRM,INTM24,XINT24)                         PBP11090
1690  CONTINUE                                                          PBP11100
1691  CONTINUE                                                          PBP11110
      IF(NAVG.EQ.0)GO TO 4591                                           PBP11120
      IF(.NOT.LFRQN)GO TO 4591                                          PBP11130
C     WRITE THE MONTHLY SUMMARIES FOR THE 'NAVG'-HOUR AVERAGES          PBP11140
      DO 4590 IJ=1,12                                                   PBP11150
      IF(IFM(IJ).NE.1)GO TO 4590                                        PBP11160
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,4580)IRECEP,IJ,NAVG                                       PBP11180
4580  FORMAT('0',15X,'RECEPTOR: ',I3,6X,'FREQUENCY DISTRIBUTION FOR ',  PBP11190
     1 'MONTH: ',I2,2X,I5,'-HOUR AVERAGE CONCENTRATIONS (UG/M**3)'///   PBP11200
     2 '0',24X,'INTERVAL NO.',37X,'PERIODS',4X,'PERCENT',5X,            PBP11210
     3 'CUMULATIVE')                                                    PBP11220
      XHRM=0.0                                                          PBP11230
      DO 4152 INV=1,NINTN                                               PBP11240
      XHRM=XHRM+INTMN(INV,IJ)                                           PBP11250
4152  CONTINUE                                                          PBP11260
      CALL FRQOUT(NINTN,IJ,XHRM,INTMN,XINTN)                            PBP11270
4590  CONTINUE                                                          PBP11280
4591  CONTINUE                                                          PBP11290
C                                                                       PBP11300
C     WRITE THE ANNUAL FREQUENCY DISTRIBUTIONS                          PBP11310
C                                                                       PBP11320
      XHRSY=FLOAT(NGANN)                                                PBP11330
      IF(.NOT.LFRQ1)GO TO 1791                                          PBP11340
C     WRITE THE ANNUAL SUMMARY FOR THE 1-HOUR AVERAGES                  PBP11350
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1780)IRECEP,IYR                                           PBP11370
1780  FORMAT('0',15X,'RECEPTOR: ',I3,6X,'FREQUENCY DISTRIBUTION FOR ',  PBP11380
     1 'YEAR: ',I2,6X,                                                  PBP11390
     1 '1-HOUR AVERAGE CONCENTRATIONS (UG/M**3)'///                     PBP11400
     2 '0',24X,'INTERVAL NO.',38X,'HOURS',5X,'PERCENT',5X,'CUMULATIVE') PBP11410
      CALL FRQOUT(NINT,1,XHRSY,INTY,XINT)                               PBP11420
1791  CONTINUE                                                          PBP11430
      IF(.NOT.LFRQ3)GO TO 1891                                          PBP11440
C     WRITE THE ANNUAL SUMMARY FOR THE 3-HOUR AVERAGES                  PBP11450
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1880)IRECEP,IYR                                           PBP11470
1880  FORMAT('0',15X,'RECEPTOR: ',I3,6X,'FREQUENCY DISTRIBUTION FOR ',  PBP11480
     1 'YEAR: ',I2,6X,                                                  PBP11490
     1 '3-HOUR AVERAGE CONCENTRATIONS (UG/M**3)'///                     PBP11500
     2'0',24X,'INTERVAL NO.',37X,'PERIODS',4X,'PERCENT',5X,'CUMULATIVE')PBP11510
      X3=0.0                                                            PBP11520
      DO 3188 INV=1,NINT3                                               PBP11530
      X3=X3+INTY3(INV)                                                  PBP11540
3188  CONTINUE                                                          PBP11550
      CALL FRQOUT(NINT3,1,X3,INTY3,XINT3)                               PBP11560
1891  CONTINUE                                                          PBP11570
      IF(.NOT.LFRQ24)GO TO 1991                                         PBP11580
C     WRITE THE ANNUAL SUMMARY FOR THE 24-HOUR AVERAGES                 PBP11590
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,1980)IRECEP,IYR                                           PBP11610
1980  FORMAT('0',15X,'RECEPTOR: ',I3,6X,'FREQUENCY DISTRIBUTION FOR ',  PBP11620
     1 'YEAR: ',I2,6X,                                                  PBP11630
     1 '24-HOUR AVERAGE CONCENTRATIONS (UG/M**3)'///                    PBP11640
     2 '0',24X,'INTERVAL NO.',39X,'DAYS',5X,'PERCENT',5X,'CUMULATIVE')  PBP11650
      X24=0.0                                                           PBP11660
      DO 3199 INV=1,NINT24                                              PBP11670
      X24=X24+INTY24(INV)                                               PBP11680
3199  CONTINUE                                                          PBP11690
      CALL FRQOUT(NINT24,1,X24,INTY24,XINT24)                           PBP11700
1991  CONTINUE                                                          PBP11710
      IF(NAVG.EQ.0)GO TO 5991                                           PBP11720
      IF(.NOT.LFRQN)GO TO 5991                                          PBP11730
C     WRITE THE ANNUAL AVERAGE SUMMARY FOR THE 'NAVG'-HOUR AVERAGES     PBP11740
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM

CPES  End PES Code Changes
      WRITE(6,5980)IRECEP,IYR,NAVG                                      PBP11760
5980  FORMAT('0',15X,'RECEPTOR: ',I3,6X,'FREQUENCY DISTRIBUTION FOR ',  PBP11770
     1 'YEAR: ',I2,2X,I5,'-HOUR AVERAGE CONCENTRATIONS (UG/M**3)'///    PBP11780
     2 '0',24X,'INTERVAL NO.',39X,'DAYS',5X,'PERCENT',5X,'CUMULATIVE')  PBP11790
      XN=0.0                                                            PBP11800
      DO 5199 INV=1,NINTN                                               PBP11810
      XN=XN+INTYN(INV)                                                  PBP11820
5199  CONTINUE                                                          PBP11830
      CALL FRQOUT(NINTN,1,XN,INTYN,XINTN)                               PBP11840
5991  CONTINUE                                                          PBP11850
C     CALL WAUDIT
      STOP                                                              PBP11860
      END                                                               PBP11870
CPES  Begin PES Code Changes

      SUBROUTINE GETCOM (MODEL,LENGTH,INPFIL,OUTFIL,CNCFIL)
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
      CHARACTER (LEN=LENGTH) :: INPFIL, OUTFIL, CNCFIL
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
      IF (IFCNT .NE. 3) THEN
C        Error on Command Line.  Write Error Message and STOP
         WRITE(*,660) MODEL
         STOP
      END IF
      INPFIL = COMLIN(LOCB(1):LOCE(1))
      OUTFIL = COMLIN(LOCB(2):LOCE(2))
      CNCFIL = COMLIN(LOCB(3):LOCE(3))

C************************************************************LAHEY STOP

  660 FORMAT (' COMMAND LINE ERROR: ',A8,' input_file output_file',
     &        ' concen_file')

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

      SUBROUTINE FILOPN (LENGTH,INPFIL,OUTFIL,CNCFIL)
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
C        ERROR HANDLING:   Checks errors opening files
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      INTEGER LENGTH
      CHARACTER (LEN=LENGTH) :: INPFIL, OUTFIL, CNCFIL
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

C     OPEN Input Concentration Data File, Unit = 20
      DUMMY = 'CONCDATA'
      OPEN (UNIT=20,FILE=CNCFIL,FORM='UNFORMATTED',ERR=99,
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
      SUBROUTINE GETDAT(NREC,CHIS,LEND,LCOMPR)                          PBP11880
C                                                                       PBP11890
C                                                                       PBP11900
      REAL CHIA(100),CHIIN(100)                                         PBP11910
      REAL CHIS(NREC)                                                   PBP11920
      LOGICAL LSUM                                                      PBP11930
      LOGICAL LEND,LTRUE,LCOMPR,LSCALE                                  PBP11940
      COMMON/ACODE/IYR,IDAY,IHOUR,ICODE,ISTAB,WS,IWD,IDPBL              PBP11950
      COMMON/BCODE/ICD,IMET2,ICD1,ICD3,IDAYHR                           PBP11960
      COMMON/CHIPRT/NTA,NSUM,ISCODE(62),LSUM,IJCODE                     PBP11970
      COMMON/SCALE/ASCALE(62),BSCALE(62),ISCALE(62),LSCALE              PBP11980
      DATA LTRUE/.TRUE./                                                PBP11990
      IF(.NOT.LSUM)GO TO 50                                             PBP12000
      INDEX=1                                                           PBP12010
C     NTA IS THE NUMBER OF RECORDS PER HOUR (ASSUMING LCOMPR=.FALSE.)   PBP12020
      DO 10 I=1,NTA                                                     PBP12030
      IF(LCOMPR)GO TO 101                                               PBP12040
C     ARRAY COMPRESSION OPTION NOT USED                                 PBP12050
      READ(20,END=999)IDAYHR,ICD,IMET2,CHIS                             PBP12060
      GO TO 102                                                         PBP12070
101   CONTINUE                                                          PBP12080
C     ARRAY COMPRESSION OPTION USED                                     PBP12090
      READ(20,END=999)II                                                PBP12100
      CALL RDXPND(NREC,II,IDAYHR,ICD,IMET2,CHIIN,CHIS)                  PBP12110
102   CONTINUE                                                          PBP12120
      ICD1=ICD/1000                                                     PBP12130
      ICODE=ICD-1000*ICD1                                               PBP12140
      IF(ICODE.NE.ISCODE(INDEX))GO TO 10                                PBP12150
      IF(LSCALE)CALL XSCALE(INDEX,NREC,CHIS)                            PBP12160
      INDEX=INDEX+1                                                     PBP12170
      IF(INDEX.NE.2)GO TO 8                                             PBP12180
      DO 7 J=1,NREC                                                     PBP12190
7     CHIA(J)=CHIS(J)                                                   PBP12200
      GO TO 10                                                          PBP12210
8     DO 9 J=1,NREC                                                     PBP12220
9     CHIA(J)=CHIA(J)+CHIS(J)                                           PBP12230
10    CONTINUE                                                          PBP12240
      IF(INDEX.EQ.NSUM+1)GO TO 15                                       PBP12250
      WRITE(6,12)NSUM,INDEX,ISCODE                                      PBP12260
12    FORMAT(/////'0','EXECUTION TERMINATING -- ERROR IN SPECIFICATION 'PBP12270
     1 ,'OF NSUM OR ISCODE'/'0','NSUM = ',I5,5X,'INDEX = ',I5,5X,       PBP12280
     2 6(/'0',10(I5,2X))/'0',2(I5,2X))                                  PBP12290
C     CALL WAUDIT
      STOP                                                              PBP12300
15    CONTINUE                                                          PBP12310
      DO 17 J=1,NREC                                                    PBP12320
17    CHIS(J)=CHIA(J)                                                   PBP12330
      GO TO 60                                                          PBP12340
50    CONTINUE                                                          PBP12350
      DO 55 I=1,NTA                                                     PBP12360
      IF(LCOMPR)GO TO 105                                               PBP12370
C     ARRAY COMPRESSION OPTION NOT USED                                 PBP12380
      READ(20,END=999)IDAYHR,ICD,IMET2,CHIS                             PBP12390
      GO TO 106                                                         PBP12400
105   CONTINUE                                                          PBP12410
C     ARRAY COMPRESSION OPTION USED                                     PBP12420
      READ(20,END=999)II                                                PBP12430
      CALL RDXPND(NREC,II,IDAYHR,ICD,IMET2,CHIIN,CHIS)                  PBP12440
106   CONTINUE                                                          PBP12450
      IF(LSCALE)CALL XSCALE(1,NREC,CHIS)                                PBP12460
      ICD1=ICD/1000                                                     PBP12470
      ICODE=ICD-1000*ICD1                                               PBP12480
      IF(ICODE.NE.IJCODE)GO TO 55                                       PBP12490
      NTA2=NTA-I                                                        PBP12500
      IF(NTA2.EQ.0)GO TO 60                                             PBP12510
      DO 54 J=1,NTA2                                                    PBP12520
C     SKIP TWO RECORDS IF USING ARRAY COMPRESSION OPTION                PBP12530
      IF(LCOMPR)READ(20)                                                PBP12540
      READ(20)                                                          PBP12550
54    CONTINUE                                                          PBP12560
      GO TO 60                                                          PBP12570
55    CONTINUE                                                          PBP12580
      WRITE(6,56)IJCODE                                                 PBP12590
56    FORMAT(/////'0','EXECUTION TERMINATING -- ERROR IN ',             PBP12600
     1 'THE SPECIFICATION OF IJCODE'/'0','IJCODE = ',I5)                PBP12610
C     CALL WAUDIT
      STOP                                                              PBP12620
60    CONTINUE                                                          PBP12630
C                                                                       PBP12640
C     DECODE IDAYHR                                                     PBP12650
C                                                                       PBP12660
      IDAY=IDAYHR/100                                                   PBP12670
      IHOUR=IDAYHR-IDAY*100                                             PBP12680
C                                                                       PBP12690
C     DECODE ICD                                                        PBP12700
C                                                                       PBP12710
      IWS=ICD1/10                                                       PBP12720
      WS=FLOAT(IWS)/10.                                                 PBP12730
      ISTAB=ICD1-10*IWS                                                 PBP12740
      ICD3=IDAYHR*10+ISTAB                                              PBP12750
C                                                                       PBP12760
C     DECODE IMET2                                                      PBP12770
C                                                                       PBP12780
      IWD=IMET2/10000                                                   PBP12790
      IDPBL=IMET2-IWD*10000                                             PBP12800
      RETURN                                                            PBP12810
999   LEND=LTRUE                                                        PBP12820
      RETURN                                                            PBP12830
      END                                                               PBP12840
C
      SUBROUTINE OUTDAT(CHI,IYR,IDAY,ITM,IAVG,NREC,NR2,NG,LDCALM)       PBP12850
C                                                                       PBP12860
C                                                                       PBP12870
      REAL CHI(100)                                                     PBP12880
      LOGICAL LDCALM                                                    PBP12890
C     COMMON/QA/VERSON,LEVEL                                            PBP12900
CPES  Begin PES Code Changes

      CHARACTER RUNDAT*8, RUNTIM*8, VERSN*5
      COMMON/DATETIME/ RUNDAT, RUNTIM, VERSN

CPES  End PES Code Changes
      IF(NREC.LT.40)GO TO 1000                                          PBP12910
CPES  Begin PES Code Changes

      WRITE(6,1400) VERSN, RUNDAT, RUNTIM
1400  FORMAT('1',43X,'BLP POSTPROCESSOR  SCRAM VERSION (DATED ',A5,')',
     1 33X,A8,/,123X,A8 /' ',13('**********'))

CPES  End PES Code Changes
      IF(IAVG.EQ.1)WRITE(6,100)IYR,IDAY,ITM,IAVG                        PBP12950
100   FORMAT('0',24X,'YEAR: ',I2,3X,'DAY: ',I3,3X,'HOUR: ',I2,10X,      PBP12960
     1 I2,'-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR')              PBP12970
C     FOR 3-HOUR AND 'NAVG'-HOUR AVERAGES, USE FORMAT 101               PBP12980
      IF(IAVG.NE.1.AND.IAVG.NE.24)WRITE(6,101)IYR,IDAY,ITM,IAVG         PBP12990
101   FORMAT('0',23X,'YEAR: ',I2,3X,'DAY: ',I3,3X,'END HR: ',I2,7X,     PBP13000
     1 I5,'-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR')              PBP13010
      IF(IAVG.EQ.24)WRITE(6,102)IYR,IDAY,IAVG                           PBP13020
102   FORMAT('0',28X,'YEAR: ',I2,3X,'DAY: ',I3,12X,                     PBP13030
     1 I2,'-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR')              PBP13040
      IF(LDCALM.AND.IAVG.NE.1)WRITE(6,3002)NG                           PBP13050
3002  FORMAT(40X,'NUMBER OF HOURS USED IN CALCULATION OF AVERAGE = ',I2)PBP13060
120   CONTINUE                                                          PBP13070
      WRITE(6,121)IAVG,IAVG                                             PBP13080
121   FORMAT('0',14X,'RECEPTOR',6X,I5,'-HR AVERAGE CONCENTRATION',      PBP13090
     1 15X,'RECEPTOR',6X,I5,'-HR AVERAGE CONCENTRATION'/                PBP13100
     2 39X,'(UG/M**3)',50X,'(UG/M**3)')                                 PBP13110
      DO 125 I=1,NR2                                                    PBP13120
      I2=NR2+I                                                          PBP13130
      WRITE(6,124)I,CHI(I),I2,CHI(I2)                                   PBP13140
124   FORMAT(17X,I3,18X,F9.1,29X,I3,18X,F9.1)                           PBP13150
125   CONTINUE                                                          PBP13160
      IF(MOD(NREC,2).EQ.0)RETURN                                        PBP13170
      WRITE(6,126)NREC,CHI(NREC)                                        PBP13180
126   FORMAT(76X,I3,18X,F9.1)                                           PBP13190
      RETURN                                                            PBP13200
1000  CONTINUE                                                          PBP13210
      IF(IAVG.EQ.1)WRITE(6,200)IYR,IDAY,ITM,IAVG                        PBP13220
200   FORMAT(//'0',24X,'YEAR: ',I2,3X,'DAY: ',I3,3X,'HOUR: ',I2,10X,    PBP13230
     1 I2,'-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR')              PBP13240
C     FOR 3-HOUR AND 'NAVG'-HOUR AVERAGES, USE FORMAT 201               PBP13250
      IF(IAVG.NE.1.AND.IAVG.NE.24)WRITE(6,201)IYR,IDAY,ITM,IAVG         PBP13260
201   FORMAT(//'0',23X,'YEAR: ',I2,3X,'DAY: ',I3,3X,'END HR: ',I2,7X,   PBP13270
     1 I5,'-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR')              PBP13280
      IF(IAVG.EQ.24)WRITE(6,202)IYR,IDAY,IAVG                           PBP13290
202   FORMAT(//'0',28X,'YEAR: ',I2,3X,'DAY: ',I3,12X,                   PBP13300
     1 I2,'-HOUR AVERAGE CONCENTRATIONS AT EACH RECEPTOR')              PBP13310
      IF(LDCALM.AND.IAVG.NE.1)WRITE(6,3002)NG                           PBP13320
      IF(NREC.NE.1)GO TO 120                                            PBP13330
      WRITE(6,131)IAVG                                                  PBP13340
131   FORMAT('0',44X,'RECEPTOR',6X,I5,'-HR AVERAGE CONCENTRATION'/      PBP13350
     1 69X,'(UG/M**3)')                                                 PBP13360
      WRITE(6,146)NREC,CHI(1)                                           PBP13370
146   FORMAT(47X,I3,18X,F9.1)                                           PBP13380
      RETURN                                                            PBP13390
      END                                                               PBP13400
C
      SUBROUTINE FREQ(CHI,IMO,IAVG,JCODE)                               PBP13410
C                                                                       PBP13420
C                                                                       PBP13430
      REAL CHI(100)                                                     PBP13440
      COMMON/FRDIST/XINT(25),XINT3(25),XINT24(25),XINTN(25),            PBP13450
     1 NINT,NINT3,NINT24,NINTN,                                         PBP13460
     1 INTM(25,12),INTY(25),INTM3(25,12),INTY3(25),INTM24(25,12),       PBP13470
     2 INTY24(25),INTMN(25,12),INTYN(25),IRECEP                         PBP13480
      XCHI=CHI(IRECEP)                                                  PBP13490
      IF(JCODE.EQ.1)GO TO 117                                           PBP13500
      IF(IAVG.NE.1)GO TO 50                                             PBP13510
      DO 10 J=2,NINT                                                    PBP13520
      IF(XCHI.GE.XINT(J))GO TO 10                                       PBP13530
      JM1=J-1                                                           PBP13540
      INTM(JM1,IMO)=INTM(JM1,IMO)+1                                     PBP13550
      INTY(JM1)=INTY(JM1)+1                                             PBP13560
      GO TO 15                                                          PBP13570
10    CONTINUE                                                          PBP13580
      INTM(NINT,IMO)=INTM(NINT,IMO)+1                                   PBP13590
      INTY(NINT)=INTY(NINT)+1                                           PBP13600
15    CONTINUE                                                          PBP13610
      RETURN                                                            PBP13620
50    CONTINUE                                                          PBP13630
      IF(IAVG.NE.3)GO TO 100                                            PBP13640
      DO 60 J=2,NINT3                                                   PBP13650
      IF(XCHI.GE.XINT3(J))GO TO 60                                      PBP13660
      JM1=J-1                                                           PBP13670
      INTM3(JM1,IMO)=INTM3(JM1,IMO)+1                                   PBP13680
      INTY3(JM1)=INTY3(JM1)+1                                           PBP13690
      GO TO 65                                                          PBP13700
60    CONTINUE                                                          PBP13710
      INTM3(NINT3,IMO)=INTM3(NINT3,IMO)+1                               PBP13720
      INTY3(NINT3)=INTY3(NINT3)+1                                       PBP13730
65    CONTINUE                                                          PBP13740
      RETURN                                                            PBP13750
100   CONTINUE                                                          PBP13760
      IF(IAVG.NE.24)GO TO 117                                           PBP13770
      DO 110 J=2,NINT24                                                 PBP13780
      IF(XCHI.GE.XINT24(J))GO TO 110                                    PBP13790
      JM1=J-1                                                           PBP13800
      INTM24(JM1,IMO)=INTM24(JM1,IMO)+1                                 PBP13810
      INTY24(JM1)=INTY24(JM1)+1                                         PBP13820
      GO TO 115                                                         PBP13830
110   CONTINUE                                                          PBP13840
      INTM24(NINT24,IMO)=INTM24(NINT24,IMO)+1                           PBP13850
      INTY24(NINT24)=INTY24(NINT24)+1                                   PBP13860
115   CONTINUE                                                          PBP13870
      RETURN                                                            PBP13880
117   CONTINUE                                                          PBP13890
C     'NAVG'-HOUR FREQUENCY DISTRIBUTION                                PBP13900
      DO 120 J=2,NINTN                                                  PBP13910
      IF(XCHI.GE.XINTN(J))GO TO 120                                     PBP13920
      JM1=J-1                                                           PBP13930
      INTMN(JM1,IMO)=INTMN(JM1,IMO)+1                                   PBP13940
      INTYN(JM1)=INTYN(JM1)+1                                           PBP13950
      GO TO 125                                                         PBP13960
120   CONTINUE                                                          PBP13970
      INTMN(NINTN,IMO)=INTMN(NINTN,IMO)+1                               PBP13980
      INTYN(NINTN)=INTYN(NINTN)+1                                       PBP13990
125   CONTINUE                                                          PBP14000
      RETURN                                                            PBP14010
      END                                                               PBP14020
C
      SUBROUTINE FRQOUT(NLEVEL,IMO,XHRS,INT,XINT)                       PBP14030
C                                                                       PBP14040
C                                                                       PBP14050
      REAL XINT(25)                                                     PBP14060
      DIMENSION INT(25,12)                                              PBP14070
      NLM1=NLEVEL-1                                                     PBP14080
      PCNT=100./XHRS                                                    PBP14090
      PERCNT=PCNT*INT(1,IMO)                                            PBP14100
      CP=PERCNT                                                         PBP14110
      WRITE(6,1481)XINT(2),INT(1,IMO),PERCNT,CP                         PBP14120
1481  FORMAT(31X,'1',21X,'CHI LT ',F9.1,6X,I4,6X,F6.2,7X,F6.2)          PBP14130
      DO 1485 I=2,NLM1                                                  PBP14140
      PERCNT=PCNT*INT(I,IMO)                                            PBP14150
      CP=CP+PERCNT                                                      PBP14160
      WRITE(6,1484)I,XINT(I),XINT(I+1),INT(I,IMO),PERCNT,CP             PBP14170
1484  FORMAT(29X,I3,7X,F9.1,2X,'LE CHI LT ',F9.1,6X,I4,6X,F6.2,7X,F6.2) PBP14180
1485  CONTINUE                                                          PBP14190
      PERCNT=PCNT*INT(NLEVEL,IMO)                                       PBP14200
      CP=CP+PERCNT                                                      PBP14210
      WRITE(6,1489)NLEVEL,XINT(NLEVEL),INT(NLEVEL,IMO),PERCNT,CP        PBP14220
1489  FORMAT(29X,I3,7X,F9.1,2X,'LE CHI',19X,I4,6X,F6.2,7X,F6.2)         PBP14230
      RETURN                                                            PBP14240
      END                                                               PBP14250
C
      BLOCK DATA                                                        PBP14260
C                                                                       PBP14270
C                                                                       PBP14280
      LOGICAL LSUM,LSCALE                                               PBP14290
      COMMON/FRDIST/XINT(25),XINT3(25),XINT24(25),XINTN(25),            PBP14300
     1 NINT,NINT3,NINT24,NINTN,                                         PBP14310
     1 INTM(25,12),INTY(25),INTM3(25,12),INTY3(25),INTM24(25,12),       PBP14320
     2 INTY24(25),INTMN(25,12),INTYN(25),IRECEP                         PBP14330
      COMMON/CHIPRT/NTA,NSUM,ISCODE(62),LSUM,IJCODE                     PBP14340
      COMMON/SCALE/ASCALE(62),BSCALE(62),ISCALE(62),LSCALE              PBP14350
CPES  Begin PES Code Changes

      CHARACTER RUNDAT*8, RUNTIM*8, VERSN*5
      COMMON/DATETIME/ RUNDAT, RUNTIM, VERSN
      DATA RUNDAT/' '/, RUNTIM/' '/, VERSN/'99176'/

CPES  End PES Code Changes
      DATA XINT/25*0.0/,XINT3/25*0.0/,XINT24/25*0.0/,XINTN/25*0.0/      PBP14360
      DATA NINT/0/,NINT3/0/,NINT24/0/,NINTN/0/                          PBP14370
      DATA INTM/300*0/,INTY/25*0/,INTM3/300*0/,INTY3/25*0/,             PBP14380
     1 INTM24/300*0/,INTY24/25*0/,INTMN/300*0/,INTYN/25*0/              PBP14390
      DATA IRECEP/1/                                                    PBP14400
      DATA NSUM/0/,ISCODE/62*0/,IJCODE/999/                             PBP14410
      DATA LSUM/.FALSE./                                                PBP14420
      DATA ASCALE/62*0.0/,BSCALE/62*0.0/,ISCALE/62*0/                   PBP14430
      DATA LSCALE/.FALSE./                                              PBP14440
      END                                                               PBP14450
C
      SUBROUTINE XSCALE(N,NREC,CHI)                                     PBP14460
C                                                                       PBP14470
C                                                                       PBP14480
      REAL CHI(NREC)                                                    PBP14490
      LOGICAL LSCALE                                                    PBP14500
      COMMON/SCALE/ASCALE(62),BSCALE(62),ISCALE(62),LSCALE              PBP14510
      A=ASCALE(N)                                                       PBP14520
      B=BSCALE(N)                                                       PBP14530
      DO 100 I=1,NREC                                                   PBP14540
      CHI(I)=A*CHI(I)+B                                                 PBP14550
100   CONTINUE                                                          PBP14560
      RETURN                                                            PBP14570
      END                                                               PBP14580
C
      SUBROUTINE RDXPND(NREC,II,IDAYHR,ICD,IMET2,CHIIN,CHIS)            PBP14590
C                                                                       PBP14600
C                                                                       PBP14610
      REAL CHIS(NREC),CHIIN(II)                                         PBP14620
      READ(20)IDAYHR,ICD,IMET2,CHIIN                                    PBP14630
      JJ=0                                                              PBP14640
      DO 100 I=1,II                                                     PBP14650
      IF(CHIIN(I).GT.0.0)GO TO 55                                       PBP14660
      NZERO=-CHIIN(I)+0.0001                                            PBP14670
      DO 40 J=1,NZERO                                                   PBP14680
      JJ=JJ+1                                                           PBP14690
      CHIS(JJ)=0.0                                                      PBP14700
40    CONTINUE                                                          PBP14710
      GO TO 100                                                         PBP14720
55    CONTINUE                                                          PBP14730
      JJ=JJ+1                                                           PBP14740
      CHIS(JJ)=CHIIN(I)                                                 PBP14750
100   CONTINUE                                                          PBP14760
      RETURN                                                            PBP14770
      END                                                               PBP14780
