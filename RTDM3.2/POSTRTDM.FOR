C***********************************************************************ANL00005
C                                                                       ANL00006
C                          POSTRTDM3.2 (DATED 89226)                    ANL00007
C                                                                       ANL00008
C***********************************************************************ANL00009
C                      POSTRTDM - VERSION 2.3 (DATED 87308)             ANL00002
C
C********************************************************************   ANL00001
C       MAIN PROGRAM FOR ANALYSIS                                       ANL00003
C                                                                       ANL00004
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          ANL00005
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          ANL00006
C                                                                       ANL00007
C       ANALYSIS  VERSION 2.3    LEVEL 851125                           ANL00008
C                                                                       ANL00009
C********************************************************************   ANL00010
C                                                                       ANL00011
        COMMON /HEAD/NPAGE,NLINE                                        ANL00012
C       REAL KEYS(5),NINES,ENDJ                                         IBM00013
        CHARACTER*4 KEYS(5),NINES,ENDJ,WORD                             IBM00001
        DATA KEYS /'CUMF','AVER','TOPV','PEAK','SEQA'/                  ANL00014
        DATA NINES/'9999'/,ENDJ/'ENDJ'/                                 ANL00015
C                                                                       ANL00016
C       PRINT HEADER
C
        WRITE(6,10)
   10   FORMAT('1', 21X, 'POSTRTDM3.2           (DATED 89226)',/)
C
        NPAGE = 0                                                       ANL00017
        NLINE = 0                                                       ANL00018
        CALL PAGE                                                       ANL00019
   30   READ(5,1,END=9000)WORD                                          ANL00020
        REWIND 8                                                        ANL00021
    1   FORMAT(A4)                                                      ANL00022
        IF(WORD.EQ.NINES)GO TO 30                                       ANL00023
        IF(WORD.EQ.ENDJ)GO TO 9000                                      ANL00024
        DO 50 I=1,5                                                     ANL00025
        IF(WORD.EQ.KEYS(I))GO TO 70                                     ANL00026
   50   CONTINUE                                                        ANL00027
        WRITE(6,2) WORD                                                 ANL00028
    2   FORMAT(/,'   KEYWORD NOT AMONG THOSE EXPECTED: ',A4)            ANL00029
        GO TO 9000                                                      ANL00030
   70   K=I                                                             ANL00031
C                                                                       ANL00032
       GO TO (100,200,300,400,500), K                                   ANL00033
C                                                                       ANL00034
  100 CALL CUMFRQ                                                       ANL00035
      GO TO 30                                                          ANL00036
C                                                                       ANL00037
  200 CALL AVERGE                                                       ANL00038
      GO TO 30                                                          ANL00039
C                                                                       ANL00040
  300 CALL TOPVAL                                                       ANL00041
      GO TO 30                                                          ANL00042
C                                                                       ANL00043
  400 CALL PEAK                                                         ANL00044
      GO TO 30                                                          ANL00045
C                                                                       ANL00046
  500 CALL SEQADD                                                       ANL00047
      GO TO 30                                                          ANL00048
C                                                                       ANL00049
 9000   CONTINUE                                                        ANL00050
        STOP                                                            ANL00051
        END                                                             ANL00052

      SUBROUTINE PEAK                                                   PEK00001
C********************************************************************   PEK00002
C                                                                       PEK00003
C       THIS SUBROUTINE IS USED TO IDENTIFY NON-OVERLAPPING             PEK00004
C       N-HOUR AVERAGES THAT EXCEED A USER-SPECIFIED THRESHOLD          PEK00005
C                                                                       PEK00006
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          PEK00007
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          PEK00008
C                                                                       PEK00009
C       ANALYSIS  VERSION 2.3    LEVEL 851125                           PEK00010
C                                                                       PEK00011
C********************************************************************   PEK00012
C                                                                       PEK00013
      LOGICAL LPRINT,EOF1                                               PEK00014
C     DIMENSION UNITS(2),NN(400),AIR(4),MET(4,24),                      IBM00015
      DIMENSION NN(400),AIR(4),MET(4,24),                               IBM00002
     1DAY(400),HOURS(400),CONC(400),TOTAL(23,400),                      PEK00016
     2CMAX(400),METNME(4)                                               PEK00017
      INTEGER AIR,DAY,HOURS                                             PEK00018
C     REAL JUNK,METNME                                                  IBM00019
      CHARACTER*4 JUNK,METNME,X1,X2,X3,UNITS(2)                         IBM00003
      DATA METNME /'MIX ','DIR ','STAB','SPD '/                         PEK00020
      DATA JUNK /'    '/                                                PEK00021
      IPUT = 6                                                          PEK00022
      IC = 5                                                            PEK00023
      ICH = 8                                                           PEK00024
      X1 = '----'                                                       PEK00025
      X2 = '+   '                                                       PEK00026
      X3 = '!   '                                                       PEK00027
C                                                                       PEK00028
        WRITE(IPUT,45)                                                  PEK00029
   45   FORMAT(///,'   PEAK',///)                                       PEK00030
C                                                                       PEK00031
      WRITE(IPUT,1)                                                     PEK00032
    1 FORMAT(//,12X,'NHR',7X,'THR',5X,'HOURIN',4X,'DAYSIN',7X,'NH',/)   PEK00033
      READ(IC,2,ERR=8000)NHR,THR,HOURIN,DAYSIN,NH                       PEK00034
    2 FORMAT(10X,I5,3F10.0,I10)                                         PEK00035
      IF(NHR.LT.0)NHR=1                                                 PEK00036
      IF(NH.LT.1) NH = 1                                                PEK00037
      WRITE(IPUT,2) NHR,THR,HOURIN,DAYSIN,NH                            PEK00038
      WRITE(IPUT,3)                                                     PEK00039
    3 FORMAT(///,13X,'RFACT',3X,'UNITS',8X,'LPRINT',3X,'NR  SCALE',/)   PEK00040
      READ(IC,4,ERR=8000)RFACT,UNITS,LPRINT,NR,SCALE                    PEK00041
    4 FORMAT(10X,F10.0,2A4,2X,L10,I5,F10.0)                             PEK00042
      IF(RFACT.LE.0.) RFACT=1.0E+6                                      PEK00043
      IF(SCALE.EQ.0.0) SCALE = 1.0                                      PEK00044
      IF(UNITS(1).EQ.JUNK.AND.UNITS(2).EQ.JUNK) LPRINT=.TRUE.           PEK00045
      WRITE(IPUT,4) RFACT,UNITS,LPRINT,NR,SCALE                         PEK00046
      IF(NR.GE.1.AND.NR.LE.400) GO TO 500                               PEK00047
      WRITE(IPUT,5) NR                                                  PEK00048
    5 FORMAT(2X,'# OF RECEPTORS SPECIFIED IS OUT OF RANGE (1-400): ',I5)PEK00049
      GO TO 9000                                                        PEK00050
  500 IF(NHR.GE.1.AND.NHR.LE.24) GO TO 700                              PEK00051
      WRITE(IPUT,6) NHR                                                 PEK00052
    6 FORMAT(2X,'NHR IS OUT OF RANGE (1-24): ',I4)                      PEK00053
      GO TO 9000                                                        PEK00054
  700 IF(THR.GE.0.)GO TO 800                                            PEK00055
      WRITE(IPUT,8) THR                                                 PEK00056
    8 FORMAT(2X,'THR IS OUT OF BOUNDS ( >0 ): ',E11.4)                  PEK00057
      GO TO 9000                                                        PEK00058
  800 SCALE = SCALE*RFACT                                               PEK00059
      IF(UNITS(1).NE.JUNK) GO TO 1100                                   PEK00060
      UNITS(1)='UG/M'                                                   PEK00061
      UNITS(2)='**3 '                                                   PEK00062
 1100 CONTINUE                                                          PEK00063
      HOURIN=HOURIN+24.*DAYSIN                                          PEK00064
      HOUR=0                                                            PEK00065
      MM=0                                                              PEK00066
      ALL=0                                                             PEK00067
      ICNT = 0                                                          PEK00068
      IF(HOURIN.LE.0.) ALL=1                                            PEK00069
      DO 1500 I = 1,400                                                 PEK00070
      CMAX(I) = 0.0                                                     PEK00071
      DAY(I) = 0.0                                                      PEK00072
      HOURS(I) = 0                                                      PEK00073
      NN(I) = 0                                                         PEK00074
 1500 CONTINUE                                                          PEK00075
      XHR = FLOAT(NHR)                                                  PEK00076
      CALL PAGE                                                         PEK00077
C                                                                       PEK00078
C     START READ LOOP                                                   PEK00079
C                                                                       PEK00080
 1550 IF(ALL.NE.1..AND.HOUR.GE.HOURIN) GO TO 4000                       PEK00081
      DO 2500 I=1,NHR                                                   PEK00082
      IF(ICNT.EQ.1)GO TO 2200                                           PEK00083
      CALL READIT(AIR,CONC,NR,ICH,EOF1)                                 PEK00084
      IF(EOF1) GO TO 8100                                               PEK00085
      ICNT=1                                                            PEK00086
      GO TO 2250                                                        PEK00087
 2200 CALL READIT(AIR,CONC,NR,ICH,EOF1)                                 PEK00088
      IF(EOF1) GO TO 4000                                               PEK00089
 2250 DO 2300 L=1,4                                                     PEK00090
      MET(L,I)=AIR(L)                                                   PEK00091
 2300 CONTINUE                                                          PEK00092
      DO 2350 K=1,NR                                                    PEK00093
      CONC(K)=CONC(K)*SCALE                                             PEK00094
      IF(I.LT.NHR) TOTAL(I,K)=CONC(K)                                   PEK00095
 2350 CONTINUE                                                          PEK00096
 2500 CONTINUE                                                          PEK00097
C                                                                       PEK00098
      MM=MM+1                                                           PEK00099
      HOUR=HOUR+FLOAT(NHR*NH)                                           PEK00100
      DO 3800 K=1,NR                                                    PEK00101
      AVR = 0.0                                                         PEK00102
      DO 2700 I = 1,NHR-1                                               PEK00103
 2700 AVR = AVR + TOTAL(I,K)/XHR                                        PEK00104
      AVR = AVR + CONC(K)/XHR                                           PEK00105
      IF(AVR.LE.CMAX(K)) GO TO 3000                                     PEK00106
      CMAX(K)=AVR                                                       PEK00107
      DAY(K)=INT((HOUR-1.)/24.)+1.                                      PEK00108
      HOURS(K)=AMOD(HOUR-1.,24.)+1                                      PEK00109
 3000 CONTINUE                                                          PEK00110
      IF(AVR.LT.THR) GO TO 3800                                         PEK00111
      NN(K)=NN(K)+1                                                     PEK00112
      IDAY=INT((HOUR-1.)/24.)+1                                         PEK00113
      IHOUR=AMOD(HOUR-1.,24.)+1.                                        PEK00114
      CALL LINES(3)                                                     PEK00115
      IF(LPRINT) CALL LINES(NHR+2)                                      PEK00116
      WRITE(IPUT,11)THR,UNITS,IDAY,IHOUR,K                              PEK00117
   11 FORMAT(/,2X,F10.4,1X,2A4,' EXCEEDED AT DAY ',I4,' HOUR ',I2,2X,   PEK00118
     X'AT RECEPTOR ',I3)                                                PEK00119
      IF(.NOT.(LPRINT))GO TO 3700                                       PEK00120
      WRITE(IPUT,12)UNITS                                               PEK00121
   12 FORMAT('  ****',1X,'CONCENTRATIONS (',2A4,') ',7X,'WEATHER')      PEK00122
      WRITE(IPUT,41) METNME                                             PEK00123
   41 FORMAT('  RECORD',7X,'TOTAL',14X,4(A4,1X))                        PEK00124
      DO 3600 I=1,NHR-1                                                 PEK00125
      WRITE(IPUT,17)I,TOTAL(I,K),(MET(L,I),L=1,4)                       PEK00126
   17 FORMAT(5X,I2,4X,F10.4,11X,4I5)                                    PEK00127
 3600 CONTINUE                                                          PEK00128
      WRITE(IPUT,17) NHR,CONC(K),(MET(L,NHR),L=1,4)                     PEK00129
 3700 WRITE(IPUT,16) AVR                                                PEK00130
   16 FORMAT('  MEAN ',4X,F10.4)                                        PEK00131
 3800 CONTINUE                                                          PEK00132
      GO TO 1550                                                        PEK00133
C                                                                       PEK00134
 4000 CALL PAGE                                                         PEK00135
      WRITE(IPUT,19)UNITS,MM,NHR                                        PEK00136
   19 FORMAT(/,' TOTAL MAXIMUM CONCENTRATIONS (',2A4,') FOR ',I5,' (',  PEK00137
     X I2,'-HOUR) AVERAGING PERIOD(S)',/)                               PEK00138
      CALL LINES(3)                                                     PEK00139
      DO 5000 LSTART = 1,NR,8                                           PEK00140
      CALL LINES(5)                                                     PEK00141
      LEND = MIN0(LSTART+7,NR)                                          PEK00142
      WRITE(IPUT,21)(L,L=LSTART,LEND)                                   PEK00143
   21 FORMAT('  RECEP +',8(2X,I3,3X,'+'))                               PEK00144
      WRITE(IPUT,22) (X1,X1,X2,L=LSTART,LEND)                           PEK00145
   22 FORMAT(' -------+',8(2A4,A1))                                     PEK00146
      WRITE(IPUT,23) (CMAX(K),K=LSTART,LEND)                            PEK00147
   23 FORMAT('   CONC !',8(F8.3,'!'))                                   PEK00148
      WRITE(IPUT,24)(DAY(K),HOURS(K),K=LSTART,LEND)                     PEK00149
   24 FORMAT(' DAY/HR !',8(I4,2X,I2,'!'),/)                             PEK00150
 5000 CONTINUE                                                          PEK00151
C                                                                       PEK00152
      CALL PAGE                                                         PEK00153
      MMNHR=MM*NHR                                                      PEK00154
      WRITE(IPUT,27)NHR,THR,UNITS,MMNHR,(X1,X1,X2,K=1,MIN0(8,NR))       PEK00155
   27 FORMAT(/,7X,'NUMBER OF ',I2,'-HOUR AVERAGES ABOVE ',F10.4,1X,2A4, PEK00156
     X ' FOR ',I5,' HOURS',//,' RECEPTORS:',/,' -------+',8(2A4,A1))    PEK00157
      CALL LINES(5)                                                     PEK00158
      DO 7000 N=1,NR,8                                                  PEK00159
      CALL LINES(2)                                                     PEK00160
      M=MIN0(N+7,NR)                                                    PEK00161
      WRITE(IPUT,28)N,M,(NN(K),K=N,M)                                   PEK00162
   28 FORMAT(1X,I3,'-',I3,'!',8(3X,I3,2X,'!'))                          PEK00163
      WRITE(IPUT,26) (X1,X1,X3,K=N,M)                                   PEK00164
   26 FORMAT(' -------!',8(2A4,A1))                                     PEK00165
 7000 CONTINUE                                                          PEK00166
      CALL PAGE                                                         PEK00167
      RETURN                                                            PEK00168
C                                                                       PEK00169
 8000 WRITE(IPUT,29)                                                    PEK00170
   29 FORMAT(2X,'ILLEGAL DATA LINE.')                                   PEK00171
      GO TO 9000                                                        PEK00172
 8100 WRITE(IPUT,30)                                                    PEK00173
   30 FORMAT(2X,'UNEXPECTED END OF FILE.')                              PEK00174
C                                                                       PEK00175
 9000 STOP                                                              PEK00176
      END                                                               PEK00177

      SUBROUTINE TOPVAL                                                 TOP00001
C                                                                       TOP00002
C********************************************************************   TOP00003
C                                                                       TOP00004
C       THIS SUBROUTINE DISPLAYS THE HIGHEST N CONCENTRATIONS           TOP00005
C       (USER-SPECIFIED N) MODELED AT EACH RECEPTOR.                    TOP00006
C                                                                       TOP00007
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          TOP00008
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          TOP00009
C                                                                       TOP00010
C       ANALYSIS  VERSION 2.3    LEVEL 851125                           TOP00011
C                                                                       TOP00012
C********************************************************************   TOP00013
C                                                                       TOP00014
       DIMENSION VALUE(2501),AVR(400),DAY(2501),HOURS(2501),MET(4),     TOP00015
     X CONC(400),JX(2501),T1(400),T2(400),T1REC(25),T2REC(25),T3(25),   TOP00016
     X T4(25)                                                           TOP00017
      INTEGER ALL,DAY,HOURS,T1REC,T2REC                                 TOP00018
      INTEGER MET                                                       TOP00019
      LOGICAL EOF1                                                      TOP00020
      CHARACTER*4 X1,X2
      IC = 5                                                            TOP00021
      ICH=8                                                             TOP00022
      IPUT=6                                                            TOP00023
      ICNT=0                                                            TOP00024
      X1 = '----'                                                       TOP00025
      X2 = '---+'                                                       TOP00026
C                                                                       TOP00027
      WRITE(IPUT,45)                                                    TOP00028
   45 FORMAT(///,'   TOPVAL',///)                                       TOP00029
C                                                                       TOP00030
      WRITE(IPUT,1)                                                     TOP00031
    1 FORMAT(//,13X,'LP',3X,'NH',3X,'NM',4X,'DAYSIN',4X,                TOP00032
     X 'HOURIN',/)                                                      TOP00033
      READ(IC,2,ERR=8000)LP,NH,NM,DAYSIN,HOURIN                         TOP00034
    2 FORMAT(10X,3I5,2F10.0)                                            TOP00035
      READ(IC,4,ERR=8000)NR,RFACT                                       TOP00036
    4 FORMAT(10X,I10,F10.0)                                             TOP00037
      IF(LP.LE.0) LP=1                                                  TOP00038
      IF(NH.LT.1) NH = 1                                                TOP00039
      WRITE(IPUT,2) LP,NH,NM,DAYSIN,HOURIN                              TOP00040
      WRITE(IPUT,3)                                                     TOP00041
    3 FORMAT(///,18X,'NR',3X,'RFACT',/)                                 TOP00042
      WRITE(IPUT,4) NR,RFACT                                            TOP00043
      IF(NR.GE.1.AND.NR.LE.400) GO TO 1000                              TOP00044
      WRITE(IPUT,5) NR                                                  TOP00045
    5 FORMAT(/,'  NR OUT OF RANGE (1-400): ',I5)                        TOP00046
      GO TO 9000                                                        TOP00047
 1000 IF(NM.GE.1.AND.NM.LE.2500/NR) GO TO 1100                          TOP00048
      WRITE(IPUT,6)NM,NR                                                TOP00049
    6 FORMAT(/,'   NM*NR OUT OF RANGE ( <=2500): NM = ',I5,             TOP00050
     X ', NR = ',I5)                                                    TOP00051
      GO TO 9000                                                        TOP00052
 1100 HOURIN=HOURIN+24.*DAYSIN                                          TOP00053
      IF(NM.EQ.1) NM = 2                                                TOP00054
      ALL=0                                                             TOP00055
      IF(HOURIN.LE.0.) ALL=1.                                           TOP00056
      IF (RFACT.LE.0.) RFACT=1.0E+6                                     TOP00057
      HOUR=0.                                                           TOP00058
      DO 1200 I = 1,2501                                                TOP00059
      VALUE(I) = 0.0                                                    TOP00060
      DAY(I) = 0                                                        TOP00061
 1200 HOURS(I) = 0                                                      TOP00062
 1500 IF(ALL.NE.1.AND.HOUR.GE.HOURIN) GO TO 4000                        TOP00063
      DO 1600 IR=1,NR                                                   TOP00064
      AVR(IR)=0.                                                        TOP00065
 1600 CONTINUE                                                          TOP00066
      DO 2100 J=1,LP                                                    TOP00067
      IF(ICNT.EQ.1) GO TO 1800                                          TOP00068
      CALL READIT(MET,CONC,NR,ICH,EOF1)                                 TOP00069
      IF(EOF1) GO TO 8500                                               TOP00070
      ICNT=1                                                            TOP00071
      GO TO 1900                                                        TOP00072
 1800 CALL READIT(MET,CONC,NR,ICH,EOF1)                                 TOP00073
      IF(EOF1) GO TO 4000                                               TOP00074
 1900 DO 2000 IR=1,NR                                                   TOP00075
      AVR(IR)=AVR(IR)+CONC(IR)                                          TOP00076
 2000 CONTINUE                                                          TOP00077
 2100 CONTINUE                                                          TOP00078
      HOUR=HOUR+FLOAT(LP*NH)                                            TOP00079
      DO 3000 IR=1,NR                                                   TOP00080
      AVR(IR)=AVR(IR)/FLOAT(LP)                                         TOP00081
      K=(IR-1)*NM                                                       TOP00082
      IM=NM                                                             TOP00083
 2200 KIM=K+IM                                                          TOP00084
      KIM1=KIM -1                                                       TOP00085
      IF(AVR(IR).LE.VALUE(KIM)) GO TO 2500                              TOP00086
      IF(IM.EQ.1) GO TO 2450                                            TOP00087
      VALUE(KIM)=VALUE(KIM1)                                            TOP00088
      DAY(KIM)=DAY(KIM1)                                                TOP00089
      HOURS(KIM)=HOURS(KIM1)                                            TOP00090
      IM=IM-1                                                           TOP00091
      GO TO 2200                                                        TOP00092
 2450 IM = 0                                                            TOP00093
 2500 IF(IM.GE.NM) GO TO 3000                                           TOP00094
      KIMP1=K+IM+1                                                      TOP00095
      VALUE(KIMP1)=AVR(IR)                                              TOP00096
      DAY(KIMP1)=(HOUR-1.)/24.+1.                                       TOP00097
      HOURS(KIMP1)=AMOD(HOUR-1.,24.)+1.                                 TOP00098
 3000 CONTINUE                                                          TOP00099
      GO TO 1500                                                        TOP00100
C                                                                       TOP00101
C       ALL DATA READ IN                                                TOP00102
C                                                                       TOP00103
C       GET TOP 25 HIGHEST AND HIGHEST, SECOND-HIGHEST VALUES           TOP00104
C                                                                       TOP00105
 4000 DO 4100 I = 1,NR                                                  TOP00106
      T1(I) = VALUE((I-1)*NM + 1) * RFACT                               TOP00107
      T2(I) = VALUE((I-1)*NM + 2) * RFACT                               TOP00108
 4100 CONTINUE                                                          TOP00109
      DO 4150 I = 1,25                                                  TOP00110
      T1REC(I) = I                                                      TOP00111
      T2REC(I) = I                                                      TOP00112
      T3(I) = 0.0                                                       TOP00113
      T4(I) = 0.0                                                       TOP00114
 4150 CONTINUE                                                          TOP00115
      NLIM = MIN0(25,NR)                                                TOP00116
      DO 4300 J = 1,NLIM                                                TOP00117
      DO 4250 I = 1,NR                                                  TOP00118
      IF(T1(I).LE.T3(J)) GO TO 4200                                     TOP00119
      T3(J) = T1(I)                                                     TOP00120
      T1REC(J) = I                                                      TOP00121
 4200 CONTINUE                                                          TOP00122
      IF(T2(I).LE.T4(J)) GO TO 4250                                     TOP00123
      T4(J) = T2(I)                                                     TOP00124
      T2REC(J) = I                                                      TOP00125
 4250 CONTINUE                                                          TOP00126
      T1(T1REC(J)) = 0.0                                                TOP00127
      T2(T2REC(J)) = 0.0                                                TOP00128
 4300 CONTINUE                                                          TOP00129
      DO 4400 I=1,2500                                                  TOP00130
      VALUE(I)=VALUE(I)*RFACT                                           TOP00131
 4400 CONTINUE                                                          TOP00132
      DO 6000 ISTART=1,NR,16                                            TOP00133
      IEND=MIN0(ISTART+15,NR)                                           TOP00134
      DO 5500 LSTART=1,NM,5                                             TOP00135
      LEND=MIN0(LSTART+4,NM)                                            TOP00136
      CALL PAGE                                                         TOP00137
      DO 4500 IM=LSTART,LEND                                            TOP00138
      JX(IM)=IM                                                         TOP00139
 4500 CONTINUE                                                          TOP00140
      WRITE(IPUT,10)NM,LP,HOUR                                          TOP00141
   10 FORMAT(/,7X,'TOP ',I3,' CONCENTRATIONS OF ',I2,'-HOUR AVERAGES ', TOP00142
     X'FOR ',F6.0,' HOURS')                                             TOP00143
      WRITE(IPUT,11)(JX(IL),IL=LSTART,LEND)                             TOP00144
   11 FORMAT(/,4X,'RECEPTOR   I',5(3X,'TOP ',I3,' I'))                  TOP00145
      WRITE(IPUT,12) (X1,X1,X2,IL = LSTART,LEND+1)                      TOP00146
   12 FORMAT(4X,6(3A4))                                                 TOP00147
      DO 5000 IR=ISTART,IEND                                            TOP00148
      K=(IR-1)*NM                                                       TOP00149
      LST1=LSTART+K                                                     TOP00150
      LND1=LEND+K                                                       TOP00151
      WRITE(IPUT,13)IR,(VALUE(KX),KX=LST1,LND1)                         TOP00152
   13 FORMAT(6X,I3,6X,'I',5(F10.4,' I'))                                TOP00153
      WRITE(IPUT,14)(DAY(KIL),HOURS(KIL),KIL=LST1,LND1)                 TOP00154
   14 FORMAT(4X,'DAY/HOUR',3X,'I',5(2I5,' I'))                          TOP00155
      WRITE(IPUT,12) (X1,X1,X2,IL = LSTART,LEND+1)                      TOP00156
 5000 CONTINUE                                                          TOP00157
 5500 CONTINUE                                                          TOP00158
 6000 CONTINUE                                                          TOP00159
C                                                                       TOP00160
      CALL PAGE                                                         TOP00161
      WRITE(IPUT,15) NLIM                                               TOP00162
   15 FORMAT(/,32X,'TOP ',I2,' HIGHEST AND HIGHEST, SECOND-',           TOP00163
     X 'HIGHEST CONCENTRATIONS',//)                                     TOP00164
      WRITE(IPUT,16)                                                    TOP00165
   16 FORMAT(43X,'HIGHEST',18X,'HIGHEST, SECOND-HIGHEST',//,            TOP00166
     X 27X,'RANK',5X,'RECEPTOR   CONCENTRATION',8X,                     TOP00167
     X 'RECEPTOR   CONCENTRATION',//)                                   TOP00168
      WRITE(IPUT,17) (J,T1REC(J),T3(J),T2REC(J),T4(J),J=1,NLIM)         TOP00169
   17 FORMAT(28X,I2,9X,I3,7X,F10.3,12X,I3,7X,F10.3,/)                   TOP00170
      CALL PAGE                                                         TOP00171
      RETURN                                                            TOP00172
C                                                                       TOP00173
 8000 WRITE(IPUT,21)                                                    TOP00174
   21 FORMAT(/,'   ILLEGAL DATA LINE.')                                 TOP00175
      GO TO 9000                                                        TOP00176
 8500 WRITE(IPUT,22)                                                    TOP00177
   22 FORMAT(/,'   UNEXPECTED END OF CONCENTRATION FILE.')              TOP00178
 9000 STOP                                                              TOP00179
      END                                                               TOP00180

      SUBROUTINE CUMFRQ                                                 CUM00001
C                                                                       CUM00002
C********************************************************************   CUM00003
C                                                                       CUM00004
C       THIS ROUTINE IS USED TO COMPUTE CUMULATIVE FREQUENCY            CUM00005
C       DISTRIBUTIONS AND AVERAGE CONCENTRATIONS FOR UP TO              CUM00006
C       400 RECEPTORS.                                                  CUM00007
C                                                                       CUM00008
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          CUM00009
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          CUM00010
C                                                                       CUM00011
C       ANALYSIS  VERSION 2.3    LEVEL 851125                           CUM00012
C                                                                       CUM00013
C********************************************************************   CUM00014
C                                                                       CUM00015
      DIMENSION LEV(20),FREQ(22)                                        CUM00016
      DIMENSION MET(4),CONC(400),FCAT(22,400),VAL(400),VIOL(400)        CUM00017
      REAL LEV                                                          CUM00018
      INTEGER ALL                                                       CUM00019
      INTEGER MET                                                       CUM00020
      LOGICAL EOF1                                                      CUM00021
      CHARACTER*4 X1,X2                                                 IBM00004
      IC = 5                                                            CUM00022
      ICH=8                                                             CUM00023
      IPUT=6                                                            CUM00024
      ICNT=0                                                            CUM00025
      X1 = '----'                                                       CUM00026
      X2 = '---+'                                                       CUM00027
C                                                                       CUM00028
      WRITE(IPUT,45)                                                    CUM00029
   45 FORMAT(///,'   CUMFREQ',///)                                      CUM00030
C                                                                       CUM00031
      WRITE(IPUT,1)                                                     CUM00032
    1 FORMAT(/,9X,'HOURIN',4X,'DAYSIN',4X,'RFACT',8X,                   CUM00033
     X 'NHR',2X,'NLEV  NR')                                             CUM00034
      READ(IC,2,ERR=8000)HOURIN,DAYSIN,RFACT,NHR,NLEV,NR                CUM00035
    2 FORMAT(30X,3F10.0,I10,2I5)                                        CUM00036
    3 FORMAT(/,5X,3F10.0,I10,2I5)                                       CUM00037
      ALL=1                                                             CUM00038
      IF(HOURIN.GT.0.0.OR.DAYSIN.GT.0.0) ALL=0                          CUM00039
      IF(RFACT.LE.0. ) RFACT=1.0E+06                                    CUM00040
      IF(NHR.LE.0) NHR=1                                                CUM00041
      WRITE(6,3) HOURIN,DAYSIN,RFACT,NHR,NLEV,NR                        CUM00042
      IF(NLEV.GE.0.AND.NLEV.LE.20) GO TO 1000                           CUM00043
      WRITE(IPUT,4)                                                     CUM00044
    4 FORMAT(/,'   NLEV OUT OF RANGE (1-20).')                          CUM00045
      GO TO 9000                                                        CUM00046
 1000 IF(NR.GE.1.AND.NR.LE.400) GO TO 1100                              CUM00047
      WRITE(IPUT,5)                                                     CUM00048
    5 FORMAT(/,'   NR OUT OF RANGE (1-400).')                           CUM00049
      GO TO 9000                                                        CUM00050
 1100 READ(IC,6)(LEV(II),II=1,NLEV)                                     CUM00051
    6 FORMAT(10X,6F10.1,10X)                                            CUM00052
      MLEV=NLEV+2                                                       CUM00053
      WRITE(IPUT,50)                                                    CUM00054
   50 FORMAT(///,'     CUMULATIVE LEVELS:',/)                           CUM00055
      WRITE(IPUT,6) (LEV(II),II=1,NLEV)                                 CUM00056
      LEV(NLEV+1) = 1.0E20                                              CUM00057
      HOUR=0.                                                           CUM00058
      DO 1300 I = 1,400                                                 CUM00059
      DO 1200 J = 1,MLEV                                                CUM00060
 1200 FCAT(J,I) = 0.0                                                   CUM00061
 1300 VIOL(I) = 0.0                                                     CUM00062
      HOURIN=HOURIN+24.*DAYSIN                                          CUM00063
      XHR = FLOAT(NHR)                                                  CUM00064
C                                                                       CUM00065
C     DATA READ LOOP                                                    CUM00066
C                                                                       CUM00067
 1500 IF(ALL.NE.1.AND.HOUR.GE.HOURIN) GO TO 3000                        CUM00068
      DO 1700 I = 1,NR                                                  CUM00069
 1700 VAL(I) = 0.0                                                      CUM00070
      DO 2500 IHR=1,NHR                                                 CUM00071
      IF(ICNT.EQ.1) GO TO 2200                                          CUM00072
      CALL READIT(MET,CONC,NR,ICH,EOF1)                                 CUM00073
      IF(EOF1) GO TO 3000                                               CUM00074
      ICNT=1                                                            CUM00075
      GO TO 2300                                                        CUM00076
 2200 CALL READIT(MET,CONC,NR,ICH,EOF1)                                 CUM00077
      IF(EOF1) GO TO 3000                                               CUM00078
 2300 DO 2400 I=1,NR                                                    CUM00079
      VAL(I) = VAL(I) + RFACT*CONC(I)/XHR                               CUM00080
 2400 CONTINUE                                                          CUM00081
 2500 CONTINUE                                                          CUM00082
      HOUR=HOUR+XHR                                                     CUM00083
      DO 2800 I=1,NR                                                    CUM00084
      DO 2600 J=1,NLEV                                                  CUM00085
      IF(VAL(I).LE.LEV(J)) GOTO 2700                                    CUM00086
 2600 CONTINUE                                                          CUM00087
      J = NLEV + 1                                                      CUM00088
 2700 FCAT(J,I)=FCAT(J,I)+1.                                            CUM00089
      FCAT(MLEV,I)=FCAT(MLEV,I)+VAL(I)                                  CUM00090
 2800 CONTINUE                                                          CUM00091
      GO TO 1500                                                        CUM00092
C                                                                       CUM00093
C     END OF DATA READ LOOP                                             CUM00094
C                                                                       CUM00095
 3000 IF(HOUR.GT.0.0) GO TO 3100                                        CUM00096
      WRITE(IPUT,10)                                                    CUM00097
   10 FORMAT(/,'   INPUT FILE TOO SHORT.')                              CUM00098
      GO TO 9000                                                        CUM00099
 3100 DO 3300 I=1,NR                                                    CUM00100
      DO 3200 J=1,MLEV                                                  CUM00101
      FCAT(J,I)=FCAT(J,I)*XHR/HOUR                                      CUM00102
 3200 CONTINUE                                                          CUM00103
 3300 CONTINUE                                                          CUM00104
      LEV(NLEV+2) = HOUR                                                CUM00105
      DO 6000 ISTART=1,NR,16                                            CUM00106
      IEND=MIN0(ISTART+15,NR)                                           CUM00107
      DO 5900 LSTART=1,MLEV,5                                           CUM00108
      LEND=MIN0(LSTART+4,MLEV)                                          CUM00109
      CALL PAGE                                                         CUM00110
      WRITE(IPUT,11)NHR,HOUR                                            CUM00111
   11 FORMAT(/,6X,'CUMULATIVE FREQUENCIES OF ',I5,'-HOUR AVERAGES FOR ',CUM00112
     X  F5.0,' OBSERVATIONS')                                           CUM00113
      IF(LEND.NE.MLEV) GO TO 5700                                       CUM00114
      KT=(LEND-LSTART+1)                                                CUM00115
      GO TO (4100,4200,4300,4400,4500),KT                               CUM00116
 4100 WRITE(IPUT,13)                                                    CUM00117
   13 FORMAT('0   ','RECEPTOR   IAVG:# OBS= I')                         CUM00118
      GO TO 4600                                                        CUM00119
 4200 WRITE (IPUT,14)                                                   CUM00120
   14 FORMAT('0   ','RECEPTOR   I',1('   LEVEL   I'),'AVG:# OBS= I')    CUM00121
      GO TO 4600                                                        CUM00122
 4300 WRITE(IPUT,15)                                                    CUM00123
   15 FORMAT('0   ','RECEPTOR   I',2('   LEVEL   I'),'AVG:# OBS= I')    CUM00124
      GO TO 4600                                                        CUM00125
 4400 WRITE(IPUT,16)                                                    CUM00126
   16 FORMAT('0   ','RECEPTOR   I',3('   LEVEL   I'),'AVG:# OBS= I')    CUM00127
      GO TO 4600                                                        CUM00128
 4500 WRITE(IPUT,17)                                                    CUM00129
   17 FORMAT('0   ','RECEPTOR   I',4('   LEVEL   I'),'AVG:# OBS= I')    CUM00130
 4600 WRITE(IPUT,18)(LEV(IL),IL=LSTART,LEND)                            CUM00131
   18  FORMAT(15X,'I',5(1X,F10.1,'I'))                                  CUM00132
      WRITE(IPUT,20) (X1,X1,X2,K=1,KT)                                  CUM00133
   20 FORMAT(15X,'+',5(3A4))                                            CUM00134
      GO TO 5750                                                        CUM00135
 5700 WRITE(IPUT,25)                                                    CUM00136
   25 FORMAT(/,4X,'RECEPTOR   I',5('   LEVEL   I'))                     CUM00137
      WRITE(IPUT,26)(LEV(IL),IL=LSTART,LEND)                            CUM00138
   26 FORMAT(15X,'I',5(1X,F10.1,'I'))                                   CUM00139
      WRITE(IPUT,24) (X1,X1,X2,K=LSTART,LEND)                           CUM00140
   24 FORMAT(15X,'+',5(3A4))                                            CUM00141
 5750 CONTINUE                                                          CUM00142
      DO 5850 IR=ISTART,IEND                                            CUM00143
      DO 5800 J=LSTART,LEND                                             CUM00144
      VIOL(IR)=VIOL(IR)+FCAT(J,IR)                                      CUM00145
      FREQ(J)=VIOL(IR)                                                  CUM00146
 5800 CONTINUE                                                          CUM00147
      IF(LEND.NE.MLEV) GO TO 5820                                       CUM00148
      FREQ(MLEV)=FCAT(MLEV,IR)                                          CUM00149
      FCAT(MLEV,IR) = 0.0                                               CUM00150
 5820 WRITE(IPUT,30)IR, (FCAT(IL,IR),IL=LSTART,LEND)                    CUM00151
   30 FORMAT(6X,I4,5X,'I',5(F10.4,' I'))                                CUM00152
      WRITE(IPUT,31) (FREQ(IL),IL=LSTART,LEND)                          CUM00153
   31 FORMAT(4X,'CUM FREQ',3X,'I',5(F10.4,' I'))                        CUM00154
      WRITE(IPUT,24) (X1,X1,X2,K=LSTART,LEND)                           CUM00155
 5850 CONTINUE                                                          CUM00156
 5900 CONTINUE                                                          CUM00157
 6000 CONTINUE                                                          CUM00158
      CALL PAGE                                                         CUM00159
      RETURN                                                            CUM00160
C                                                                       CUM00161
 8000 WRITE(IPUT,40)                                                    CUM00162
   40 FORMAT(/,'   FIRST DATA LINE FORMAT ERROR.')                      CUM00163
 9000 WRITE(IPUT,41)                                                    CUM00164
   41 FORMAT(/,'   EXECUTION TERMINATED DUE TO FATAL ERROR IN CUMFREQ.')CUM00165
      STOP                                                              CUM00166
      END                                                               CUM00167

      SUBROUTINE AVERGE                                                 AVG00001
C                                                                       AVG00002
C********************************************************************   AVG00003
C                                                                       AVG00004
C       THIS ROUTINE CREATES A NEW FILE OF RUNNING (OVERLAPPING)        AVG00005
C       AVERAGES FOR A USER-SPECIFIED LENGTH OF PERIOD                  AVG00006
C                                                                       AVG00007
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          AVG00008
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          AVG00009
C                                                                       AVG00010
C       ANALYSIS  VERSION 2.3    LEVEL 851125                           AVG00011
C                                                                       AVG00012
C********************************************************************   AVG00013
C                                                                       AVG00014
      DIMENSION SIGMA(400),CONC(400),MET(4),TEMP(24,400),               AVG00015
     *AVG(400)                                                          AVG00016
      LOGICAL EOF1                                                      AVG00017
      INTEGER MET                                                       AVG00018
      IC = 5                                                            AVG00019
      IPR = 6                                                           AVG00020
      ISPT=1                                                            AVG00021
      ICH = 8                                                           AVG00022
      IOUT = 9                                                          AVG00023
C                                                                       AVG00024
      WRITE(IPR,45)                                                     AVG00025
   45 FORMAT(//,'   AVERAGE',///)                                       AVG00026
C                                                                       AVG00027
      DO 500 I = 1,400                                                  AVG00028
  500 SIGMA(I) = 0.0                                                    AVG00029
      READ(IC,1,ERR=8100) N,NRECP                                       AVG00030
    1 FORMAT(2I5)                                                       AVG00031
      WRITE (IPR,2) N,NRECP                                             AVG00032
    2 FORMAT(10X,'# OF HOURS = ',I5,', # OF RECEPTORS = ',I5)           AVG00033
      IF(N.GT.1) GO TO 1000                                             AVG00034
      WRITE(IPR,3)                                                      AVG00035
    3 FORMAT(/,'   NUMBER OF HOURS TOO SMALL.')                         AVG00036
 1000 NR=N-1                                                            AVG00037
      DO 1500 IREC=1,NR                                                 AVG00038
      CALL READIT(MET,CONC,NRECP,ICH,EOF1)                              AVG00039
      IF(EOF1) GO TO 8500                                               AVG00040
      ISPT=ISPT+1                                                       AVG00041
      DO 1200 J=1,NRECP                                                 AVG00042
      SIGMA(J)=SIGMA(J)+CONC(J)                                         AVG00043
      TEMP(ISPT,J)=CONC(J)                                              AVG00044
      IF (NR.EQ.1)  TEMP(N,J)=0                                         AVG00045
 1200 CONTINUE                                                          AVG00046
 1500 CONTINUE                                                          AVG00047
      IREC=0                                                            AVG00048
 1700 CALL READIT(MET,CONC,NRECP,ICH,EOF1)                              AVG00049
      IF (EOF1) GO TO 5000                                              AVG00050
      ISPT=ISPT+1                                                       AVG00051
      IF (ISPT.GT.N) ISPT=1                                             AVG00052
      DO 2000 J=1,NRECP                                                 AVG00053
      SIGMA(J)=SIGMA(J)+CONC(J)-TEMP(ISPT,J)                            AVG00054
      AVG(J)=SIGMA(J)/FLOAT(N)                                          AVG00055
      TEMP(ISPT,J)=CONC(J)                                              AVG00056
 2000 CONTINUE                                                          AVG00057
      CALL WRITIT(MET,AVG,NRECP,IOUT)                                   AVG00058
      IREC=IREC+1                                                       AVG00059
      GO TO 1700                                                        AVG00060
 5000 WRITE(IPR,10)IREC                                                 AVG00061
   10 FORMAT(/,10X,I5,' RECORDS UPDATED')                               AVG00062
      CALL PAGE                                                         AVG00063
      RETURN                                                            AVG00064
C                                                                       AVG00065
 8100 WRITE(IPR,11)                                                     AVG00066
   11 FORMAT(/,'   ILLEGAL CARD FORMAT FOR DATA.')                      AVG00067
      GO TO 9000                                                        AVG00068
 8500 WRITE(IPR,12)                                                     AVG00069
   12 FORMAT(/,'   UNEXPECTED END OF FILE.')                            AVG00070
 9000 STOP                                                              AVG00071
      END                                                               AVG00072

      SUBROUTINE SEQADD                                                 SEQ00001
C                                                                       SEQ00002
C********************************************************************   SEQ00003
C                                                                       SEQ00004
C      THIS ROUTINE CREATES A NEW FILE OF CONCENTRATIONS ADDED (AND     SEQ00005
C      POSSIBLY SCALED) FROM A TOTAL OF 1 TO 5 INPUT CONCENTRATION FILESSEQ00006
C                                                                       SEQ00007
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          SEQ00008
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          SEQ00009
C                                                                       SEQ00010
C       ANALYSIS  VERSION 2.3    LEVEL 851125                           SEQ00011
C                                                                       SEQ00012
C********************************************************************   SEQ00013
C                                                                       SEQ00014
      DIMENSION CONC(400),MET(4),TEMP(400),SCALE(5)                     SEQ00015
      LOGICAL EOF1                                                      SEQ00016
      INTEGER MET                                                       SEQ00017
      IC = 5                                                            SEQ00018
      IPR = 6                                                           SEQ00019
      IOUT = 10                                                         SEQ00020
C                                                                       SEQ00021
      READ(IC,*,ERR=8100) N,NRECP                                       SEQ00022
      IF(N.LT.1.OR.N.GT.5) GO TO 8300                                   SEQ00023
      IF(NRECP.LT.1.OR.NRECP.GT.400) GO TO 8400                         SEQ00024
      READ(IC,*,ERR=8200) (SCALE(I),I=1,N)                              SEQ00025
      WRITE(IPR,25)                                                     SEQ00026
   25 FORMAT(//,'   SEQADD',///)                                        SEQ00027
      WRITE(IPR,30) (I,SCALE(I),I=1,N)                                  SEQ00028
   30 FORMAT(//,(5X,'FILE # ',I1,':  SCALE FACTOR = ',F12.6))           SEQ00029
C                                                                       SEQ00030
      IREC = 0                                                          SEQ00031
  300 DO 500 I = 1,400                                                  SEQ00032
  500 CONC(I) = 0.0                                                     SEQ00033
      DO 1000 IFILE=1,N                                                 SEQ00034
      ICH = 10 + IFILE                                                  SEQ00035
      CALL READIT(MET,TEMP,NRECP,ICH,EOF1)                              SEQ00036
      IF(EOF1) GO TO 5000                                               SEQ00037
      DO 800 J=1,NRECP                                                  SEQ00038
  800 CONC(J) = CONC(J) + TEMP(J)*SCALE(IFILE)                          SEQ00039
 1000 CONTINUE                                                          SEQ00040
      CALL WRITIT(MET,CONC,NRECP,IOUT)                                  SEQ00041
      IREC = IREC + 1                                                   SEQ00042
      GO TO 300                                                         SEQ00043
 5000 WRITE(IPR,10)IREC                                                 SEQ00044
   10 FORMAT(//,10X,I5,' HOURS SUMMED')                                 SEQ00045
      CALL PAGE                                                         SEQ00046
      RETURN                                                            SEQ00047
C                                                                       SEQ00048
 8100 WRITE(IPR,11)                                                     SEQ00049
   11 FORMAT(/,'   FORMAT ERROR IN READING NUMBER OF FILES, RECEPTORS.')SEQ00050
      GO TO 9000                                                        SEQ00051
 8200 WRITE(IPR,12)                                                     SEQ00052
   12 FORMAT(/,'   FORMAT ERROR IN READING SCALE FACTORS.')             SEQ00053
      GO TO 9000                                                        SEQ00054
 8300 WRITE(IPR,13)                                                     SEQ00055
   13 FORMAT(/,'   # OF INPUT FILES NOT BETWEEN 1 AND 5.')              SEQ00056
      GO TO 9000                                                        SEQ00057
 8400 WRITE(IPR,14)                                                     SEQ00058
   14 FORMAT(/,'   # OF RECEPTORS NOT BETWEEN 1 AND 400.')              SEQ00059
 9000 WRITE(IPR,20)                                                     SEQ00060
   20 FORMAT(1X,'   *** FATAL ERROR ***')                               SEQ00061
      STOP                                                              SEQ00062
      END                                                               SEQ00063

        SUBROUTINE WRITIT(MET,CONC,NREC,ICH)                            WRI00001
C                                                                       WRI00002
C********************************************************************   WRI00003
C                                                                       WRI00004
C       THIS ROUTINE WRITES ONE CONCENTRATION FILE RECORD, WHICH HAS UP WRI00005
C       TO 400 RECEPTOR CONCENTRATIONS.                                 WRI00006
C                                                                       WRI00007
C       MET     ARRAY CONTAINING METEOROLOGY: WD,WS,MIX,IST             WRI00008
C       CONC    ARRAY OF SIZE NREC TO RECEIVE CONCENTRATION VALUES      WRI00009
C       NREC    # OF CONCENTRATIONS TO BE WRIT FOR EACH RECORD          WRI00010
C       ICH     INPUT CHANNEL FOR WRITING DATA                          WRI00011
C                                                                       WRI00012
C                                                                       WRI00013
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          WRI00014
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          WRI00015
C                                                                       WRI00016
C                                                                       WRI00017
C       ANALYSIS  VERSION 2.3    LEVEL 851125                           WRI00018
C                                                                       WRI00019
C********************************************************************   WRI00020
C                                                                       WRI00021
        DIMENSION CONC(NREC),MET(4)                                     WRI00022
        INTEGER MET                                                     WRI00023
C                                                                       WRI00024
        WRITE(ICH) MET,CONC                                             WRI00025
        RETURN                                                          WRI00026
        END                                                             WRI00027

        SUBROUTINE READIT(MET,CONC,NREC,ICH,EOF1)                       RED00001
C                                                                       RED00002
C********************************************************************   RED00003
C                                                                       RED00004
C       THIS ROUTINE READS ONE CONCENTRATION FILE RECORD, WHICH HAS UP  RED00005
C       TO 400 RECEPTOR CONCENTRATIONS.                                 RED00006
C                                                                       RED00007
C       MET     ARRAY CONTAINING METEOROLOGY: MIX,WD,IST,WS             RED00008
C       CONC    ARRAY OF SIZE NREC TO RECEIVE CONCENTRATION VALUES      RED00009
C       NREC    # OF CONCENTRATIONS TO BE READ FOR EACH RECORD          RED00010
C       ICH     INPUT CHANNEL FOR READING DATA                          RED00011
C       EOF1    END OF FILE LOGICAL VARIABLE                            RED00012
C                                                                       RED00013
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          RED00014
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          RED00015
C                                                                       RED00016
C                                                                       RED00017
C       ANALYSIS  VERSION 2.3    LEVEL 851125                           RED00018
C                                                                       RED00019
C********************************************************************   RED00020
C                                                                       RED00021
        LOGICAL EOF1                                                    RED00022
        DIMENSION CONC(NREC),MET(4)                                     RED00023
        INTEGER MET                                                     RED00024
C                                                                       RED00025
        EOF1 = .FALSE.                                                  RED00026
        READ(ICH,END=1000) MET,CONC                                     RED00027
        RETURN                                                          RED00028
 1000   EOF1 = .TRUE.                                                   RED00029
        RETURN                                                          RED00030
        END                                                             RED00031

        SUBROUTINE LINES(N)                                             LIN00001
C                                                                       LIN00002
C***********************************************************************LIN00003
C                                                                       LIN00004
C       N       NUMBER OF NEW LINES                                     LIN00005
C                                                                       LIN00006
C      ENVIRONMENTAL RESEARCH AND TECHNOLOGY, INC.                      LIN00007
C      696 VIRGINIA ROAD, CONCORD, MASS. 01742                          LIN00008
C                                                                       LIN00009
C      ANALYSIS  VERSION 2.3     LEVEL 851125                           LIN00010
C                                                                       LIN00011
C***********************************************************************LIN00012
C                                                                       LIN00013
        COMMON /HEAD/ NPAGE,NLINE                                       LIN00014
        NLINE = NLINE + N                                               LIN00015
        IF(NLINE.LE.63) RETURN                                          LIN00016
        CALL PAGE                                                       LIN00017
        NLINE = NLINE + N                                               LIN00018
        RETURN                                                          LIN00019
        END                                                             LIN00020

       SUBROUTINE PAGE                                                  PAG00001
C********************************************************************   PAG00002
C                                                                       PAG00003
C       THIS ROUTINE SKIPS TO A NEW PAGE AND WRITES A PAGE HEADER       PAG00004
C                                                                       PAG00005
C       ENVIRONMENTAL  RESEARCH AND TECHNOLOGY                          PAG00006
C       696 VIRGINIA ROAD, CONCORD, MASS 01742                          PAG00007
C                                                                       PAG00008
C       ANALYSIS  VERSION 2.30    LEVEL 851125                          PAG00009
C                                                                       PAG00010
C********************************************************************   PAG00011
C                                                                       PAG00012
       COMMON /HEAD/ NPAGE,NLINE                                        PAG00013
       NPAGE=NPAGE+1                                                    PAG00014
       NLINE=3                                                          PAG00015
       WRITE(6,1000) NPAGE                                              PAG00016
 1000  FORMAT('1','      POST-PROCESSING ANALYSIS PROGRAM     VERSION ',PAG00017
     X '2.3      LEVEL 851125',22X,'PAGE',1X,I3,/)                      PAG00018
       RETURN                                                           PAG00019
       END                                                              PAG00020
