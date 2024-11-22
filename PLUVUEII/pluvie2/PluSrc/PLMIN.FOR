CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPLN00010
      SUBROUTINE PLMIN(RPR,ROR,SY,RP,YINTR)                             PLN00020
C                                                                       PLN00030
C  PLMIN.FOR                                                            PLN00040
C                                                                       PLN00050
C  RECALCULATES PLUME CENTROID WHEN OBSERVER OR BACKGROUND              PLN00060
C  OBJECT IS WITHIN THE PLUME.                                          PLN00070
C                                                                       PLN00080
C  PLUVUE II                                                            PLN00090
C  Version 92243   Level 1.0                                            PLN00100
C                                                                       PLN00110
C  U.S. Environmental Protection Agency                                 PLN00120
C  Office of Air Quality Planning and Standards                         PLN00130
C  Research Triangle Park, North Carolina                               PLN00140
C                                                                       PLN00150
C  Modifications:                                                       PLN00160
C                                                                       PLN00170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPLN00180
      SAVE                                                              PLN00190
C                                                                       PLN00200
      YINTRF=0.5                                                        PLN00210
      YINTRB=0.5                                                        PLN00220
      YINTR=YINTRF+YINTRB                                               PLN00230
      IF(RPR.LT.2.17.AND.ROR.GT.2.17)GO TO 10                           PLN00240
      IF(RPR.GT.2.17.AND.ROR.LT.2.17)GO TO 20                           PLN00250
      IF(RPR.LT.2.17.AND.ROR.LT.2.17)GO TO 30                           PLN00260
C                                                                       PLN00270
C   OBSERVER IN PLUME                                                   PLN00280
C                                                                       PLN00290
10    RPHALF=0.18*(RPR+0.25)+0.153*(RPR+0.75)+0.167*(RPR+1.5)           PLN00300
      IF(RPR.GT.0.5)GO TO 11                                            PLN00310
C                                                                       PLN00320
C   OBSERVER > 1/2 SIGMA-Y FROM CENTERLINE.                             PLN00330
C                                                                       PLN00340
      YINTR1=(RPR/0.5)*0.18                                             PLN00350
      YINTRF=YINTR1                                                     PLN00360
      YINTR=YINTRF+YINTRB                                               PLN00370
      RP=((RPR*YINTR1/2.+RPHALF)/YINTR)*(SY/1000.)                      PLN00380
      GO TO 100                                                         PLN00390
11    IF(RPR.GT.1.0)GO TO 12                                            PLN00400
C                                                                       PLN00410
C   OBSERVER BETWEEN 1/2 AND 1 SIGMA-Y FROM CENTERLINE OF PLUME.        PLN00420
C                                                                       PLN00430
      YINTR1=((RPR-0.5)/0.5)*0.153                                      PLN00440
      YINTRF=YINTR1+0.18                                                PLN00450
      YINTR=YINTRF+YINTRB                                               PLN00460
      RP=(((RPR-0.5)/2.*YINTR1+RPHALF)/YINTR)*SY/1000.                  PLN00470
      GO TO 100                                                         PLN00480
12    YINTR1=(RPR-1.0)*0.142                                            PLN00490
C                                                                       PLN00500
C   OBSERVER BETWEEN 1 AND 2.17 SIGMA-Y FROM CENTERLINE OF PLUME.       PLN00510
C                                                                       PLN00520
      YINTRF=YINTR1+0.18+0.153                                          PLN00530
      YINTR=YINTRF+YINTRB                                               PLN00540
      RP=(((RPR-1.)*YINTR1/2.+(RPR-0.75)*0.153+(RPR-0.25)*0.18+RPHALF   PLN00550
     1)/YINTR)*SY/1000.                                                 PLN00560
      GO TO 100                                                         PLN00570
20    CONTINUE                                                          PLN00580
C                                                                       PLN00590
C   OBSERVER OUTSIDE PLUME                                              PLN00600
C   ADJUST RPHALF FOR OBJECT WITHIN PLUME                               PLN00610
C   RPFRNT FOR FRONT SIDE                                               PLN00620
C                                                                       PLN00630
      RPFRNT=(RPR-0.25)*0.18+(RPR-0.75)*0.153+(RPR-1.5)*0.167           PLN00640
      IF(ROR.GT.0.5)GO TO 21                                            PLN00650
      YINTR1=(ROR/0.5)*0.18                                             PLN00660
      YINTRB=YINTR1                                                     PLN00670
      YINTR=YINTRB+YINTRF                                               PLN00680
      RP=((RPFRNT+(RPR+ROR/2.)*YINTR1)/YINTR)*SY/1000.                  PLN00690
      GO TO 100                                                         PLN00700
21    IF (ROR.GT.1.0)GO TO 22                                           PLN00710
C                                                                       PLN00720
C   OBJECT BETWEEN 1/2 AND 1 SIGMA-Y FROM PLUME CENTERLINE.             PLN00730
C                                                                       PLN00740
      YINTR1=((ROR-0.5)/0.5)*0.153                                      PLN00750
      YINTRB=YINTR1+0.18                                                PLN00760
      YINTR=YINTRF+YINTRB                                               PLN00770
      RP=((RPFRNT+(RPR+(ROR-0.5)/2.)*YINTR1+(RPR+0.25)*0.18)/YINTR)*    PLN00780
     1  SY/1000.                                                        PLN00790
      GO TO 100                                                         PLN00800
22    CONTINUE                                                          PLN00810
C                                                                       PLN00820
C   OBJECT BETWEEN 1 AND 2.17 SIGMA-Y FROM PLUME CENTERLINE.            PLN00830
C                                                                       PLN00840
      YINTR1=(ROR-1.0)*0.142                                            PLN00850
      YINTRB=YINTR1+0.18+0.153                                          PLN00860
      YINTR=YINTRF+YINTRB                                               PLN00870
      RP=((RPFRNT+(RPR+(ROR-1.0)/2.)*YINTR1+(RPR+0.75)*0.153+(RPR+0.25) PLN00880
     1 *0.18)/YINTR)*SY/1000.                                           PLN00890
      GO TO 100                                                         PLN00900
30    CONTINUE                                                          PLN00910
C                                                                       PLN00920
C   BOTH OBSERVER AND OBJECT IN PLUME.                                  PLN00930
C                                                                       PLN00940
      IF(RPR.GT.0.5)GO TO 35                                            PLN00950
      IF(ROR.GT.0.5)GO TO 31                                            PLN00960
C                                                                       PLN00970
C  BOTH OBSERVER AND OBJECT ARE WITHIN 0.5 SIGMA-Y OF THE PLUME CENTERLIPLN00980
C                                                                       PLN00990
      YINTR1=(RPR/0.5)*0.18                                             PLN01000
      YINTR2=(ROR/0.5)*0.18                                             PLN01010
      YINTR=YINTR1+YINTR2                                               PLN01020
      RP=((RPR*YINTR1/2.+(RPR+(ROR/2.))*YINTR2)/YINTR)*SY/1000.         PLN01030
      GO TO 100                                                         PLN01040
31    IF(ROR.GT.1.0)GO TO 32                                            PLN01050
C                                                                       PLN01060
C   OBJECT BETWEEN 0.5 AND 1.0 SIGMA-Y FROM CENTERLINE AND              PLN01070
C   OBSERVER < 0.5 SIGMA-Y FROM CENTERLINE OF PLUME.                    PLN01080
C                                                                       PLN01090
      YINTR1=(RPR/0.5)*0.18                                             PLN01100
      YINTR2=((ROR-0.5)/0.5)*0.153                                      PLN01110
      YINTRB=YINTR2+0.18                                                PLN01120
      YINTR=YINTR1+YINTRB                                               PLN01130
      RP=((RPR*YINTR1/2.+(RPR+0.5+(ROR-0.5)/2.)*YINTR2+(RPR+0.25)*0.18) PLN01140
     1 /YINTR)*SY/1000.                                                 PLN01150
      GO TO 100                                                         PLN01160
32    YINTR1=(RPR/0.5)*0.18                                             PLN01170
C                                                                       PLN01180
C   OBJECT BETWEEN 1.0 AND 2.17 SIGMA-Y FROM CENTERLINE AND OBSERVER    PLN01190
C   < 0.5 SIGMA-Y FROM CENTERLINE.                                      PLN01200
C                                                                       PLN01210
      YINTR2=(ROR-0.5)*0.142                                            PLN01220
      YINTRB=YINTR2+0.153+0.18                                          PLN01230
      YINTR=YINTR1+YINTRB                                               PLN01240
      RP=((RPR*YINTR1/2.+(RPR+1.0+(ROR-1.)/2.)*YINTR2+(RPR+0.75)*0.153  PLN01250
     1 +(RPR+0.25)*0.18)/YINTR)*SY/1000.                                PLN01260
      GO TO 100                                                         PLN01270
35    IF(RPR.GT.1.0)GO TO 40                                            PLN01280
      IF(ROR.GT.0.5)GO TO 36                                            PLN01290
C                                                                       PLN01300
C   OBJECT < 0.5 SIGMA-Y FROM CENTERLINE AND OBSERVER BETWEEN 0.5 AND   PLN01310
C   1.0 SIGMA-Y FROM CENTERLINE OF PLUME.                               PLN01320
C                                                                       PLN01330
      YINTR1=((RPR-0.5)/0.5)*0.153                                      PLN01340
      YINTRF=YINTR1+0.18                                                PLN01350
      YINTR2=(ROR/0.5)*0.18                                             PLN01360
      YINTR=YINTRF+YINTR2                                               PLN01370
      RP=(((RPR-0.5)*YINTR1/2.+(RPR-0.25)*0.18+(RPR+ROR/2.)*YINTR2)     PLN01380
     1/YINTR)*SY/1000.                                                  PLN01390
      GO TO 100                                                         PLN01400
36    IF(ROR.GT.1.0)GO TO 37                                            PLN01410
C                                                                       PLN01420
C   BOTH OBSERVER AND OBJECT BETWEEN 0.5 AND 1.0 SIGMA-Y FROM           PLN01430
C   PLUME CENTERLINE.                                                   PLN01440
C                                                                       PLN01450
      YINTR1=((RPR-0.5)/0.5)*0.153                                      PLN01460
      YINTRF=YINTR1+0.18                                                PLN01470
      YINTR2=((ROR-0.5)/0.5)*0.153                                      PLN01480
      YINTRB=YINTR2+0.18                                                PLN01490
      YINTR=YINTRF+YINTRB                                               PLN01500
      RP=(((RPR-0.5)*YINTR1/2.+(RPR-0.25)*0.18+(RPR+0.5+(ROR-0.5)/2.)   PLN01510
     1 *YINTR2+(RPR+0.25)*0.18)/YINTR)*SY/1000.                         PLN01520
      GO TO 100                                                         PLN01530
37    YINTR1=((RPR-0.5)/0.5)*0.153                                      PLN01540
C                                                                       PLN01550
C   OBSERVER BETWEEN 0.5 AND 1.0 SIGMA-Y FROM PLUME CENTERLINE AND OBJECPLN01560
C   BETWEEN 1.0 AND 2.17 SIGMA-Y FROM PLUME CENTERLINE.                 PLN01570
      YINTRF=YINTR1+0.18                                                PLN01580
      YINTR2=(ROR-1.0)*0.142                                            PLN01590
      YINTRB=YINTR2+0.153+0.18                                          PLN01600
      YINTR=YINTRF+YINTRB                                               PLN01610
      RP=(((RPR-0.5)*YINTR1/2.+(RPR-0.25)*0.18+(RPR+1.0+(ROR-1.)/2.)    PLN01620
     1 *YINTR2+(RPR+0.75)*0.153+(RPR+0.25)*0.18)/YINTR)*SY/1000.        PLN01630
      GO TO 100                                                         PLN01640
40    IF(ROR.GT.0.5)GO TO 41                                            PLN01650
C                                                                       PLN01660
C   OBSERVER BETWEEN 1.0 AND 2.17 SIGMA-Y FROM PLUME CENTERLINE AND     PLN01670
C   OBJECT < 0.5 SY FROM PLUME CENTERLINE.                              PLN01680
C                                                                       PLN01690
      YINTR1=(RPR-1.0)*0.142                                            PLN01700
      YINTRF=YINTR1+0.18+0.153                                          PLN01710
      YINTR2=(ROR/0.5)*0.18                                             PLN01720
      YINTR=YINTRF+YINTR2                                               PLN01730
      RP=(((RPR-1.)*YINTR1/2.+(RPR-0.75)*0.153+(RPR-0.25)*0.18+(RPR+ROR/PLN01740
     1 2.)*YINTR2)/YINTR)*SY/1000.                                      PLN01750
      GO TO 100                                                         PLN01760
41    IF(ROR.GT.1.0)GO TO 42                                            PLN01770
C                                                                       PLN01780
C   OBSERVER BETWEEN 1.0 AND 2.17 SIGMA-Y FROM PLUME CENTERLINE AND OBJEPLN01790
C   BETWEEN 1.0 AND 1.5 SIGMA-Y FROM PLUME CENTERLINE.                  PLN01800
C                                                                       PLN01810
      YINTRF=YINTR1+0.18+0.153                                          PLN01820
      YINTR2=((ROR-0.5)/0.5)*0.153                                      PLN01830
      YINTRB=YINTR2+0.18                                                PLN01840
      YINTR=YINTRF+YINTRB                                               PLN01850
      RP=(((RPR-1.0)*YINTR1/2.+(RPR-0.75)*0.153+(RPR-0.25)*0.18+(RPR+0.5PLN01860
     1 +(ROR-0.5)/2.)*YINTR2+(RPR+0.25)*0.18)/YINTR)*SY/1000.           PLN01870
      GO TO 100                                                         PLN01880
42    YINTR1=(RPR-1.0)*0.142                                            PLN01890
C                                                                       PLN01900
C   BOTH OBSERVER AND OBJECT BETWEEN 1.0 AND 2.17 SIGMA-Y FROM PLUME    PLN01910
C   CENTERLINE.                                                         PLN01920
C                                                                       PLN01930
      YINTRF=YINTR1+0.18+0.153                                          PLN01940
      YINTR2=(ROR-1.0)*0.142                                            PLN01950
      YINTRB=YINTR2+0.153+0.18                                          PLN01960
      YINTR=YINTRF+YINTRB                                               PLN01970
      RP=(((RPR-1.0)*YINTR1/2.+(RPR-0.75)*0.153+(RPR-0.25)*0.18+(RPR+1.0PLN01980
     1 +(ROR-1.)/2.)*YINTR2+(RPR+0.75)*0.153+(RPR+0.25)*0.18)/YINTR)*SY/PLN01990
     21000.                                                             PLN02000
100   RETURN                                                            PLN02010
      END                                                               PLN02020
