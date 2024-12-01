10 CLS
20 PRINT "THIS PROGRAM ALLOWS YOU TO ADD, CHANGE, DELETE, OR PRINT DATA IN <CH.DAT>"
30 DIM X(17)
40 NUM=0:NP=0
50 S$="CH.DAT"
60 WW=16
70 PRINT
80 PRINT "1--display or print data"
90 PRINT "2--edit exposure limits for specific chemical"
100 PRINT "3--edit data for specific chemical"
110 PRINT "4--add chemical"
120 PRINT "5--delete chemical"
130 PRINT "6--quit"
140 PRINT "---------------------------"
150 INPUT "Choose one of the above ",CH
160 PRINT
170 ON CH GOTO 210,1060,1060,1920,2900,3350
180 GOTO 150
190 '-----------------------------------------------
200 ' PRINTOUT OF DATA IN FILE
210 INPUT "DO YOU WANT CHEMICAL NUMBER AND NAME ONLY (Y/N)";YY$
220 IF YY$="Y" OR YY$="y" THEN FLAG=1: GOTO 260
230 IF YY$="N" OR YY$="n" THEN FLAG=2: GOTO 260
240 GOSUB 3360
250 GOTO 210
260 INPUT "DO YOU WANT SCREEN OR PRINTER LISTING (S/P)";YN$
270 IF YN$="P" OR YN$="p" THEN IFLAG=1: GOTO 310
280 IF YN$="S" OR YN$="s" THEN IFLAG=2: GOTO 310
290 GOSUB 3360
300 GOTO 260
310 IF FLAG=1 THEN 340
320 INPUT "ENTER CHEMICAL NO. OR PRESS <ENTER> FOR COMPLETE LISTING ",CC$
330 CCC=VAL(CC$)
340 OPEN "i",#1,S$
350 IF EOF(1) THEN 440
360 INPUT #1,N,CHM$
370 FOR J=1 TO WW
380 INPUT #1,X(J)
390 NEXT J
400 IF FLAG=2 THEN 470
410 PRINT N,CHM$
420 IF IFLAG=1 THEN 790
430 IF INT(N/23)<>N/23 THEN 460
440 INPUT "Press <ENTER> to continue",CR$
450 IF EOF(1) THEN 3310
460 GOTO 350
470 IF CC$="" THEN 500
480 IF CCC=N THEN 500
490 GOTO 360
500 PRINT
510 PRINT N,CHM$
520 PRINT
530 PRINT "TWA(ppm) ";X(1)
540 PRINT "TWA(mg/m3) ";X(2)
550 PRINT "STEL(ppm) ";X(3)
560 PRINT "STEL(mg/m3) ";X(4)
570 PRINT "MOLECULAR WEIGHT ";X(5)
580 PRINT "BOILING TEMP(K) ";X(6)
590 PRINT "CRITICAL TEMP(K) ";X(7)
600 PRINT "CRITICAL PRES(atm) ";X(8)
610 PRINT "CRITICAL VOL(cm3/g-mole) ";X(9)
620 PRINT "VAPOR PRES CONSTANT, A";X(10)
630 PRINT "VAPOR PRES CONSTANT, B";X(11)
640 PRINT "VAPOR PRES CONSTANT, C";X(12)
650 PRINT "LIQUID DENSITY CONSTANT ";X(13)
660 PRINT "LIQUID DENSITY CONSTANT ";X(14)
670 PRINT "EFFECTIVE DIAM OF MOLECULE(A) ";X(15)
680 PRINT "ENERGY OF MOLECULAR INTERACTION(J) ";X(16)
685 IF XFLAG=1 THEN XFLAG=0: GOTO 2690
690 PRINT: PRINT: PRINT: PRINT: PRINT
700 IF IFLAG=1 THEN 790
710 INPUT "Press <ENTER> to continue, or <Q> to quit ",CR$
720 IF CR$<> "" THEN 3310
730 PRINT 
740 IF CC$<>"" THEN CLOSE #1: GOTO 320
750 GOTO 350
760 'IF CC$="" THEN 658
770 'IF CCC=N THEN 658
780 'GOTO 330
790 LPRINT N;CHM$
800 IF FLAG=1 THEN 350 
810 LPRINT "TWA(ppm) ";TAB(45) X(1)
820 LPRINT "TWA(mg/m3) ";TAB(45) X(2)
830 LPRINT "STEL(ppm) ";TAB(45) X(3)
840 LPRINT "STEL(mg/m3) ";TAB(45) X(4)
850 LPRINT "MOLECULAR WEIGHT ";TAB(45) X(5)
860 LPRINT "BOILING TEMP(K) ";TAB(45) X(6)
870 LPRINT "CRITICAL TEMP(K) ";TAB(45) X(7)
880 LPRINT "CRITICAL PRES(atm) ";TAB(45) X(8)
890 LPRINT "CRITICAL VOL(cm3/g-mole) ";TAB(45) X(9)
900 LPRINT "VAPOR PRES CONSTANT, A";TAB(45) X(10)
910 LPRINT "VAPOR PRES CONSTANT, B";TAB(45) X(11)
920 LPRINT "VAPOR PRES CONSTANT, C";TAB(45) X(12)
930 LPRINT "LIQUID DENSITY CONSTANT ";TAB(45) X(13)
940 LPRINT "LIQUID DENSITY CONSTANT ";TAB(45) X(14)
950 LPRINT "EFFECTIVE DIAM OF MOLECULE(A) ";TAB(45) X(15)
960 LPRINT "ENERGY OF MOLECULAR INTERACTION(J) ";TAB(45) X(16)
970 LPRINT
980 IF CC$="" THEN 350
990 INPUT "Press <ENTER> to continue, or <Q> to quit ",CR$
1000 IF CR$<> "" THEN 3310
1010 CLOSE #1
1020 INPUT "ENTER CHEMICAL NO. ",CCC
1030 GOTO 340
1040 '-----------------------------------------------
1050 ' CHANGE DATA IN EXISTING FILE
1060 INPUT "ENTER CHEMICAL NO. ",N
1070 NU=N
1080 OPEN "i",#1,S$
1090 OPEN "o",#2,"copy"
1100 FOR I=1 TO N-1
1110	INPUT #1,N,CHM$
1120	PRINT #2,N,CHM$
1130 FOR J=1 TO WW
1140	INPUT #1,X(J)
1150	PRINT #2,X(J)
1160 NEXT J
1170 NEXT I
1180 INPUT #1,N,CHM$
1190 CLS: PRINT
1200 PRINT N,CHM$
1210 FOR I=1 TO WW
1220	    INPUT #1,X(I)
1230 NEXT I
1240 PRINT "TWA(ppm): ";X(1)
1250 PRINT "TWA(mg/m3): ";X(2)
1260 PRINT "STEL(ppm): ";X(3)
1270 PRINT "STEL(mg/m3): ";X(4)
1280 IF CH=2 THEN 1410
1290 PRINT "MOLECULAR WEIGHT ";X(5)
1300 PRINT "BOILING TEMP(K) ";X(6)
1310 PRINT "CRITICAL TEMP(K) ";X(7)
1320 PRINT "CRITICAL PRES(atm) ";X(8)
1330 PRINT "CRITICAL VOL(cm3/g-mole) ";X(9)
1340 PRINT "VAPOR PRES CONSTANT, A";X(10)
1350 PRINT "VAPOR PRES CONSTANT, B";X(11)
1360 PRINT "VAPOR PRES CONSTANT, C";X(12)
1370 PRINT "LIQUID DENSITY CONSTANT ";X(13)
1380 PRINT "LIQUID DENSITY CONSTANT ";X(14)
1390 PRINT "EFFECTIVE DIAM OF MOLECULE(A) ";X(15)
1400 PRINT "ENERGY OF MOLECULAR INTERACTION(J) ";X(16)
1410 PRINT:PRINT
1420 INPUT "IS THIS THE CHEMICAL YOU WISH TO CHANGE (Y/N)? <Y>",YY$
1430 IF YY$="Y" OR YY$="y" OR YY$="" THEN 1530
1440 IF YY$="N" OR YY$="n" THEN 1470
1450 GOSUB 3360
1460 GOTO 1420
1470 CLOSE
1480 PRINT: INPUT "DO YOU WISH TO TRY ANOTHER NUMBER (Y/N)";NN$
1490 IF NN$="N" OR NN$="n" THEN 3310
1500 IF NN$="Y" OR NN$="y" THEN 1060
1510 GOSUB 3360
1520 GOTO 1480
1530 IF CH=2 THEN 1560
1540 PRINT "  (IF VALUE UNKNOWN, ENTER 0)"
1550 INPUT "CHEMICAL NAME ";CHM$
1560 INPUT "TWA(ppm) "; X(1)
1570 INPUT "TWA(mg/m3) "; X(2)
1580 INPUT "STEL(ppm) ";X(3)
1590 INPUT "STEL(mg/m3) ";X(4)
1600 IF CH=2 THEN 1730
1610 INPUT "MOLECULAR WEIGHT ";X(5)
1620 INPUT "BOILING TEMP(K) ";X(6)
1630 INPUT "CRITICAL TEMP(K) ";X(7)
1640 INPUT "CRITICAL PRES(atm) ";X(8)
1650 INPUT "CRITICAL VOL(cm3/g-mole) ";X(9)
1655 PRINT "Vapor pressure units are atmospheres"
1660 INPUT "VAPOR PRES CONSTANT, A";X(10)
1670 INPUT "VAPOR PRES CONSTANT, B";X(11)
1680 INPUT "VAPOR PRES CONSTANT, C";X(12)
1690 INPUT "LIQUID DENSITY CONSTANT ";X(13)
1700 INPUT "LIQUID DENSITY CONSTANT ";X(14)
1710 INPUT "EFFECTIVE DIAM OF MOLECULE(A) ";X(15)
1720 INPUT "ENERGY OF MOLECULAR INTERACTION(J) ";X(16)
1730 PRINT
1740 INPUT "ARE THESE DATA CORRECT (Y/N)? <Y>",YN$
1750 IF YN$="N" OR YN$="n" THEN 1530
1760 IF YN$="Y" OR YN$="y" OR YN$="" THEN 1790
1770 GOSUB 3360
1780 GOTO 1740
1790 PRINT #2,N,CHM$
1800 FOR I=1 TO WW
1810	 PRINT #2,X(I)
1820 NEXT I
1830 IF EOF(1) THEN 3280
1840 INPUT #1,N,CHM$
1850 PRINT #2,N,CHM$
1860 FOR I=1 TO WW
1870	 INPUT #1,X(I)
1880	 PRINT #2,X(I)
1890 NEXT I
1900 GOTO 1830
1910 '----------------------------------------------------
1920 'ADD DATA to file
1930 INPUT "DO YOU WISH TO ADD TO BEGINNING OF FILE (Y/N)";YY$
1940 IF YY$="Y" OR YY$="y" THEN FLAG=1: GOTO 2040
1950 IF YY$="N" OR YY$="n" THEN FLAG=2: GOTO 1980
1960 GOSUB 3360
1970 GOTO 1930
1980 INPUT "DO YOU WISH TO ADD TO END OF FILE (Y/N)";YY$
1990 IF YY$="Y" OR YY$="y" THEN 2040
2000 IF YY$="N" OR YY$="n" THEN 2030
2010 GOSUB 3360
2020 GOTO 1980
2030 INPUT "AFTER WHAT CHEMICAL NUMBER DO YOU WISH TO INSERT NEW CHEMICAL";NUM
2040 OPEN "i",#1,S$
2050 OPEN "o",#2,"copy"
2060 IF FLAG=1 THEN 2340 
2070 IF EOF(1) AND NP=1 THEN 3280
2080 IF EOF(1) THEN 2350
2090 INPUT #1,N,CHM$
2100 IF NP=0 THEN PRINT N,CHM$
2110 IF NP=1 THEN PRINT N+1,CHM$
2120 IF NP=0 THEN PRINT #2,N ELSE PRINT #2,N+1
2130 PRINT #2,CHM$
2140 FOR I=1 TO WW
2150	 INPUT #1,X(I)
2160	 PRINT #2,X(I)
2170 NEXT I
2180 IF NUM=N THEN NP=1: GOTO 2210
2190 GOTO 2070
2200 IF NP=1 THEN 2830
2210 PRINT:PRINT
2220 INPUT "DO YOU WANT THE CHEMICAL TO COME NEXT (Y/N)? <Y>",YY$
2230 IF YY$="Y" OR YY$="y" OR YY$="" THEN 2340
2240 IF YY$="N" OR YY$="n" THEN 2270
2250 GOSUB 3360
2260 GOTO 2210
2270 CLOSE
2280 INPUT "DO YOU WISH TO TRY ANOTHER NUMBER (Y/N)";NN$
2290 IF NN$="N" OR NN$="n" THEN 3350
2300 IF NN$="Y" OR NN$="y" THEN NP=0: GOTO 2030
2310 GOSUB 3360
2320 GOTO 2280   
2330 ' ADD NEW ENTRIES TO FILE
2340 PRINT: PRINT "(IF A VALUE IS UNKNOWN, ENTER <0>)"
2350 PRINT
2360 N=N+1
2370 INPUT "CHEMICAL NAME ";CHM$
2380 INPUT "TWA(ppm) "; X(1)
2390 INPUT "TWA(mg/m3) "; X(2)
2400 INPUT "STEL(ppm) ";X(3)
2410 INPUT "STEL(mg/m3) ";X(4)
2420 INPUT "MOLECULAR WEIGHT ";X(5)
2430 INPUT "BOILING TEMP(K) ";X(6)
2435 PRINT
2440 PRINT " IF BOILING TEMP < AMBIENT TEMP, CHEM IS GAS.  ENTER 0 FOR REMAINING VARIABLES"
2445 PRINT
2460 INPUT "CRITICAL TEMP(K) ";X(7)
2470 INPUT "CRITICAL PRES(atm) ";X(8)
2480 INPUT "CRITICAL VOL(cm3/g-mole) ";X(9)
2490 PRINT 
2500 PRINT "FOR VAPOR PRESSURE, ENTER ONE OF THE FOLLOWING:"
2510 PRINT "   3 CONSTANTS FOR ANTOINE EQ (A,B,C)"
2520 PRINT "   2 CONSTANTS FOR FROST-KALKWARF EQ (A,B,C=0)"
2525 PRINT "     (can not be used if crit. pres. or vol. is unknown)"
2530 PRINT "   VAPOR PRESSURE (ATM)  (A,B=0,C=0)"
2540 PRINT 
2550 INPUT "VAPOR PRES CONSTANT, A";X(10)
2560 INPUT "VAPOR PRES CONSTANT, B";X(11)
2570 INPUT "VAPOR PRES CONSTANT, C";X(12)
2580 PRINT
2590 PRINT "FOR LIQUID DENSITY, ENTER ONE OF THE FOLLOWING:"
2600 PRINT "   2 CONSTANTS FOR GUGGENHEIM EQ (A,B)"
2605 PRINT "    (can not be used if crit. pres. or temp. is unknown)"
2610 PRINT "   LIQUID DENSITY(g/cm3) (A,B=0)"
2615 PRINT "    (if unknown, enter A=1)"
2620 PRINT
2630 INPUT "LIQUID DENSITY CONSTANT, A";X(13)
2640 INPUT "LIQUID DENSITY CONSTANT, B";X(14)
2650 PRINT
2660 PRINT " IF CRITICAL TEMP IS UNKNOWN, ENTER 0 FOR THE NEXT 2 VARIABLES":PRINT
2670 INPUT "EFFECTIVE DIAM OF MOLECULE(A) ";X(15)
2680 INPUT "ENERGY OF MOLECULAR INTERACTION(J) ";X(16)
2685 CLS:XFLAG=1
2686 GOTO 500
2690 PRINT
2700 INPUT "ARE DATA CORRECT (Y/N)? <Y>",NY$
2710 IF NY$="Y" OR NY$="y" OR NY$="" THEN 2770
2720 IF NY$="N" OR NY$="n" THEN 2750
2730 GOSUB 3360
2740 GOTO 2700
2750 N=N-1
2760 GOTO 2350
2770 PRINT #2,N,CHM$
2780 FOR I=1 TO WW
2790	 PRINT #2,X(I)
2800 NEXT I
2810 IF FLAG=1 THEN NP=1
2820 IF NP=1 THEN 2070
2830 PRINT
2840 INPUT "DO YOU WISH TO ENTER MORE DATA (Y/N)";NY$
2850 IF NY$="Y" OR NY$="y" THEN 2350
2860 IF NY$="N" OR NY$="n" THEN 3280
2870 GOSUB 3360
2880 GOTO 2840
2890 '---------------------------------------------
2900 INPUT "ENTER NO. OF CHEMICAL TO BE DELETED ",NUM
2910 OPEN "i",#1,S$
2920 OPEN "o",#2,"copy"
2930 IF NUM=1 THEN 3030
2940 IF EOF(1) THEN 3260
2950 INPUT #1,N,CHM$
2960 IF NP=0 THEN PRINT #2,N ELSE PRINT #2,N-1
2970 PRINT #2,CHM$
2980 FOR I=1 TO WW
2990	 INPUT #1,X(I)
3000	 PRINT #2,X(I)
3010 NEXT I
3020 IF NUM<>N+1 THEN 2940
3030 INPUT #1,N,CHM$
3040 FOR I=1 TO WW
3050	 INPUT #1,X(I)
3060 NEXT I
3070 PRINT N,CHM$
3080 INPUT "IS THIS THE CHEMICAL YOU WISH TO DELETE (Y/N)? <Y>",YY$
3090 IF YY$="Y" OR YY$="y" OR YY$="" THEN 3240
3100 IF YY$="N" OR YY$="n" THEN 3130
3110 GOSUB 3360
3120 GOTO 3080
3130 CLOSE
3140 INPUT "DO YOU WISH TO TRY ANOTHER NUMBER (Y/N)";NN$
3150 IF NN$="N" OR NN$="n" THEN 3310
3160 IF NN$="Y" OR NN$="y" THEN 2900
3170 GOSUB 3360
3180 GOTO 3140 
3190 PRINT #2,N,CHM$
3200 FOR I=1 TO WW
3210	 PRINT #2,X(I)
3220 NEXT I
3230 GOTO 2940
3240 NP=1
3250 GOTO 2940
3260 IF NP=1 THEN PRINT: PRINT "CHEMICAL HAS BEEN DELETED FROM FILE"
3270 FOR I=1 TO 15000: NEXT I
3280 CLOSE
3290 KILL S$
3300 NAME "copy" AS S$
3310 CLOSE
3320 CLS
3330 PRINT: PRINT "SELECT OPTION"
3340 GOTO 40
3350 END
3360 'SUBROUTINE BEEP
3370 PRINT CHR$(7)A
3380 PRINT: PRINT "WRONG ENTRY--PLEASE TRY AGAIN": PRINT
3390 RETURN
