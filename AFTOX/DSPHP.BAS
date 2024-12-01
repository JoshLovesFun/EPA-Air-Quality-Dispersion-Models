10 REM****************************DSPHP**********************************
20 REM  THIS SUB-PROGRAM IS FOR A BUOYANT PLUME FROM A STACK (OPTION 3)
30 DIM TW(4),CONCI(3),CONCP(3)
40 COMMON CONCON,IUNIT,QSS,QPIPE,TTSP,TW(),WWIDTH,U,HINV,STB,WAT,WD,OPTN,ZO,UU
50 COMMON HTCONV,ISTAT,IUNCO,STBV,TAK,H,SIGTH,MFLAG,ZOO,PRES,TNU,TA,MW,CLOUD
60 COMMON SR,A,VISCK,CMTC,TMSP,TB,NU,RENL,EOK,DVAP,DLIQ,SIG,HF,PYN,NO
70 COMMON AMW,HS,TS,VF,AT,IUCON,Z,CONCI(),CONCP(),NNNN,T,CMIN,SVE,TC,VP,AA3
80 COMMON SN$,DTE$,ZT$,LA,LO,ZW,DFT,HET,CHM$,DW,PI,EGA,SCR
90 COMMON COL1,COL4,COL5
100 CLS
110 MWA=28.966
120 RCONS=2870400
130 IF PYN>0 THEN 180
140 PRINT "SORRY--YOU CAN NOT START IN THE MIDDLE OF THE PROGRAM."
150 PRINT "PLEASE ENTER <AFTOX>"
160 FOR I=1 TO 15000: NEXT I
170 END
180 IF MFLAG=1 THEN 1260
190 IF PYN=1 THEN LPRINT
200 COLOR COL4: INPUT "DO YOU KNOW THE MOLECULAR WEIGHT(Y/N)? ",YN$
210 IF YN$="Y" OR YN$="y" THEN IUNCO=0: GOTO 280
220 IF YN$="N" OR YN$="n" THEN IUNCO=1: GOTO 350
230 IF YN$<>"999" THEN 260 
240 COLOR COL5: PRINT "SORRY, YOU CAN NOT BACK UP AT THIS POINT."
250 PRINT "IF YOU WISH TO CHANGE PREVIOUS DATA, YOU MUST START OVER."
260 PRINT CHR$(7)
270 GOTO 200
280 PRINT
290 INPUT "MOLECULAR WEIGHT? ",AMW
300 IF AMW=999 THEN 200
310 IF PYN=1 THEN 330
320 PRINT #3,"MOLECULAR WEIGHT =";AMW: GOTO 340
330 LPRINT "MOLECULAR WEIGHT =";AMW
340 CONCON=RCONS*MWA*TAK/(AMW*PRES*1000000!)
350 IF IUNIT=1 THEN 430
360 INPUT "EMISSION RATE(LBS/MIN)? ",QSS
370 IF QSS=999 THEN 200
380 IF PYN=1 THEN 400
390 PRINT #3,"EMISSION RATE(LBS/MIN) =";QSS: GOTO 410
400 LPRINT "EMISSION RATE(LBS/MIN) =";QSS
410 QSS=QSS/2.2046
420 GOTO 480
430 INPUT "EMISSION RATE(KG/MIN)? ",QSS
440 IF QSS=999 THEN 200
450 IF PYN=1 THEN 470
460 PRINT #3,"EMISSION RATE(KG/MIN) ="QSS: GOTO 480
470 LPRINT "EMISSION RATE(KG/MIN) =";QSS
480 QSS=QSS*1000
490 INPUT "IS EFFLUENT STILL BEING EMITTED(Y/N)? ",YN$
500 IF YN$="Y" OR YN$="y" THEN 610
510 IF YN$="N" OR YN$="n" THEN 550
520 IF YN$="999" THEN 350
530 PRINT CHR$(7)
540 GOTO 490
550 INPUT "ELAPSED TIME OF EMISSION(MIN)? ",TTSP
560 IF TTSP=999 THEN 490
570 IF PYN=1 THEN 590
580 PRINT #3,"ELAPSED TIME OF EMISSIONS IS";TTSP;"MIN": GOTO 650
590 LPRINT "ELAPSED TIME OF EMISSIONS IS";TTSP;"MIN"
600 GOTO 650
610 IF PYN=1 THEN 630
620 PRINT #3, "EFFLUENT IS STILL BEING EMITTED": GOTO 640
630 LPRINT "EFFLUENT IS STILL BEING EMITTED."
640 TTSP=10000
650 IF IUNIT=1 THEN 900
660 INPUT "STACK HEIGHT ABOVE GROUND(FT)? ",HS
670 IF HS=999 AND TTSP=10000 THEN 490
680 IF HS=999 THEN 550
690 IF PYN=1 THEN 710
700 PRINT #3,"STACK HEIGHT ABOVE GROUND(FT) =";HS :GOTO 720
710 LPRINT "STACK HEIGHT ABOVE GROUND(FT) =";HS
720 HS=HS*.3048
730 IF HS>HINV THEN 980
740 COLOR COL4: INPUT "GAS STACK TEMP(F)? ",TS
750 IF TS=999 THEN 660
760 IF PYN=1 THEN 780
770 PRINT #3,"GAS STACK TEMP(F) =";TS: GOTO 790
780 LPRINT "GAS STACK TEMP(F) =";TS
790 TS=(TS-32)/1.8+273.15
800 IF TS=>TAK THEN 830 
810 PRINT CHR$(7): COLOR COL5
820 PRINT "ERROR-STACK TEMP LOWER THAN AMBIENT TEMP - TRY AGAIN": GOTO 740
830 INPUT "VOLUME FLOW RATE(FT3/MIN)? ",VF
840 IF VF=999 THEN 740
850 IF PYN=1 THEN 870
860 PRINT #3,"VOLUME FLOW RATE(FT3/MIN) =";VF: GOTO 880
870 LPRINT "VOLUME FLOW RATE(FT3/MIN) =";VF
880 VF=(VF/60)*.3048^3
890 GOTO 1260
900 INPUT "STACK HEIGHT ABOVE GROUND(M)? ",HS
910 IF HS=999 AND TTSP=10000 THEN 490
920 IF HS=999 THEN 550
930 IF PYN=1 THEN 960
940 PRINT #3,"STACK HEIGHT ABOVE GROUND(M) =";HS
950 GOTO 970
960 LPRINT "STACK HEIGHT ABOVE GROUND(M) =";HS
970 IF HS<=HINV THEN 1090
980 PRINT CHR$(7)
990 COLOR COL5: PRINT "                -----WARNING-----"
1000 PRINT "STACK IS ABOVE INVERSION AND THEREFORE GIVEN MET CONDITIONS DO NOT APPLY"
1010 PRINT "--NO CALCULATION--": PRINT
1020 IF PYN=1 THEN 1040
1030 PRINT #3,"STACK IS ABOVE INVERSION--NO CALCULATION": GOTO 1050
1040 LPRINT "STACK IS ABOVE INVERSION--NO CALCULATION"
1050 COLOR COL4: INPUT "DO YOU WISH TO RUN ANOTHER CASE";NN$
1060 IF NN$="999" THEN 900
1070 IF NN$="Y" OR NN$="y" THEN CHAIN "AFTOX"
1080 END
1090 COLOR COL4: INPUT "GAS STACK TEMP(C)? ",TS
1100 IF TS=999 THEN 900
1110 IF PYN=1 THEN 1130
1120 PRINT #3,"GAS STACK TEMP(C) ="; TS: GOTO 1140
1130 LPRINT "GAS STACK TEMP(C) =";TS
1140 TS=TS+273.15
1150 IF TS=>TAK THEN 1180  
1160 PRINT CHR$(7): COLOR COL5
1170 PRINT "ERROR-STACK TEMP LOWER THAN AMBIENT TEMP - TRY AGAIN": GOTO 1090
1180 COLOR COL4: INPUT "VOLUME FLOW RATE(M3/MIN)? ",VF
1190 IF VF=999 THEN 1090
1200 IF PYN=1 THEN  1220
1210 PRINT #3,"VOLUME FLOW RATE(M3/MIN) =";VF: GOTO 1230
1220 LPRINT "VOLUME FLOW RATE(M3/MIN) =";VF
1230 VF=VF/60
1240 '
1250 '-----CALCULATE EFFECTIVE PLUME HEIGHT (H) AND DISTANCE (DX) AT WHICH PLUME REACHES EQUILIBRIUM HEIGHT-----
1260 F=9.8/PI*VF*(TS-TAK)/TS
1270 IF F=<0 THEN DX=0: DZ=0: GOTO 1470
1280 ISTAB=INT(STB+1)
1290 ON ISTAB GOTO 1310,1310,1310,1310,1390,1420,1420
1300 'UNSTABLE OR NEUTRAL ATMOSPHERE
1310 IF F>=55 THEN 1340
1320 XST=14*F^.625
1330 GOTO 1350
1340 XST=34*F^.4
1350 DX=3.5*XST
1360 DZ=1.6*F^(1/3)*DX^(2/3)/U: 'BRIGGS 1975, EQ 48
1370 GOTO 1470
1380 ' SLIGHTLY STABLE ATMOSPHERE
1390 DTDZ=.02
1400 GOTO 1430
1410 'STABLE ATMOSPHERE
1420 DTDZ=.035
1430 S=9.8*DTDZ/TAK
1440 DZ=2.6*(F/(U*S))^(1/3):         'BRIGGS 1975, EQ 59
1450 IF U<=1 THEN DZ=5*F^.25/S^.375: 'BRIGGS 1975, EQ 56
1460 DX=2.07*U/S^.5
1470 H=HS+DZ
1480 IF H>HINV THEN H=HINV
1490 '
1500 HENG=H/.3048
1510 DXENG=DX/.3048
1520 COLOR COL1
1530 IF IUNIT =1 THEN PRINT "EFFECTIVE PLUME HEIGHT(M) =";INT(H);"AT DISTANCE(M) =";INT(DX)
1540 IF IUNIT =2 THEN PRINT "EFFECTIVE PLUME HEIGHT(FT) =";INT(HENG);"AT DISTANCE(FT) =";INT(DXENG)
1550 IF PYN=1 THEN 1590
1560 IF IUNIT =1 THEN PRINT #3,"EFFECTIVE PLUME HEIGHT(M) =";INT(H);"AT DISTANCE(M) =";INT(DX)
1570 IF IUNIT =2 THEN PRINT #3,"EFFECTIVE PLUME HEIGHT(FT) =";INT(HENG);"AT DISTANCE(FT) =";INT(DXENG)
1580 GOTO 1610
1590 IF IUNIT =1 THEN LPRINT "EFFECTIVE PLUME HEIGHT(M) =";INT(H);"AT DISTANCE(M) =";INT(DX)
1600 IF IUNIT =2 THEN LPRINT "EFFECTIVE PLUME HEIGHT(FT) =";INT(HENG);"AT DISTANCE(FT) =";INT(DXENG)
1610 WWIDTH=1
1620 CHAIN"DSP3"
1630 END
