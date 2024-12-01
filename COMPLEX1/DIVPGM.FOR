10 '**************************************************************************
20 '*                                                                        *
30 '*                       DIVPGM (DATED 90053)                             *
40 '*                                                                        *
50 '*             *** SEE UTILITY SECTION OF SCRAM BBS ***                   *
60 '*                                                                        *
70 '*                           919-541-5742                                 *
80 '*                                                                        *
90 '**************************************************************************
100 '
110 '                           INTRODUCTION
120 '
130 REM     THIS PROGRAM WAS DESIGNED AS A QUICK AND EASY METHOD OF SEPARATING
140 REM  FORTRAN SOURCE CODE INTO ITS SUBROUTINES BY DETECTING AN 'END'
150 REM  STATEMENT. WHEN AN END STATEMENT IS DETECTED, THE OLD OUTPUT FILE IS
160 REM  CLOSED AND A SEARCH IS BEGUN IN THE NEXT LINE OR LINES OF CODE FOR
170 REM  THE WORD: SUBROUTINE, FUNCTION OR BLOCK. IF THE WORD BLOCK IS READ,
180 REM  THE NEW OUTPUT FILE IS CALLED BLOCK.FOR. IF THE WORD SUBROUTINE OR
190 REM  FUNCTION IS FOUND, THE REST OF THE LINE IS SEARCHED FOR THE
200 REM  SUBROUTINE OR FUNCTION NAME. THAT NAME IS USED IN OPENING A NEW
210 REM  OUTPUT FILE WITH A .FOR EXTENTION. NEW LINES OF CODE ARE THEN READ
220 REM  FROM THE SOURCE CODE FILE AND WRITTEN TO THE NEW FILE ALONG WITH THE
230 REM  SUBROUTINE, FUNCTION OR BLOCK LINE. THIS CYCLE CONTINUES UNTIL THE
240 REM  PROGRAM TERMINATES WHEN AN 'END OF FILE' IS DETECTED OR WHEN AN ERROR
250 REM  OCCURS DURING DISK OPERATIONS.
260 REM
270 REM     THIS PROGRAM CAN SPLIT A 625 KILOBYTE FORTRAN PROGRAM INTO 60
280 REM  SUBROUTINES IN LESS THAN 4 MINUTES ON A 12 Mhz IBM-PC/AT COMPATIBLE.
290 REM
300 REM
310 REM
320 INPUT "ENTER THE NAME OF THE FILE TO BE DIVIDED          :", OFN$
330 INPUT "ENTER THE FULL PATH NAME FOR THE OUTPUT FILES     :", PTH$
340  IF LEFT$(PTH$,1) <> "\" THEN PTH$ = "\" + PTH$
350 CHDIR PTH$
360 INPUT "ENTER THE NAME OF THE FILE TO BE WRITTEN TO FIRST :", AN$
370 INPUT "ENTER THE NAME OF THE FILE FOR STORING FILENAMES  :", SFN$
380 PRINT
390 INPUT"DO YOU WANT TO SEE THE SOURCE CODE LINES AS THEY ARE READ (Y/N)";Q$
400    IF Q$ = "Y" OR Q$ = "y" THEN Q = 1 : GOTO 430
410    IF Q$ = "N" OR Q$ = "n" THEN Q = 0 : GOTO 430
420     GOTO 390
430   ON ERROR GOTO 1040
440  OPEN "I", 1, OFN$
450  OPEN "O", 2, AN$
460  OPEN "O", 3, SFN$
470   PRINT #3, AN$
480  LINE INPUT #1, L$
490    IF Q THEN PRINT L$
500  LTH = LEN(L$)
510   IF LTH < 9  THEN GOTO 1020
520   IF LEFT$(L$,6) <> "      " THEN GOTO 1020
530   IF LEFT$(L$,LTH) = SPACE$(LTH) THEN GOTO 1020
540   FOR I = 7 TO LTH
550    IF MID$(L$,I,1) =  " " THEN GOTO 610
560    IF MID$(L$,I,1) <> "E" THEN GOTO 1020
570    IF MID$(L$,I,3) <> "END" THEN GOTO 1020
580    IF ((I+2) = LTH) THEN GOTO 630
590    IF (LTH < 73 AND MID$(L$,I+3,LTH-I-2 )= SPACE$(LTH-I-2)) THEN GOTO 630
600    IF (LTH > 72 AND MID$(L$,I+3,(72-I-2)) = SPACE$(72-I-2))  THEN GOTO 630
610   NEXT I
620  GOTO 1020
630   PRINT #2, L$
640   CLOSE #2
650 LINE INPUT #1,L$
660    IF Q THEN PRINT L$
670  LTH = LEN(L$)
680   IF LTH < 12 THEN GOTO 650
690   IF LEFT$(L$,6) <> "      " THEN GOTO 650
700   IF LTH < 73 AND L$ = SPACE$(LTH) THEN GOTO 650
710   IF LTH > 72 AND LEFT$(L$,72) = SPACE$(72) THEN GOTO 650
720  FOR I = 7 TO 20
730    IF MID$(L$,I,1) = " " THEN GOTO 850
740    IF MID$(L$,I,10) = "SUBROUTINE" THEN CODE = I + 10 : GOTO 920
750    IF MID$(L$,I,8) = "FUNCTION" THEN CODE = I + 8 : GOTO 920
760    IF MID$(L$,I,5) <> "BLOCK" THEN GOTO 850
770     AN$ = "BLOCK.FOR"
780     CODE = I + 10
790     IF CODE > LTH THEN GOTO 1000
800     FOR J = CODE TO LTH
810      IF J > 72 THEN GOTO 1000
820      IF MID$(L$,J,1) <> " " THEN GOTO 920
830     NEXT J
840    GOTO 1000
850  NEXT I
860   PRINT:PRINT" A SUBROUTINE, FUNCTION NOR A BLOCK STATEMENT COULD BE ";
870   PRINT "FOUND AFTER A BONAFIDE"
880   PRINT "END STATEMENT. THE FOLLOWING IS THE LAST LINE READ:"
890   PRINT L$
900    END
910 REM FIND AND CREATE A FILENAME
920  FOR I = CODE TO LTH
930   IF MID$(L$,I,1) <> " " THEN GOTO 950
940  NEXT I
950  FOR J = I TO LTH
960   IF MID$(L$,J,1) = " " THEN GOTO 990
970   IF MID$(L$,J,1) = "(" THEN GOTO 990
980  NEXT J
990 AN$ = MID$(L$,I,J-I) + ".FOR"
1000 OPEN "O", #2, AN$
1010  PRINT #3, AN$
1020  PRINT #2, L$
1030 GOTO 480
1040  CLOSE
1050 END
