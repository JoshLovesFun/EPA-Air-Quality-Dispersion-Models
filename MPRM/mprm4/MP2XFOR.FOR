      SUBROUTINE MPCARD( KOUNT,CARD )
C=====================================================================**
C   PURPOSE:  Processes the records for the JB pathway
C
C     The available keywords are:
C
C     VALUE    KEYWRD   ACTION
C
C        3       MET        ASSIGN DISK FILE DEV40 & DEFINE TIME ZONE
C        4       MMP        ASSIGN DISK FILE DEV80 & DEFINE DISP. MODEL
C        5       EXT        START AND STOP DATES FOR PROCESSING
C        6       VBL        SET PROCESSING PARAMETERS
C        7       CHK        RESET INTERNAL 'CHECK' PARAMETERS
C        8       TRA        TURN ON DETAILED AUDIT TRAIL
C        9       LST        LIST IN PRINTED FORM GENERATED METEOROLOGY
C
C-----------------------------------------------------------------------
C     Data declarations
C
      INTEGER WIDTH,ISTAT,IFRM
      CHARACTER CARD*80,NAME*48
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     IFRM    defines the type of disk file to open.
C     WIDTH   computed length of a word within the record
C     NAME    temporary storage location for file/tape-names to be opened.
C     ISTAT   temporary location to report the status of the attempt
C             to open a file/tape.
C
C     Data initialization
C
      PATH = 'MP'
      LOC  = 'MPCARD'
C
C     Initial check on KEYWRD value just to insure there is no error
C     in the program logic up to this point
C
      IF(IRD2.LE.2 .OR. IRD2.GT.NUMKEY(6) ) THEN
C        Program logic error, stop processing
         ECODE = '***'
         MESS = BLNK40
         WRITE( MESS,900 ) IRD1,IRD2
 900     FORMAT(1X,'LOGIC ERROR, IRD1 & IRD2 ARE: ',2I3)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         STOP
      END IF
C
C     1.  Check status to see if this keyword has been processed before.
C         Note, skip test for 'VBL' and 'CHK' records, as these can
C         be repeated.
C
        IF( IRD2.LT.6 ) THEN
C
        IF( STATUS(IRD1,IRD2).NE.0 ) THEN
C       ERROR, WE HAVE SEEN THIS KEYWORD BEFORE
        ECODE = 'E04'
        MESS = BLNK40
        WRITE( MESS,1000 ) KEYWRD(IRD1,IRD2)
1000    FORMAT(1X,'REDUNDANT SPECIFICATION OF KEYWRD ',A3)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        STATUS(IRD1,IRD2) = 1
        END IF
C
        END IF
C
C     2.  DECIPHER CARD.
C
Cjop    GO TO(10,20,30,40,50,60,70),IRD2-2
C
      IF( IRD2 .EQ. 3 )THEN
C        MET record - assign input (merged) meteorology file
         NAME = BLNK48
         ISTAT = 0
         CALL FLOPEN( DEV40,NAME,KOUNT,CARD,2,2,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE

         ELSE
            DISK40 = NAME
            STATUS(IRD1,IRD2) = ISTAT
C
C           Retrieve time zone (conversion factor equal to GMT-LST)
            CALL TZONE( KOUNT,CARD,ISTAT)
C
            IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
               CONTINUE
            ELSE
               STATUS(IRD1,IRD2) = ISTAT
            END IF
C
         END IF
         RETURN
C
      ELSEIF( IRD2 .EQ. 4 )THEN
C        MMP record - assign output meteorology file
C        Decipher dispersion model, so we know whether
C        output file is to be formatted or unformatted.
         CALL MDCARD( KOUNT,CARD,ISTAT )
C
C        Check ISTAT:  if = 2 continue, else skip attempt of
C        assignment of output file.
C
         IF( ISTAT.LT.2 ) THEN
            RETURN

         ELSE
C           Define file format (formatted/unformatted).  Note, this
C           logic needs some improvement to accomodate all the
C           regulatory models.  For now, we attempt to statisfy
C           'RAMMET', 'RTDM' and 'CDM' type formats.
C           IFRM defines whether the output is unformatted (IFRM=2) or
C           formatted (IFRM=1)
C
            IF( MDSTAT.LE.6 ) THEN
               IFRM = 2
            ELSE
               IFRM = 1
            END IF
C
            NAME = BLNK48
            CALL FLOPEN( DEV80,NAME,KOUNT,CARD,3,IFRM,ISTAT )
            IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
               CONTINUE
            ELSE
               DISK80 = NAME
               STATUS(IRD1,IRD2) = ISTAT
            END IF
C
         END IF
         RETURN
C
      ELSEIF( IRD2 .EQ. 5 )THEN
C        EXT record - define extract information
         ISTAT = 0
         CALL EXCARD( KOUNT,CARD,JBYR1,JBGMO1,JBGDY1,
     1                JBYR2,JBGMO2,JBGDY2,JBDAY1,JBDAY2,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
C
C           Compute cronological day for start and stop dates
            CALL CHROND(PATH,JBYR1,JBDAY1,JBCDY1 )
            CALL CHROND(PATH,JBYR2,JBDAY2,JBCDY2 )
C
         END IF
         RETURN
C
      ELSEIF( IRD2 .EQ. 6 )THEN
C        VBL record - define processing information
         ISTAT = 0
         CALL VRCARD( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN
C
      ELSEIF( IRD2 .EQ. 7 )THEN
C        CHK record - alter default range checks for variable
         CALL JBCHK( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN
C
      ELSEIF( IRD2 .EQ. 8 )THEN
C        TRA record - turn on complete audit trail of processing
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = 2
         END IF
         RETURN
C
      ELSEIF( IRD2 .EQ. 9 )THEN
C        LST record - turn on listing of generated meteorology
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = 2
         PCNTRL = 1
         END IF
         RETURN
C
      ENDIF

      END


      SUBROUTINE MPHEAD
C=====================================================================**
C        PURPOSE:     THIS ROUTINE WRITES, IF APPROPRIATE, HEADER(S) TO
C                     OUTPUT FILES.
C
C        LOCAL VARIABLES
C
        INTEGER   NUMBER,ITEST,WIDTH
        CHARACTER CARD*80,OPATH*2
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'UA1.INC'
        INCLUDE 'UA2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'MP1.INC'
        INCLUDE 'WORK1.INC'
C
C        NUMBER  KEEPS TRACK OF HOW MANY HEADER RECORDS,
C                ALLOWING US TO CREATE SPECIAL HEADERS
C                WHEN WE FIRST START TO WRITE A HEADER.
C        ITEST   TEMPORARY STORAGE OF PATHWAY STATUS.
C
C        INITIALIZE VALUES
C
        PATH  = PATHWD(IRD1)
        LOC   = 'MPHEAD'
        OPATH = '  '
C
C        CHECK PATHWAYS'S STATUS WORDS
C        IF ANY ARE LE. 0        -       NO HEADER(S)
C
        IF( JBSTAT.LT.0 .OR. UASTAT.LT.0 .OR. SFSTAT.LT.0
     1      .OR. OSSTAT.LT.0 ) THEN
        RETURN
        END IF
C
C        INITIALIZE HEADER COUNTER
C
        NUMBER = 0
C
C        MAKE SURE DEV80 IS AVAILABLE
C
        IF( STATUS(6,4).LT.2 ) THEN
        RETURN
        END IF
C
C       NONE OF THE GIVEN MODELS CAN ACCEPT HEADER RECORDS
C       SO FOR NOW WE SKIP THIS LOGIC.  NOTE, IT HAS BEEN TESTED
C       AND APPEARS TO WORK JUST FINE.  WE USE THE TEST ON
C       STATUS(6,4) TO FOOL THE FORTRAN COMPILER INTO THINKING
C       THAT STATEMENTS BELOW THE 'GO TO 20' ARE REACHABLE.
C
        IF( STATUS(6,4).EQ.2 ) GO TO 20
C
        REWIND DEV80
        REWIND DEV70
10      READ( DEV70,1000,END=20 ) BUF08(1),CARD
1000    FORMAT( A8,A80 )
        BUF02 = BUF08(1)(1:2)
        CALL DEFINE( KOUNT,80,CARD )
        WIDTH = IC2(2) - IC1(2) + 1
        IF( WIDTH.EQ.3 ) THEN
        BUF03 = CARD( IC1(2):IC2(2) )
        ELSE
        BUF03 = '   '
        END IF
C
C        SKIP INPUT IMAGES 'STA' AND 'FIN'
C
        IF( BUF03.EQ.KEYWRD(IRD1,1) .OR. BUF03.EQ.KEYWRD(IRD1,2) ) THEN
        GO TO 10
        END IF
C
          NUMBER = NUMBER + 1
C
C        SKIP 'SUB-HEADER' RECORDS
C
        IF( CARD(14:19).EQ.'HEADER' ) THEN
        GO TO 10
        END IF
C
C        SEE IF WE NEED A SUB-HEADER RECORD
C
          IF( BUF02.NE.'  ' .AND. BUF02.NE.OPATH ) THEN
          BUF80(10) = BLNK80
          BUF03     = '   '
          WRITE( BUF80(10),1500 ) BUF02
1500      FORMAT( '*T',14X,'HEADER RECORDS FOR ',A2,'-PATHWAY' )
C
          IF( MDSTAT.LT.5 ) THEN
          WRITE( DEV80 ) BUF80(10),BUF03
          ELSE
          WRITE( DEV80,1600 ) BUF80(10),BUF03
1600      FORMAT(A80,A3)
          END IF
C
          NUMBER = NUMBER + 1
          OPATH = BUF02
          END IF
C
C        WRITE HEADER TO FILE
C
        BUF03 = '   '
        WRITE( BUF03,2000,IOSTAT=IRD5 ) BUF08(1)(3:3),BUF08(1)(8:8)
2000    FORMAT( '*',2A1 )
C
          IF( MDSTAT.LT.5 ) THEN
          WRITE( DEV80 ) BUF03,CARD
          ELSE
          WRITE( DEV80,2100 ) BUF03,CARD
2100      FORMAT(A3,A80)
          END IF
C
C        CHECK WRITE STATUS
C
        IF( IRD5.NE.0 ) THEN
        MESS = BLNK40
        ECODE = 'E10'
        WRITE( MESS,3000 ) IRD5,PATHWD(5)
3000    FORMAT(1X,'IOSTAT= ',I8,' HEADER FOR ',A2,'-PATH')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        RETURN
        END IF
C
        GO TO 10
C
20      RETURN
        END


      SUBROUTINE MPPROC( KOUNT )
C=====================================================================**
C   PURPOSE:  To read the header records from the merged meteorological
C             data file.  If appropriate (ie. $ is given in column 2
C             of header card), this record's information is processed.
C
C-----------------------------------------------------------------------
C        LOCAL VARIABLES
C
        INTEGER   NUMBER,WIDTH,DEVICE,TEST
        CHARACTER PREP*3,XTRA*3,CARD*80
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'UA1.INC'
        INCLUDE 'UA2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
C        NUMBER = COUNTER KEEPS TRACK OF HEADER COUNT
C        DEVICE = DEVICE NUMBER OF INPUT FILE FOR HEADERS
C        PREP   = IST THREE CHARACTERS FROM HEADER RECORD
C        CARD   = CARD 'IMAGE' AS READ FOR HEADER FILE
C
C        INITIALIZE
C
        PATH = PATHWD(6)
        LOC  = 'MPPROC'
C
C        PROCESS HEADERS, IF ANY, FROM MERGED METEOROLOGICAL DATA
C        FILE (DEV40)
C
C        A. DEFINE DEVICE NUMBER
C
        DEVICE = DEV40
C
C        B.  REWIND DEVICE
C            LOOP ON A READ THAT CHECKS FOR '*' IN FIRST COLUMN
C            IF NOT "*', STOP PROCESSING, OTHERWISE CONTINUE.
C
        REWIND DEVICE
        CALL EHANDL( 213 )
        NUMBER = 0
        IWORK1(25) = 0
        IWORK1(26) = 0
C
C      USE IWORK1(25) AND IWORK1(26) TO KEEP TRACK OF HOW OFTEN
C      WE HAVE ENCOUNTERED THE UA AND SF LOC-CRDS.  WE ONLY
C      ATTEMPT TO PROCESS THESE ON THE FIRST ENCOUNTER.
C
10      READ( DEVICE,IOSTAT=IRD5,END=100 ) PREP,CARD
C
        NUMBER = NUMBER + 1
C
C        CHECK READ STATUS
C
        IF( IRD5.NE.0 ) THEN
        MESS = BLNK40
        ECODE = 'I03'
        WRITE( MESS,2000 ) IRD5
2000    FORMAT(1X,'IOSTAT= ',I8,' REACHED END OF HEADERS')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        RETURN
        END IF
C
C        LOOK FOR '*'
C
        IF( PREP(1:1).NE.'*' ) THEN
        IRD1 = 1
        RETURN
        END IF
C
C        MUST BE A HEADER, WRITE IT TO THE TEMPORARY FILE
C
C        LOGIC TO PROVIDE A 'TRACE' ON THE HISTORY OF THIS
C        CARD IMAGE.  THIRD CHARACTER OF HEADER CARD PREFIX
C        WILL CONTAIN A LETTER (A-Z), DENOTING WHEN IT WAS
C        INTRODUCED TO THE RUN.  BLNK = CURRENT, A = NEXT TO
C        LAST, B = PREV. ONE TO A, ETC.
C
        IF( PREP(3:3).EQ.' ') THEN
        PREP(3:3) = 'A'
        ELSE
        IRD5 = ICHAR( PREP(3:3) ) + 1
        IF( IRD5.GT.90 ) IRD5 = 90
        IF( IRD5.LT.65 ) THEN
        END IF
        PREP(3:3) = CHAR( IRD5 )
        END IF
C
C       DEFINE PATHWAY FOR THIS CARD IMAGE
C       ASSIGN ALL GENERAL HEADER IMAGES TO PATHWAY JB
C
        CALL DEFINE( NUMBER,80,CARD )
        WIDTH = IC2(1) - IC1(1) + 1
C
        IRD1 = 1
C
        IF( WIDTH.EQ.2 ) THEN
        DO 20 I=1,5
        IF( CARD(IC1(1):IC2(1)).EQ.PATHWD(I) ) THEN
        IRD1 = I
        GO TO 25
        END IF
20      CONTINUE
        END IF
C
25      IF( IRD1.LT.1 .OR. IRD1.GT.5 ) THEN
        STOP
        END IF
C
        CALL WRTCRD( NUMBER,PREP,CARD )
C
C        CHECK TO SEE IF WE SHOULD ACTIVELY PROCESS THIS HEADER
C
        IF( PREP(2:2).EQ.'$' .OR. PREP(2:2).EQ.'@' ) THEN
C
C        PROCESS ACTIVELY
C
        CALL DEFINE( NUMBER,80,CARD )
        CALL FDPATH( NUMBER,CARD )
        CALL FDKEY( NUMBER,CARD )
C
        IF( IRD1.EQ.2 ) CALL UACARD( NUMBER,CARD )
        IF( IRD1.EQ.3 ) CALL SFCARD( NUMBER,CARD )
        IF( IRD1.EQ.4 ) CALL OSCARD( NUMBER,CARD )
C
        END IF
C
C        LOOK FOR UA AND SF LOC-CRDS.
C        (SEARCH FOR 'LOC ' AND 'LOC,'.  HOPEFULLY, THIS
C        WILL ELIMINATE OCCURRANCES OF 'LOCATE' AND
C        'LOCATION ', WHICH MIGHT BE USED IN NORMAL
C        COMMENT STATMENTS.)
C
        IWORK1(24) = 0
        IWORK1(24) = INDEX( CARD,'LOC ' )
        IF( IWORK1(24).EQ.0 ) IWORK1(24) = INDEX( CARD,'LOC,' )
C
        IF( IWORK1(24).GT.0 ) THEN
C
        CALL DEFINE( NUMBER,80,CARD )
        CALL FDPATH( NUMBER,CARD )
        CALL FDKEY( NUMBER,CARD )
C
        IF( IRD1.EQ.2 ) IWORK1(25) = IWORK1(25) + 1
        IF( IRD1.EQ.3 ) IWORK1(26) = IWORK1(26) + 1
C
        IF( IRD1.EQ.2 .AND. IWORK1(25).EQ.1 ) THEN
        CALL UACARD( NUMBER,CARD )
        END IF
C
        IF( IRD1.EQ.3 .AND. IWORK1(26).EQ.1 ) THEN
        CALL SFCARD( NUMBER,CARD )
        END IF
C
        END IF
C
        GO TO 10
C
100     IRD1 = 1
        RETURN
        END


      SUBROUTINE MDCARD( KOUNT,CARD,ISTAT )
C=====================================================================**
C   PURPOSE:  Processes the user's selection of dispersion model,
C             and thereby the format of the meteorological output
C             data file.
C
C-----------------------------------------------------------------------
C       LOCAL VARIABLES
C
        INTEGER      ISTAT
        CHARACTER*80 CARD
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'MP1.INC'
        INCLUDE 'WORK1.INC'
C
C       INTIALIZE VALUES
C
        PATH = PATHWD(6)
        LOC  = 'MDCARD'
C
C        CARD        'IMAGE' DISPERSION MODEL DATA
C        ISTAT       STATUS OF PROCESSING 'IMAGE'
C                    1 = ERROR OCCURRED
C                    2 = ALL OK
C
        ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO DEFINE THE OUTPUT FILE (DEFAULT) ,
C        IRD3 SHOULD EQUAL 4.  LESS THAN 4 MEANS TOO LITTLE
C        INFORMATION HAS BEEN GIVEN; MORE THAN 6 MEANS TOO MUCH
C        INFORMATION IS GIVEN.  IN EITHER CASE, WE CALL IT AN
C        ERROR CONDITION.
C
        IF( IRD3.LT.4 .OR. IRD3.GT.5) THEN
        ECODE = 'E04'
        MESS = BLNK40
        IF( IRD3 .LT. 4 ) THEN
        WRITE( MESS,1000 )
1000    FORMAT(1X,'INSUFFICIENT DATA ON MMP-INPUT')
        ELSE  IF( IRD3 .GT. 5 ) THEN
        WRITE( MESS,2000 )
2000    FORMAT(1X,'SUPERFLUOUS DATA ON MMP-INPUT')
        END IF
C
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        RETURN
        END IF
C
C        2.  ATTEMPT FETCH OF 'MODEL' NAME (IF GIVEN)
C
        IF( IRD3.EQ.5 ) THEN
C
C       CHECK LENGTH OF 'NAME'
C
        IWORK1(10) = IC2(5) - IC1(5) + 1
C
        IF( IWORK1(10).LT.3 .OR. IWORK1(10).GT.8 ) THEN
        MESS = BLNK40
        ECODE = 'E06'
        WRITE( MESS,3000 )
3000    FORMAT(1X,'DISP. MODEL NAME IS IN ERROR')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        RETURN
        END IF
C
        BUF08(1) = BLNK08
        BUF08(1) = CARD( IC1(5):IC2(5) )
C
        ELSE
C       USE DEFAULT DISP. MODEL (ISCST)
C
        BUF08(1) = BLNK08
        BUF08(1) = 'ISCST   '
        END IF
C
C        3.  TEST ON ARRAY OF DISP. MODEL NAMES
C            GIVEN IS DISPMD ARRAY
C
        DO 10 I=1,NDISP
           IF( BUF08(1).EQ.DISPMD(I) ) THEN
              MDSTAT = I
              GO TO 20
           END IF
  10    CONTINUE
C
C       WE GET HERE ONLY IF NO MATCH IS FOUND
C
        MESS = BLNK40
        ECODE = 'E06'
        WRITE( MESS,4000 ) BUF08(1)
4000    FORMAT(1X,A8,':NO MATCH WITH AVAILABLE MODELS')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        MDSTAT = 0
        RETURN
C
20      CONTINUE
C
C        4.  ALL LOOKS OK, RETURN
C
        ISTAT = 2
C
        RETURN
        END
C

        SUBROUTINE VRCARD( KOUNT,CARD,ISTAT )
C=====================================================================**
C   PURPOSE:  This routine processes the user's choices as to how the
C             meteorological data are to be processed in developing
C             the output meteorological data file for the dispersion
C             model specified by the user.
C
C-----------------------------------------------------------------------
C       LOCAL VARIABLES
C
        INTEGER      ISTAT
        CHARACTER*80 CARD
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'MP1.INC'
        INCLUDE 'WORK1.INC'
C
C       INTIALIZE VALUES
C
        PATH = PATHWD(6)
        LOC  = 'VRCARD'
C
C        CARD        'IMAGE' PROCESSOR CONTRAOL INFORMATION
C        ISTAT       STATUS OF PROCESSING 'IMAGE'
C                    1 = ERROR OCCURRED
C                    2 = ALL OK
C
        ISTAT = 2
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR THIS TP BE A VALID 'VBL' CARD IMAGE,
C        IRD3 SHOULD BE AT LEAST 4.  LESS THAN 4 MEANS TOO LITTLE
C        INFORMATION HAS BEEN GIVEN; MORE THAN 7 MEANS TOO MUCH
C        INFORMATION IS GIVEN.  IN EITHER CASE, WE CALL IT AN
C        ERROR CONDITION.
C
        IF( IRD3.LT.4 ) THEN
        ECODE = 'E04'
        MESS = BLNK40
        WRITE( MESS,1000 )
1000    FORMAT(1X,'INSUFFICIENT DATA ON VBL CARD')
        END IF
C
        IF( IRD3.GT.7 ) THEN
        WRITE( MESS,2000 )
2000    FORMAT(1X,'SUPERFLUOUS DATA ON VBL CARD')
        END IF
C
        IF( IRD3.LT.4 .OR. IRD3.GT.7 ) THEN
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        RETURN
        END IF
C
C        2.  ATTEMPT FETCH 'ITEM'
C
        BUF08(10) = '   ITEM '
        BUF04(10) = BLNK04
        CALL GETWRD( 3,KOUNT,CARD,4,4,1,BUF08(10),BUF04(10),ISTAT)
C
C       CHECK ISTAT
C
        IF( ISTAT.EQ.1 ) THEN
        RETURN
        END IF
C
C       LOOK FOR MATCH ON LIST OF 'ITEMS'
C
        DO 20 I=1,NITEM
C
        IF( BUF04(10).EQ.ITEM(I) ) THEN
C
C       FETCH 4-TH WORD TO COMPLETE DEFINITION OF CONTRL
C
        BUF08(9) = '  VALUE '
        BUF08(10) = BLNK08
        CALL GETWRD( 4,KOUNT,CARD,6,8,2,BUF08(9),BUF08(10),ISTAT)
C
C       CHECK ISTAT
C
        IF( ISTAT.EQ.1 ) THEN
        RETURN
        END IF
C
C       FIND MATCH ON 'ACTION'
C
        DO 10 J=1,NACT
C
        IF( BUF08(10)(1:6).EQ.ACTION(J) ) THEN
        CONTRL(I) = J
C
C       DOES IT MAKE SENSE THAT FOR METEOR. PARAMETER (I)
C       WE HAVE REQUESTED IT TO BE PROCESSED USING ACTION (J)?
C
        IF( I.EQ.1 ) THEN
C-------- Wind speed
          IF( J.EQ.1 .OR. J.EQ.2 ) THEN
C            NWS (1) or ONSITE(2)
             CONTINUE
          ELSE
             MESS = BLNK40
             ECODE = 'E70'
             WRITE( MESS,2010 ) ITEM(I), J
2010         FORMAT(1X,A4,' :NO ACTION-',I2,' AVAILABLE ' )
             CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
             ISTAT = 1
             RETURN
          END IF

        ELSE IF( I.EQ.2 ) THEN
C-------- Temperature
          IF( J.EQ.1 .OR. J.EQ.2 ) THEN
C            NWS (1) or ONSITE (2)
             CONTINUE
          ELSE
             MESS = BLNK40
             ECODE = 'E70'
             WRITE( MESS,2010 ) ITEM(I), J
             CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
             ISTAT = 1
             RETURN
          END IF

        ELSE IF( I.EQ.3 ) THEN
C-------- Mixing height
          IF( J.EQ.1 .OR. J.EQ.2 .OR. J.EQ.9 ) THEN
C            NWS(1), ONITEW(2), PXMIX(9)
             CONTINUE
          ELSE
             MESS = BLNK40
             ECODE = 'E70'
             WRITE( MESS,2010 ) ITEM(I), J
             CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
             ISTAT = 1
             RETURN
          END IF

        ELSE IF( I.EQ.4 ) THEN
C-------- Stability
          IF( J.GE.1 .AND. J.LE.7 ) THEN
             CONTINUE
          ELSE
             MESS = BLNK40
             ECODE = 'E70'
             WRITE( MESS,2010 ) ITEM(I), J
             CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
             ISTAT = 1
             RETURN
          END IF

        ELSE
          MESS = BLNK40
          ECODE = 'E70'
          WRITE( MESS,2020 ) ITEM(I)
2020      FORMAT(1X,A4,' : NOT ACTIVATED AS YET ' )
          CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
          RETURN

        END IF
C
C       ---------------------------------------------------
C       Check to see if we should further process this card
C       ---------------------------------------------------
C
        IF( I.EQ.1 .OR. I.EQ.6 ) THEN
C          Fetch stack height info.
           BUF08(9) = ' STACKHT'
           BUF08(10) = BLNK08
           CALL GETWRD( 5,KOUNT,CARD,2,8,1,BUF08(9),BUF08(10),ISTAT )
           IF( ISTAT.EQ.1 ) THEN
              RETURN
           END IF
C
           READ( BUF08(10),3000,IOSTAT=IRD4 ) XRD1
3000       FORMAT( F8.0 )
           IF( IRD4.NE.0 ) THEN
              MESS = BLNK40
              ECODE = 'E03'
              WRITE( MESS,3500 ) BUF08(10)
3500          FORMAT(1X,A8,': ERROR IN STACK HEIGHT INFO.')
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
              RETURN
           ELSE

C             See if STKHGT was previously defined and,
C             if so, given a different value
              IF( STKHGT.NE.0.0 .AND. XRD1.NE.STKHGT ) THEN
                 MESS = BLNK40
                 ECODE = 'E14'
                 WRITE( MESS,3550 ) BUF08(10),STKHGT
3550             FORMAT(1X,A8,': STKHGT ALREADY SET: ',F8.1)
                 CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
                 ISTAT = 1
                 RETURN
              END IF

C             All seems OK, set STKHGT to XRD1 value
              IF( XRD1.GT.0.0 ) THEN
                 STKHGT = XRD1
              ELSE
                 MESS = BLNK40
                 ECODE = 'E06'
                 WRITE( MESS,3555 ) BUF08(10)
3555             FORMAT(1X,A8,': STACK HEIGHT .LE. 0')
                 CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
                 ISTAT = 1
              END IF
           END IF
C
        END IF
C......................................................................

        IF( I.EQ.2 .AND. J.EQ.2) THEN
C          Fetch temperature-measurment height info.
           BUF08(9) = 'TEMPMHGT'
           BUF08(10) = BLNK08
           CALL GETWRD( 5,KOUNT,CARD,1,8,1,BUF08(9),BUF08(10),ISTAT )
           IF( ISTAT.EQ.1 ) THEN
              RETURN
           END IF
C
           READ( BUF08(10),3000,IOSTAT=IRD4 ) XRD1
           IF( IRD4.NE.0 ) THEN
              MESS = BLNK40
              ECODE = 'E03'
              WRITE( MESS,3600 ) BUF08(10)
3600          FORMAT(1X,A8,': ERROR IN TEMP-MEAS HGT INFO.')
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
              RETURN

           ELSE
C             See if TMPHGT was previously defined and,
C             if so, given a different value
              IF( TMPHGT.NE.2.0 .AND. XRD1.NE.TMPHGT ) THEN
                 MESS = BLNK40
                 ECODE = 'E14'
                 WRITE( MESS,3650 ) BUF08(10),TMPHGT
3650             FORMAT(1X,A8,': TMPHGT ALREADY SET: ',F8.1)
                 CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
                 ISTAT = 1
                 RETURN
              END IF

C             All seems OK, set TMPHGT to XRD1 value
              TMPHGT = XRD1
           END IF
C
        END IF
C......................................................................

Crwb
        IF( I.EQ.4 .AND. J.GE.1 ) THEN
C          Fetch anemometer height info.
           BUF08(9) = 'ANEMOMHT'
           BUF08(10) = BLNK08
           CALL GETWRD( 5,KOUNT,CARD,1,8,1,BUF08(9),BUF08(10),ISTAT )
           IF( ISTAT.EQ.1 ) THEN
              RETURN
           END IF
C
           READ( BUF08(10),3000,IOSTAT=IRD4 ) XRD1
           IF( IRD4.NE.0 ) THEN
              MESS = BLNK40
              ECODE = 'E03'
              WRITE( MESS,3660 ) BUF08(10)
3660          FORMAT(1X,A8,': ERROR IN ANEMOM. HGT INFO.')
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
              RETURN

           ELSE
C             see if ANEHGT was previously defined and
C             if so, given a different value
              IF( ANEHGT.NE.10.0 .AND. XRD1.NE.ANEHGT ) THEN
                 MESS = BLNK40
                 ECODE = 'E14'
                 WRITE( MESS,3665 ) BUF08(10),ANEHGT
3665             FORMAT(1X,A8,': ANEHGT ALREADY SET: ',F8.1)
                 CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
                 ISTAT = 1
                 RETURN
              END IF
C             All seems OK, set ANEHGT to user-value in runstream
              ANEHGT = XRD1
           END IF
C
        END IF
C
C......................................................................

        IF( I.EQ.4 .AND. J.EQ.7 ) THEN
C          Fetch index for user-defined variable for stability class
           BUF08(9) = 'STAB_VAR'
           BUF01(1) = BLNK01(1)
           CALL GETWRD( 6,KOUNT,CARD,1,1,1,BUF08(9),BUF01(1),ISTAT )
           IF( ISTAT.EQ.1 ) RETURN
C
           READ( BUF01(1),3700,IOSTAT=IRD4 ) ISCVAR
3700       FORMAT(I1)
           IF( IRD4.NE.0 ) THEN
              MESS = BLNK40
              ECODE = 'E03'
              WRITE( MESS,3705 ) BUF01(1)
3705          FORMAT(1X,A1,': ERROR IN STABILITY CLASS VAR INFO')
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
              RETURN
           ELSE
C             Make sure ISCVAR equals 1, 2, or 3
              IF( ISCVAR.LT.1 .OR. ISCVAR.GT.3 ) THEN
                 MESS = BLNK40
                 ECODE = 'E06'
                 WRITE( MESS,3710 ) BUF08(10),ISCVAR
3710             FORMAT(1X,A8,': NOT EQUAL TO 1,2,OR 3: ',I1)
                 CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
                 ISTAT = 1
                 RETURN
              END IF
C             All seems ok, proceed
           END IF
        END IF
C
C......................................................................

        IF( I.EQ.5 ) THEN
C          FETCH STACK HEIGHT, PLUME TOP HEIGHT, HILL TOP HEIGHT
           BUF08(9) = ' STKHGT '
           BUF08(10) = BLNK08
           CALL GETWRD( 5,KOUNT,CARD,2,8,1,BUF08(9),BUF08(10),ISTAT )
           IF( ISTAT.EQ.1 ) THEN
              RETURN
           END IF
C
           READ( BUF08(10),3000,IOSTAT=IRD4 ) XRD1
           IF( IRD4.NE.0 ) THEN
              MESS = BLNK40
              ECODE = 'E03'
              WRITE( MESS,3500 ) BUF08(10)
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
              RETURN
           ELSE

C             See if STKHGT was previously defined and,
C             if so, given a different value
              IF( STKHGT.NE.0.0 .AND. XRD1.NE.STKHGT ) THEN
                 MESS = BLNK40
                 ECODE = 'E14'
                 WRITE( MESS,3550 ) BUF08(10),STKHGT
                 CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
                 ISTAT = 1
                 RETURN
              END IF
C             All seems OK, set STKHGT to XRD1 value
              IF( XRD1.GT.0.0 ) THEN
                 STKHGT = XRD1
              ELSE
                 MESS = BLNK40
                 ECODE = 'E06'
                 WRITE( MESS,3555 ) BUF08(10)
                 CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
                 ISTAT = 1
              END IF
           END IF
C
C          Fetch plume top height
           BUF08(9) = ' PLMTOP '
           BUF08(10) = BLNK08
           CALL GETWRD( 6,KOUNT,CARD,2,8,1,BUF08(9),BUF08(10),ISTAT )
           IF( ISTAT.EQ.1 ) THEN
              RETURN
           END IF
C
           READ( BUF08(10),3000,IOSTAT=IRD4 ) PLMTOP
           IF( IRD4.NE.0 ) THEN
              MESS = BLNK40
              ECODE = 'E03'
              WRITE( MESS,4000 ) BUF08(10)
4000          FORMAT(1X,A8,': ERROR IN PLUME TOP INFO.')
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
           END IF
C
C          Fetch hill top height
           BUF08(9) = ' HILLHT '
           BUF08(10) = BLNK08
           CALL GETWRD( 7,KOUNT,CARD,2,8,1,BUF08(9),BUF08(10),ISTAT )
           IF( ISTAT.EQ.1 ) THEN
              RETURN
           END IF
C
           READ( BUF08(10),3000,IOSTAT=IRD4 ) HILLHT
           IF( IRD4.NE.0 ) THEN
              MESS = BLNK40
              ECODE = 'E03'
              WRITE( MESS,4500 ) BUF08(10)
4500          FORMAT(1X,A8,': ERROR IN HILL HEIGHT INFO.')
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
           END IF
C
        END IF
C
        RETURN
        END IF
C
10      CONTINUE
C
C       ERROR CONDITION TO REACH THIS POINT
C       NO MATCH ON 'ACTION' LIST.
        MESS = BLNK40
        ECODE = 'E06'
        WRITE( MESS,5000 ) BUF08(10)
5000    FORMAT(1X,A8,': NO MATCH WITH ACTION ARRAY')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
C
        RETURN
C
        END IF
C
20      CONTINUE
C
C       ERROR CONDITION TO REACH THIS POINT
C       NO MATCH ON 'ITEM' LIST
        MESS = BLNK40
        ECODE = 'E06'
        WRITE( MESS,6000 ) BUF04(10)
6000    FORMAT(1X,A4,': NO MATCH WITH ITEM ARRAY')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
C
        RETURN
        END
C

        SUBROUTINE MPTEST
C=====================================================================
C   PURPOSE:  This routine processes the STATUS array to determine
C             what input (without errors) are available and
C             whether it is meaningful to continue processing.
C
C-----------------------------------------------------------------------
C        LOCAL VARIABLES
C
        INTEGER ISTAT
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'MP1.INC'
        INCLUDE 'WORK1.INC'
C
C        INITIALIZE VALUES
C
        PATH = 'JB'
        LOC  = 'MPTEST'
C
C        PROCESS 'JOB' PATHWAY
C
        JBSTAT = 0
        ISTAT  = 0
C
        DO 10 I=1,NUMKEY(1)
C
        IF( STATUS(1,I).EQ.1 ) THEN
        ISTAT = ISTAT + 1
        END IF
C
10        CONTINUE
C
        IF( ISTAT.GT.0 ) THEN
        MESS = BLNK40
        ECODE = 'E12'
        WRITE( MESS,1000 ) ISTAT, PATHWD(1)
1000    FORMAT(1X,'HAVE DETECTED ',I3,' ERRORS ON PATHWAY-',A2)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        JBSTAT = -1
        ISTAT = 1
        END IF
C
C        MAKE SURE WE HAVE OPENED AN ERROR REPORT FILE
C
        IF( STATUS(1,5).EQ.2 ) THEN
        CONTINUE
        ELSE
        MESS = BLNK40
        ECODE = 'E07'
        WRITE( MESS,1500 )
1500    FORMAT(1X,'SUMMARY: NO ACCESS TO AN ERROR FILE')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        JBSTAT = -1
        END IF
C
C       NOW TEST MP-PATHWAY STATUS
C
        IRD5 = 0
        DO 20 I=1,NUMKEY(6)
C
        IF( STATUS(6,I).EQ.1 ) THEN
        IRD5 = IRD5 + 1
        END IF
C
20        CONTINUE
C
        IF( IRD5.GT.0 ) THEN
        MESS = BLNK40
        ECODE = 'E12'
        WRITE( MESS,1000 ) IRD5, PATHWD(6)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        JBSTAT = -1
        ISTAT = 1
        END IF
C
C        MAKE SURE WE HAVE OPENED AN INPUT FILE OF
C        MERGED METEOROLOGY
C
        IF( STATUS(6,3).EQ.2 ) THEN
        CONTINUE
        ELSE
        MESS = BLNK40
        ECODE = 'E07'
        WRITE( MESS,2000 )
2000    FORMAT(1X,'SUMMARY: NO ACCESS TO MERGED MET FILE')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        JBSTAT = -1
        END IF
C
C        MAKE SURE WE HAVE OPENED AN OUTPUT FILE FOR
C        PROCESSED DIFFUSION METEOROLOGY
C
        IF( STATUS(6,4).EQ.2 ) THEN
        CONTINUE
        ELSE
        MESS = BLNK40
        ECODE = 'E07'
        WRITE( MESS,2500 )
2500    FORMAT(1X,'SUMMARY: NO ACCESS TO OUTPUT MET FILE')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        JBSTAT = -1
        END IF
C
C       If LONGZ or SHORTZ has been selected as the dispersion model,
C       set the job status to -1 (MPRM does not support them yet)
C
        IF( (MDSTAT.EQ.6) .OR. (MDSTAT.EQ.10) ) THEN
           MESS = BLNK40
           WRITE(MESS,2600) DISPMD(MDSTAT)
           CALL ERROR(0,PATH,'E70',LOC,MESS)
           ISTAT  =  1
           JBSTAT = -1
        ELSE
           CONTINUE
        ENDIF
2600    FORMAT(' MODEL (',A8,') NOT SUPPORTED IN MPRM')
C
C        PROCESS OS PATHWAY (IF APPROPRIATE) TO INSURE DATA
C        MAP AND FORMATS ARE SUFFICIENT FOR PROCESSING
C
        CALL OSTEST( ISTAT )
        IF( ISTAT.EQ.1 ) THEN
        OSSTAT = -1
        JBSTAT = -1
        END IF
C
        IF(JBSTAT.NE.0) THEN
        CONTINUE
        ELSE
        JBSTAT = 1
        END IF
C
        RETURN
        END
C

        SUBROUTINE TZONE( NUMBER,CARD,TEST )
C=====================================================================**
C   PURPOSE:  This routine looks for an integer value just after the
C             filename for the merged input meteorology file.  This
C             value is interpreted as the factor to convert GMT time
C             to LST time (conversion factor is assumed to be equal
C             to GMT-LST).
C
C             Hence, locations with west longitude should have
C             a factor .ge. zero and locations with east longitude
C             should have a factor .le. zero.
C
C-----------------------------------------------------------------------
C       LOCAL VARIABLES
C
        CHARACTER CARD*80
        INTEGER   TEST,NUMBER
C
        INCLUDE 'MP1.INC'
        INCLUDE 'WORK1.INC'
C
C       INITIALIZE VALUES
C
        LOC  = ' TZONE'
        PATH = 'MP'
C
C       1.  MAKE SURE WE HAVE A VALUE TO READ
C
        IF( IRD3.LT.5 ) THEN
C
        MESS = BLNK40
        ECODE = 'E04'
        WRITE( MESS,1000 )
1000    FORMAT(1X,'ERROR: NO TIME ZONE GIVEN ON MET-CRD')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        TEST = 1
        RETURN
        END IF
C
C       2.  ATTEMPT FETCH 'ZONE'
C
        BUF08(10) = '  ZONE  '
        BUF04(10) = BLNK04
        CALL GETWRD( 5,NUMBER,CARD,1,4,1,BUF08(10),BUF04(10),TEST )
C
C       CHECK TEST
C
        IF( TEST.EQ.1 ) THEN
        RETURN
        END IF
C
C       CONVERT VALUE TO INTEGER
C
        READ( BUF04(10),2000,IOSTAT=IRD4 ) ZONE
2000    FORMAT( I4 )
C
C       CHECK READ STATUS
C
        IF( IRD4.NE.0 ) THEN
C
        MESS = BLNK40
        ECODE = 'E03'
        WRITE( MESS,3000 ) IRD4
3000    FORMAT(1X,'IOSTAT= ',I8,' FETCHING ZONE' )
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        TEST = 1
        RETURN
        END IF
C
C       WE CAN NOT CHECK TO INSURE THE CORRECT SIGN
C       UNTIL WE HAVE A WORKING VALUE FOR THE LOCATION'S
C       LONGITUDE.
C
        TEST = 2
C
        RETURN
        END
C
        SUBROUTINE JBCHK(N,CARD,NTEST)
        CHARACTER CARD*80
        NTEST = 2
        RETURN
        END
C

        SUBROUTINE MPSTUP
C=====================================================================**
C   PURPOSE:  This routine controls the processing of the user supplied
C             setup records in the runstream file.
C
C   Revisions:
C
C     1/19/96  Pacific Environmental Services, Inc.
C              Added code to allow onsite location and site characteris-
C              tics to be processed in stage 3.
C-----------------------------------------------------------------------
C       LOCAL VARIABLES
C
Cjop    INTEGER DEV90, IST90
        CHARACTER NAME*48
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'MP1.INC'
        INCLUDE 'WORK1.INC'
C
Cjop        DATA DEV90/90/
C
C====== 1.  Initialize values
C
        PATH = 'MP'
        LOC  = 'MPSTUP'
        JBSTAT = 0
        KOUNT = 0
        BUF80(1) = BLNK80
C
C====== 2. Process setup cards
C
10      KOUNT = KOUNT + 1
        BUF80(1) = BLNK80
        READ( DEVIN,2500,ERR=70,IOSTAT=IRD4,END=80 ) (BUF01(I),I=1,80)
2500    FORMAT(80A1)
C
C====== 3. Define location of words in image               ---- CALL DEFINE
C
        CALL DEFINE( KOUNT,80,BUF80(1) )
C
C====== 4. Check IRD3 (returned through common from DEFINE)
C           It contains the number of words found.  If it
C           is zero, we have a blank card.
C
        IF( IRD3.EQ.0 ) THEN
           GO TO 10
        END IF

C---- 5.  Image is not blank, is this a comment record (asterisk in
C         column 1 or 2)?  If so, read another record

      IF( BUF01(1) .eq. '*'  .OR.  BUF01(2) .eq. '*' )THEN
         ECODE = 'I00'
         WRITE( MESS,2550 )
2550     FORMAT(' COMMENT CARD FOUND, SKIP TO NEXT IMAGE')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         GO TO 10
      ENDIF

C
C====== 6. The record is not blank or a comment card,      ---- CALL FDPATH
C          define the pathway id
C
        CALL FDPATH( KOUNT,BUF80(1) )
C
C====== 7. Check IRD1 (returned through common from FDPATH)
C           It contains the pathway ID number, which if
C           valid is 1.
C
        IF( IRD1.EQ.1 .OR. IRD1.EQ.4 .OR. IRD1.EQ.6 ) THEN
           CONTINUE
        ELSEIF( IRD1.EQ.2 .OR. IRD1.EQ.3 .OR. IRD1.EQ.5)THEN
           ECODE = 'W16'
           WRITE( MESS,2600 ) PATHWD(IRD1)
2600       FORMAT(' INVALID PATHWAY FOR STAGE 3: ',A2)
           CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
           GO TO 10
        END IF
C
C====== 8. Found valid pathway, check for valid keyword    ---- CALL FDKEY
c
        CALL FDKEY( KOUNT,BUF80(1) )
C
C====== 9. Check IRD2 (returned through common from FDKEY)
C           It contains the keywrd value found.  If no valid
C           match was found IRD2 will equal 0.
C
        IF( IRD2.EQ.0 ) THEN
           ECODE = 'E00'
           WRITE( MESS,2700 ) KEYWRD(IRD1,IRD2),PATHWD(IRD1)
2700       FORMAT(' INVALID KEYWORD (', A5, ') FOR PATH ', A5 )
           CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
           GO TO 10
        END IF

C       Only the LOC & SFC keywords are allowable for ONSITE data in Stage 3
        IF( IRD1 .EQ. 4 )THEN
            IF( IRD2.NE.1  .AND.  IRD2.NE.2  .AND.
     &          IRD2.NE.6  .AND.  IRD2.NE.16 )THEN
               ECODE = 'W16'
               WRITE( MESS,2800 ) KEYWRD(IRD1,IRD2)
2800           FORMAT(' INVALID KEYWORD FOR OS, STG 3 ',A8)
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               GO TO 10
            ENDIF
        ENDIF
C
C====== 10. Write 'CARD' image to temporary file for       ---- CALL WRTCRD
C            possible later use.
C
        BUF03 = '   '
        CALL WRTCRD( KOUNT,BUF03,BUF80(1) )
C
C====== 11. Check for STA keyword for this pathway;
C           The STA image is valid for onsite data to allow the
C           user to define and/or surface characteristics in Stage 3.
C
        IF( IRD2.EQ.1 ) THEN
C
           IF( IRD1.NE.4 .AND. STATUS(IRD1,IRD2).NE.0 ) THEN
C             This card has been seen before, something is amiss
              STATUS(IRD1,IRD2) = 1
              ECODE = 'E01'
              WRITE( MESS,3000 ) PATHWD(IRD1)
3000          FORMAT(' DUPLICATE STA CARD FOR PATHWAY-',A2)
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
C
           ELSE
              STATUS(IRD1,IRD2) = 2
           END IF
           GO TO 10
        END IF
C
C====== 13.  Check to see if this is a FIN card
C            The FIN image is valid for onsite data to allow the
C            user to define surface characteristics in Stage 3
C            and a location in Stage 2
C
        IF( IRD2.EQ.2 ) THEN
           IF( IRD1.NE.4  .AND.  STATUS(IRD1,IRD2).NE.0 ) THEN
C             This card has been seen before, write error message
              STATUS(IRD1,IRD2) = 1
              ECODE = 'E01'
              MESS = BLNK40
              WRITE( MESS,3500 ) PATHWD(IRD1)
3500          FORMAT(1X,'DUPLICATE FIN CARD FOR PATHWAY-',A2)
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )

           ELSE
C             If MP FIN image, then process previously written
C             header records, as appropriate, for this pathway.
              STATUS(IRD1,IRD2) = 2
              IF( IRD1.EQ.6 ) THEN
                 CALL MPPROC( KOUNT )
              END IF
           END IF
           GO TO 10
        END IF
C
C====== 13.  Check to see if this is the 'END OF JOB/RUN CARD'
C
        IF( IRD1.EQ.1 .AND. IRD2.EQ.6) THEN
           STATUS(IRD1,IRD2) = 2
           GO TO 90
        END IF
C
C====== 14.  Call appropriate routine to decipher this image
C             for this pathway.
C
        IF( IRD1.EQ.1 ) CALL JBCARD( KOUNT,BUF80(1) )
        IF( IRD1.EQ.6 ) CALL MPCARD( KOUNT,BUF80(1) )
        IF( IRD1.EQ.4 ) CALL OSCARD( KOUNT,BUF80(1) )
C
        GO TO 10
C
C====== We get here because we encountered a read error in
C       fetching the next BUF80(1) input image.
C
70      CONTINUE
        MESS = BLNK40
        ECODE = 'E03'
        WRITE( MESS,4000 ) IRD4
4000    FORMAT(1X,'IOSTAT= ',I8,' READING INPUT CARD' )
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        GO TO 10
C
80      CONTINUE
C
C====== We got here by encountering an EOF status
C
        ECODE = 'I19'
        MESS = BLNK40
        WRITE( MESS,5000 ) DEVIN
5000    FORMAT(1X,'FOUND "END OF FILE" ON DEVICE DEVIN ',I2)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
C
        GO TO 100
C
90      CONTINUE
C
C====== We got here by encountering the 'END OF JOB/RUN CARD'
C
        ECODE = 'I19'
        MESS = BLNK40
        WRITE( MESS,6000 )
6000    FORMAT(1X,'ENCOUNTERED END OF "JOB/RUN CARD" ')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
C
C====== 15. We can now test the status array for           ---- CALL MPTEST
C           completeness
 
100     CALL MPTEST
 
 
Cjop     15. LOAD RANDOM NUMBERS INTO ARRAY-IRND

Cjop         The random numbers are now loaded through block data

C====== 16. We now construct and write header records      ----CALL MPHEAD
C           to appropriate output files.
C           (Note, only if call to 'TEST' shows no errors
C            detected on all (repeat: all) pathways, will
C            header(s) be constructed and written)
C
         CALL MPHEAD
C
        RETURN
        END
