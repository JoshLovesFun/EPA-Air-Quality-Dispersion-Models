      SUBROUTINE MERGE
C---------------------------------------------------------------------**
C  SUBROUTINE MERGE
C
C  Purpose
C     To combine on-site, rawinsonde/mixing height and surface
C     observation data files into one unformatted data file.
C
C  Called by:     MRPATH
C
C  Calls to:      BANNER, CHROND, ERROR, GMTLST, GREG, HR0024, ICHRND,
C                 JULIAN, MIXCNT, MRHDR, OSDUMP, OSFILL, OSSWAP, UAMOVE
C                 FLIWK1, FLIWK2, FLSDG, FLSFC, FLZI, FLOS
C
C  Version date:  14 July 1988
C
C-----------------------------------------------------------------------
C
      INTEGER      MR2YR, MR4YR, CENTURY
      INTEGER      MR2YR1, MR4YR1, MR2YR2, MR4YR2

      INTEGER      JULIAN, IOST13,IOST22,IOST32, JV,NV, LEV,LEV1,LEV2,
     &             JLEV, IHR, IVBL, NVBL, KDAY, LVBL, IFLAG, NUM
      INTEGER      UARED, SFRED, OSRED, OSCNTR, RCNTR, PCNTR
      INTEGER      UAWRT, SFWRT, OSWRT, DASTAT(10,7)
      CHARACTER*8  VERDAT
      LOGICAL      OSEOF,SFEOF,UAEOF, PRDAY,CUDAY,NXDAY,WR2RPT

Cjop Remove the next declaration after testing
      LOGICAL      FIRSTW

C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'
C
C---  INITIALIZE VARIABLES
      DATA OSEOF/.FALSE./, SFEOF/.FALSE./, UAEOF/.FALSE./
      DATA PRDAY/.FALSE./, CUDAY/.FALSE./, NXDAY/.FALSE./
      DATA UARED /0/, SFRED/0/, OSRED/0/, OSCNTR/0/
      DATA RCNTR/0/, PCNTR/0/
      DATA UAWRT /0/, SFWRT/0/, OSWRT/0/, IFLAG/0/
      DATA PATH/'MR'/, LOC/' MERGE'/, VERDAT/'14 JUL88'/
C
C-----------------------------------------------------------------------
C- THE FOLLOWING IS A MAP OF THE WORK ARRAYS AND THE MET. DATA
C   ARE IN THE ARRAY ELEMENTS
C    IWORK1(10) = UPPER AIR CHRONOLOGICAL DAY (COMPUTATIONAL VBL.)
C    IWORK1(11) = NUMBER OF LEVELS IN A SOUNDING
C    IWORK1(12) = A.M. MIXING HEIGHT
C    IWORK1(13) = P.M. MIXING HEIGHT
C    IWORK1(100) = SURFACE OBS. CHRONOLOGICAL DAY (COMPUTATIONAL)
C    IWORK1(101:122) = SURFACE OBS. DATA, ONE OBSERVATION, TEMPORARY
C    IWORK1(250) = ON-SITE DATA CHRONOLOGICAL DAY (COMPUTATIONAL)
C    IWORK1(900:901) = CURRENT MERGE YEAR : JULIAN DAY
C    IWORK1(910:911) = BEGINNING : ENDING CHRONOLOGICAL DAYS
C                        TO MERGE
C    IWORK1(915) = CURRENT MERGE MONTH, RETURNED FROM ROUTINE 'GREG'
C    IWORK1(916) = CURRENT MERGE DAY, RETURNED FROM ROUTINE 'GREG'
C
C    IWORK2(1:UAML,1:6) = UPPER AIR DATA, ONE SOUNDING, TEMPORARY
C
C    IWORK1(20)   = NUMBER OF UPPER AIR OBS. TO WRITE FOR THE MERGE DAY
C    IWORK1(21)   = NUMBER OF SFC. OBS. TO WRITE FOR THE MERGE DAY
C    IWORK1(22)   = NUMBER OF ON-SITE OBS. TO WRITE FOR THE MERGE DAY
C
C *** OTHER VARIABLE DEFINITIONS
C
C    DASTAT(1:10,1:7) = ARRAY OF STATISTICS FOR 10-DAY SUMMARY TABLE
C
C    IOST12,-22,-32 = IOSTAT VALUES RETURNED FROM READING UNITS 12, 22,
C                     AND 32 (RESP.)
C    JV,NV,LEV,     = LOOP INDICES
C     LEV1,LEV2,JLEV
C     IHR,IVBL,
C     INVBL,LVBL
C    IFLAG          = FLAG RETURNED FROM OSDUMP
C    KDAY           = LOOP INDEX BEGINNING WITH MERGE CHRONOLOGICAL DAY
C                      IWORK1(910) AND ENDING WITH DAY IWORK1(911);
C                      ALSO USED IN DETERMINING CHRONOLOGICAL DAYS IF
C                      THERE WAS NO MERGE EXTRACT CARD
C    UARED,SFRED, = COUNTERS TRACKING THE NUMBER OF READS OF THE FILES
C     OSRED
C    OSCNTR         = COUNTER FOR SUBROUTINE OSFILL
C
C    PRDAY, CUDAY,  = LOGICAL FLAG INDICATING WHETHER OR NOT THERE IS A
C     NXDAY            SOUNDING FOR THE PREVIOUS, CURRENT AND NEXT DAYS
C    USEOF, SFOEF,  = END-OF-FILE FLAGS
C     OSEOF
C    WR2RPT         = FLAG INDICATING IF THE DAILY STATISTICS HAVE BEEN
C                      WRITTEN TO THE SUMMARY FILE
C    VERDAT         = VERSION DATE
C
C=======================================================================
C- SELECT THE UNIT TO WRITE ALL MESSAGES
      IF(STATUS(1,3) .EQ. 2) THEN
       IRD4 = DEV50
      ELSE
       IRD4 = DEVIO
      ENDIF
C
C- ECHO USER INPUT PARAMETERS

      CALL Y2K(PATH, MRYR1, MR2YR1, MR4YR1, CENTURY)
      CALL Y2K(PATH, MRYR2, MR2YR2, MR4YR2, CENTURY)

      WRITE(IRD4,130)
      WRITE(IRD4,200) MRGMO1,MRGDY1,MR4YR1, MRGMO2,MRGDY2,MR4YR2,
     &                 OSLAT,OSLON
C-  WRITE HEADER FOR USER OUTPUT SUMMARY TABLE
      WRITE(IRD4,210)
C
C-  WRITE EACH FILE'S HEADERS TO THE UNFORMATTED OUTPUT FILE AND CHECK
C    THE STATUS OF THE WRITE BY INTERROGATING 'NUM'
      NUM = 5
      CALL MRHDR(PATH,' MRHDR',NUM,DEV13,DEV22,DEV32,DEV40,DEV60)
      IF(NUM .LT. 0) THEN
       MESS = BLNK40
       WRITE(MESS,135)
       CALL ERROR(0,PATH,'E66',LOC,MESS)
       RETURN
      ENDIF
C
       MESS = BLNK40
       WRITE(MESS,400) VERDAT
       BUF03 = '*  '
       BUF80(2)(1:40) = MESS
       WRITE(DEV40) BUF03,BUF80(2)
C
C-----------------------------------------------------------------------
C- FLUSH ALL ARRAYS; INITIALIZE VARIABLES
       CALL FLIWK1
       CALL FLIWK2
       CALL FLSDG(UAMH)
       CALL FLSFC(SFMH)
       CALL FLZI
       CALL FLOS(OSMH)
C
       IWORK1(20) = 0
       IWORK1(21) = 0
       IWORK1(22) = 0
C
C=======================================================================
C- READ EACH FILE AND RETRIEVE THE FIRST OBSERVATION IN EACH FILE
C   IF THERE IS DATA (TO ATTEMPT) TO MERGE
C  NOTE: THE SOUNDING DATA ARE ALREADY ON A 1-24 CLOCK
C
C  PROCESS THE FIRST OBSERVATION OF UPPER AIR DATA, IF THERE ARE DATA
      IF(STATUS(2,5) .LT. 2) THEN
        UAEOF = .TRUE.
        IWORK1(10) = 0
        GO TO 22
       ENDIF
C
       READ(DEV13,1350,IOSTAT=IOST13,ERR=13010,END=13030) UAGYR,
     &      UAGMO, UAGDY, UAGHR, (IWORK1(NV),NV=11,13)
       DO 131 LEV = 1,IWORK1(11)
        READ(DEV13,1355,IOSTAT=IOST13,ERR=13020,END=13030)
     &    (IWORK2(LEV,IVBL),IVBL=1,UAMV)
  131  CONTINUE
       UARED = UARED + 1
C
       UADAYC = JULIAN(UAGYR, UAGMO, UAGDY)
       CALL CHROND( PATH,UAGYR,UADAYC,IWORK1(10) )
       IF(IWORK1(10).LT.0) THEN
        MESS = BLNK40
        WRITE(MESS,140) DEV13
        CALL ERROR(UARED,'UA','E60',LOC,MESS)
        RETURN
       ENDIF
C
       GO TO 22
C
13010  MESS = BLNK40
       WRITE(MESS,255)  IOST13
       GO TO 13025
13020  MESS = BLNK40
       WRITE(MESS,256)  IOST13
13025  CALL ERROR(UARED,'UA','E62',LOC,MESS)
       RETURN
C
13030  UAEOF = .TRUE.
C
C-----------------------------------------------------------------------
C- PROCESS THE FIRST OBSERVATION OF SURFACE DATA, IF THERE ARE DATA
   22  IF(STATUS(3,5) .LT. 2) THEN
         SFEOF = .TRUE.
         IWORK1(100) = 0
         GO TO 32
       ENDIF
C
       READ(DEV22,2265,IOSTAT=IOST22,ERR=22010,END=22030) SFGYR,SFGMO,
     &      SFGDY, SFGHR, (IWORK1(100+IVBL),IVBL=1,23)
       SFRED = SFRED + 1
C
       SFDAYC = JULIAN(SFGYR,SFGMO,SFGDY)
       IF(SFGHR .EQ. 0) THEN
        CALL HR0024(SFGYR,SFDAYC,SFGHR)
        CALL GREG(SFGYR,SFDAYC,SFGMO,SFGDY)
       ENDIF
       CALL CHROND( PATH,SFGYR,SFDAYC,IWORK1(100) )
       IF(IWORK1(100).LT.0) THEN
          MESS = BLNK40
          WRITE(MESS,140) DEV22
          CALL ERROR(SFRED,'SF','E60',LOC,MESS)
          RETURN
       ENDIF
C
       GO TO 32
C
22010  MESS = BLNK40
       WRITE(MESS,267)  IOST22
       CALL ERROR(SFRED,'SF','E63',LOC,MESS)
       RETURN
C
22030  SFEOF = .TRUE.
C
C-----------------------------------------------------------------------
C-    Process the on-site data
   32 CONTINUE
      IF(STATUS(4,5) .LT. 2) THEN
         OSEOF = .TRUE.
         IWORK1(250) = 0
         GO TO 9001
      ENDIF

      CALL OSFILL(5,OSCNTR,DEV32,IOST32)
      OSRED = OSRED + 1

      IF(IOST32 .EQ. 0) THEN
         CONTINUE
      ELSE IF (IOST32 .EQ. 4) THEN
         OSRED = OSRED - 1
         GO TO 32030
      ELSE
         GO TO 32010
      ENDIF

      IF( OSLST .NE. 0) CALL GMTLST(IWORK1(1458),IWORK1(1457),
     &                              IWORK1(1456),IWORK1(1459),OSLST)
      OSDAYC = JULIAN(IWORK1(1458),IWORK1(1457),IWORK1(1456))
      IF(IWORK1(1459) .EQ. 0) THEN
         CALL HR0024(IWORK1(1458),OSDAYC,IWORK1(1459))
         CALL GREG(IWORK1(1458),OSDAYC,IWORK1(1457),IWORK1(1456))
      ENDIF

      CALL CHROND( PATH,IWORK1(1458),OSDAYC,IWORK1(250) )
      IF(IWORK1(250).LT.0) THEN
         MESS = BLNK40
         WRITE(MESS,140) DEV32
         CALL ERROR(OSRED,'OS','E60',LOC,MESS)
         RETURN
      ENDIF
C
      GO TO 9001
C
32010 MESS = BLNK40
      WRITE(MESS,271) IOST32
      CALL ERROR(OSRED,'OS','E64',LOC,MESS)
      RETURN
C
32030 OSEOF = .TRUE.
C
C=======================================================================
C     Begin iteration on days - first determine if the user gave any
C     merge dates.  If not use the earliest date available from the 3
C     pathways as the beginning date and use that day plus 367 as the
C     last day to merge

C     Compute the merge beginning and ending Julian days and chronolog-
C     ical days (IWORK1(910 & 911))

 9001 CONTINUE

      IF( (MRYR1 .EQ. 0) .OR. (MRYR2 .EQ. 0) ) THEN
         KDAY = 0
         IF(IWORK1(10) .GT. 0) THEN
            KDAY = KDAY + 1
            IWORK1(30 + KDAY) = IWORK1(10)
         ENDIF
C
         IF(IWORK1(100) .GT. 0) THEN
            KDAY = KDAY + 1
            IWORK1(30 + KDAY) = IWORK1(100)
         ENDIF
C
         IF(IWORK1(250) .GT. 0) THEN
            KDAY = KDAY + 1
            IWORK1(30 + KDAY) = IWORK1(250)
         ENDIF
C
         IF(KDAY .EQ. 0) THEN
            MESS = BLNK40
            WRITE(MESS,143)
            CALL ERROR(0,PATH,'E67',LOC,MESS)
            RETURN
         ELSE IF( KDAY .EQ. 1) THEN
            IWORK1(910) = IWORK1(31)
         ELSE IF( KDAY .EQ. 2) THEN
            IWORK1(910) = MIN(IWORK1(31),IWORK1(32))
         ELSE
            IWORK1(35)  = MIN(IWORK1(31),IWORK1(32))
            IWORK1(910) = MIN(IWORK1(35),IWORK1(33))
         ENDIF

         IWORK1(911) = IWORK1(910) + 367
         MESS = BLNK40
         WRITE(MESS,144) IWORK1(910),IWORK1(911)
         CALL ERROR(0,PATH,'I67',LOC,MESS)

      ELSE
         MRDAY1 = JULIAN(MRYR1,MRGMO1,MRGDY1)
         MRDAY2 = JULIAN(MRYR2,MRGMO2,MRGDY2)

         CALL CHROND( PATH,MRYR1,MRDAY1,IWORK1(910) )
         IF(IWORK1(910).LT.0) THEN
            MESS = BLNK40
            WRITE(MESS,142)
            CALL ERROR(IWORK1(910),PATH,'E60',LOC,MESS)
            RETURN
         ENDIF

         CALL CHROND( PATH,MRYR2,MRDAY2,IWORK1(911) )
         IF(IWORK1(911).LT.0) THEN
            MESS = BLNK40
            WRITE(MESS,142)
            CALL ERROR(IWORK1(911),PATH,'E60',LOC,MESS)
            RETURN
         ENDIF

      ENDIF

      DO 9000 KDAY = IWORK1(910),IWORK1(911)
         CALL ICHRND(path,kday,iyear,ijulda)

         CALL Y2K(PATH, IYEAR, MR2YR, MR4YR, CENTURY)

         WRITE(*, 610 ) IJULDA, MR4YR
  610    FORMAT('+  Stage 2: Merging Julian day and year ', I3.3, I6)


C        PROCESS THE UPPER AIR DATA
13000    IF(.NOT. UAEOF) THEN
13001       IF(IWORK1(10) .LT. KDAY) THEN
C              The upper air day is before the merge day
               IF(IWORK1(10) .EQ. (KDAY-1)) THEN
                  MIXP1        = IWORK1(12)
                  MIXP2        = IWORK1(13)
C
                  IF(IWORK1(11) .GT. 0) THEN
                     PRDAY = .TRUE.
                     IWORK1(20) = 1
                     CALL UAMOVE
                  ENDIF
               ENDIF
C
               IWORK1(11) = 0
               IWORK1(12) = UAQA(11,2)
               IWORK1(13) = UAQA(12,2)
               READ(DEV13,1350,IOSTAT=IOST13,ERR=13011,END=13031) UAGYR,
     &                    UAGMO, UAGDY, UAGHR, (IWORK1(NV),NV=11,13)
               DO 134 LEV = 1,IWORK1(11)
                  READ(DEV13,1355,IOSTAT=IOST13,ERR=13021,END=13032)
     &                       (IWORK2(LEV,IVBL),IVBL=1,UAMV)
  134          CONTINUE
               UARED = UARED + 1
               UADAYC = JULIAN(UAGYR, UAGMO, UAGDY)
               CALL CHROND( PATH,UAGYR,UADAYC,IWORK1(10) )
               IF(IWORK1(10).LT.0) THEN
                  MESS = BLNK40
                  WRITE(MESS,140) DEV13
                  CALL ERROR(UARED,'UA','E60',LOC,MESS)
                  RETURN
               ENDIF
C
               GO TO 13001
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C- THE UPPER AIR DAY IS EQUAL TO THE MERGE DAY
C
            ELSE IF(IWORK1(10) .EQ. KDAY) THEN
        MIXC1        = IWORK1(12)
        MIXC2        = IWORK1(13)
C
        IF(IWORK1(11) .GT. 0) THEN
         CUDAY = .TRUE.
         IWORK1(20) = IWORK1(20) + 1
         CALL UAMOVE
        ENDIF
C
        IWORK1(11) = 0
        IWORK1(12) = UAQA(11,2)
        IWORK1(13) = UAQA(12,2)
        READ(DEV13,1350,IOSTAT=IOST13,ERR=13011,END=13031) UAGYR,
     &      UAGMO, UAGDY, UAGHR, (IWORK1(NV),NV=11,13)
        DO 137 LEV = 1,IWORK1(11)
         READ(DEV13,1355,IOSTAT=IOST13,ERR=13021,END=13032)
     &    (IWORK2(LEV,IVBL),IVBL=1,UAMV)
  137   CONTINUE
        UARED = UARED + 1
C
        UADAYC = JULIAN(UAGYR, UAGMO, UAGDY)
        CALL CHROND( PATH,UAGYR,UADAYC,IWORK1(10) )
        IF(IWORK1(10).LT.0) THEN
         MESS = BLNK40
         WRITE(MESS,140) DEV13
         CALL ERROR(UARED,'UA','E60',LOC,MESS)
         RETURN
        ENDIF
C
        GO TO 13001
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C-  THE UPPER AIR DAY IS AFTER THE MERGE DAY
       ELSE IF(IWORK1(10) .GT. KDAY) THEN
        IF(IWORK1(10) .EQ. (KDAY+1)) THEN
         MIXN1        = IWORK1(12)
         MIXN2        = IWORK1(13)
C
         IF(IWORK1(11) .GT. 0) THEN
          NXDAY = .TRUE.
          IWORK1(20) = IWORK1(20) + 1
          CALL UAMOVE
         ENDIF
C
        ENDIF
       ENDIF
      ENDIF
      GO TO 22000
C-----------------------------------------------------------------------
C- PROCESSING CONTINUES HERE IF THERE WAS AN ERROR READING THE UPPER
C-     AIR FILE
C      (13011 = HDR ERROR, 13021 = LVL ERROR, 13032 = EOF LVL)
C
13011  MESS = BLNK40
       WRITE(MESS,255)  IOST13
       GO TO 13026
13021  MESS = BLNK40
       WRITE(MESS,256)  IOST13
       GO TO 13026
13032  MESS = BLNK40
       WRITE(MESS,257) IOST13
13026  CALL ERROR(UARED,'UA','E62',LOC,MESS)
       RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C- PROCESSING CONTINUES HERE IF AN END OF FILE WAS ENCOUNTERED
C
13031  UAEOF  = .TRUE.
C
C=======================================================================
C- PROCESS THE SURFACE DATA
C
C     THE SURFACE OBS. DAY IS BEFORE THE MERGE DAY
22000 IF(.NOT. SFEOF) THEN
22001  IF(IWORK1(100) .LT. KDAY) THEN

        READ(DEV22,2265,IOSTAT=IOST22,ERR=22011,END=22031) SFGYR,SFGMO,
     &       SFGDY, SFGHR, (IWORK1(100+IVBL),IVBL=1,23)
C
        SFRED = SFRED + 1
        SFDAYC = JULIAN(SFGYR,SFGMO,SFGDY)
        IF(SFGHR .EQ. 0) THEN
         CALL HR0024(SFGYR,SFDAYC,SFGHR)
         CALL GREG(SFGYR,SFDAYC,SFGMO,SFGDY)
        ENDIF
        CALL CHROND( PATH,SFGYR,SFDAYC,IWORK1(100) )
C
        IF(IWORK1(100).LT.0) THEN
         MESS = BLNK40
         WRITE(MESS,140) DEV22
         CALL ERROR(SFRED,'SF','E60',LOC,MESS)
         RETURN
        ENDIF
C
        GO TO 22001
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ELSE IF(IWORK1(100) .EQ. KDAY) THEN

        IWORK1(21) = IWORK1(21) + 1
C
        SFYR(IWORK1(21))   = SFGYR
        SFMO(IWORK1(21))   = SFGMO
        SFDAY(IWORK1(21))  = SFGDY
        SFHR(IWORK1(21))   = SFGHR
        DO 221 NVBL = 1,23
         SFOBS(IWORK1(21),NVBL+29) = IWORK1(NVBL+100)
  221   CONTINUE
C
        READ(DEV22,2265,IOSTAT=IOST22,ERR=22011,END=22031) SFGYR,SFGMO,
     &       SFGDY, SFGHR, (IWORK1(100+IVBL),IVBL=1,23)
        SFRED = SFRED + 1
C
        SFDAYC = JULIAN(SFGYR,SFGMO,SFGDY)
        IF(SFGHR .EQ. 0) THEN
         CALL HR0024(SFGYR,SFDAYC,SFGHR)
         CALL GREG(SFGYR,SFDAYC,SFGMO,SFGDY)
        ENDIF
        CALL CHROND( PATH,SFGYR,SFDAYC,IWORK1(100) )
        IF(IWORK1(100).LT.0) THEN
         MESS = BLNK40
         WRITE(MESS,140) DEV22
         CALL ERROR(SFRED,'SF','E60',LOC,MESS)
         RETURN
        ENDIF
C
        GO TO 22001
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ELSE IF (IWORK1(100) .GT. KDAY) THEN

        GO TO 32000
       ENDIF
      ENDIF
      GO TO 32000
C
C-----------------------------------------------------------------------
C- PROCESSING CONTINUES HERE IF THERE WAS AN ERROR READING THE SFC. FILE
22011  MESS = BLNK40
       WRITE(MESS,267)  IOST22
       CALL ERROR(SFRED,'SF','E63',LOC,MESS)
       RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C- PROCESSING CONTINUES HERE IF AN END OF FILE WAS ENCOUNTERED ON THE
C-     SURFACE FILE
22031  SFEOF  = .TRUE.
C
C=======================================================================
C- PROCESS THE ON-SITE DATA
32000 IF(.NOT. OSEOF) THEN
32001  IF(IWORK1(250) .LT. KDAY) THEN
        CALL OSFILL(5,OSCNTR,DEV32,IOST32)
        OSRED = OSRED + 1
C
        IF(IOST32 .EQ. 0) THEN
         CONTINUE
        ELSE IF (IOST32 .EQ. 4) THEN
         OSRED = OSRED - 1
         GO TO 32031
        ELSE
         GO TO 32011
        ENDIF
C
        IF( OSLST .NE. 0) CALL GMTLST(IWORK1(1458),IWORK1(1457),
     &                                IWORK1(1456),IWORK1(1459),OSLST)
        OSDAYC = JULIAN(IWORK1(1458),IWORK1(1457),IWORK1(1456))
        IF(IWORK1(1459) .EQ. 0) THEN
           CALL HR0024(IWORK1(1458),OSDAYC,IWORK1(1459))
           CALL GREG(IWORK1(1458),OSDAYC,IWORK1(1457),IWORK1(1456))
        ENDIF

        CALL CHROND( PATH,IWORK1(1458),OSDAYC,IWORK1(250) )
        IF(IWORK1(250).LT.0) THEN
           MESS = BLNK40
           WRITE(MESS,140) DEV32
           CALL ERROR(OSRED,'OS','E60',LOC,MESS)
           RETURN
        ENDIF
C
        GO TO 32001
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ELSE IF(IWORK1(250) .EQ. KDAY) THEN
C       STORE THE DATA AND READ AGAIN
C
        IWORK1(22) = IWORK1(22) + 1
        CALL OSSWAP(IWORK1(22))
C
        CALL OSFILL(5,OSCNTR,DEV32,IOST32)
        OSRED = OSRED + 1
C
        IF(IOST32 .EQ. 0) THEN
         CONTINUE
        ELSE IF (IOST32 .EQ. 4) THEN
         OSRED = OSRED - 1
         GO TO 32031
        ELSE
         GO TO 32011
        ENDIF
C
        IF( OSLST .NE. 0) CALL GMTLST(IWORK1(1458),IWORK1(1457),
     &                                IWORK1(1456),IWORK1(1459),OSLST)
        OSDAYC = JULIAN(IWORK1(1458),IWORK1(1457),IWORK1(1456))
        IF(IWORK1(1459) .EQ. 0) THEN
           CALL HR0024(IWORK1(1458),OSDAYC,IWORK1(1459))
           CALL GREG(IWORK1(1458),OSDAYC,IWORK1(1457),IWORK1(1456))
        ENDIF

        CALL CHROND( PATH,IWORK1(1458),OSDAYC,IWORK1(250) )
        IF(IWORK1(250).LT.0) THEN
           MESS = BLNK40
           WRITE(MESS,140) DEV32
           CALL ERROR(OSRED,'OS','E60',LOC,MESS)
           RETURN
        ENDIF
C
        GO TO 32001
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ELSE IF(IWORK1(250) .GT. KDAY) THEN
        GO TO 42000
       ENDIF
      ENDIF
      GO TO 42000
C-----------------------------------------------------------------------
C- PROCESSING CONTINUES HERE IF AN ERROR PROCESSING THE ON-SITE DATA WAS
C    ENCOUNTERED
32011 MESS = BLNK40
      WRITE(MESS,271) IOST32
      CALL ERROR(OSRED,'OS','E64',LOC,MESS)
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C- PROCESSING CONTINUES HERE IF AN EOF WAS ENCOUNTERED IN THE ON-SITE
C    DATA
32031 OSEOF = .TRUE.
C
C=======================================================================
C- WRITE THE DATA TO THE OUTPUT FILE AND REPOSITION THE UPPER AIR DATA
C
42000 CONTINUE

Cjop  FIRSTW = .TRUE.
Cjop  IF( FIRSTW )THEN
Cjop     OPEN(FILE='MERGE.ASC',UNIT=41)
Cjop     FIRSTW = .FALSE.
Cjop  ENDIF

      CALL ICHRND(PATH,KDAY,IWORK1(900),IWORK1(901))
      IF((IWORK1(900).LT.0) .OR. (IWORK1(900).LT.0) )THEN
        MESS = BLNK40
        WRITE(MESS,141) DEV40
        CALL ERROR(KDAY,PATH,'E61',LOC,MESS)
        RETURN
      ENDIF
      CALL GREG (IWORK1(900),IWORK1(901),IWORK1(915),IWORK1(916))
Cjop
Cjop  WRITE(41,41) IWORK1(900),IWORK1(915),IWORK1(916),IWORK1(901),
Cjop &             MIXP1,MIXP2,MIXC1,MIXC2,MIXN1,MIXN2,IWORK1(20),
Cjop &             IWORK1(21),IWORK1(22)
Cjop1 FORMAT( 13I6 )

      WRITE(DEV40) IWORK1(900),IWORK1(915),IWORK1(916),IWORK1(901),
     &             MIXP1,MIXP2,MIXC1,MIXC2,MIXN1,MIXN2,IWORK1(20),
     &             IWORK1(21),IWORK1(22)
C
      IF(IWORK1(20).GT.0) THEN
       DO 42010 M = 1,IWORK1(20)
Cjop
Cjop    WRITE(41,42) UAYR(M),UAMO(M),UADAY(M),UAHR(M),UALEV(M)
Cjop2   FORMAT( 5I6 )

        WRITE(DEV40) UAYR(M),UAMO(M),UADAY(M),UAHR(M),UALEV(M)
        WRITE(DEV40) ((UAOBS(M,JLEV,JV),JV=1,UAMV),JLEV=1,UALEV(M))
42010  CONTINUE
      ENDIF
C
      IF(IWORK1(21) .GT. 0) THEN
       DO 42020 M = 1,IWORK1(21)
Cjop
Cjop    WRITE(41,44) SFYR(M),SFMO(M),SFDAY(M),SFHR(M)
Cjop4   FORMAT( 4I6 )
Cjop    WRITE(41,45) (SFOBS(M,IVBL+29),IVBL=1,23)
Cjop5   FORMAT( 10I8 )

        WRITE(DEV40) SFYR(M),SFMO(M),SFDAY(M),SFHR(M)
        WRITE(DEV40) (SFOBS(M,IVBL+29),IVBL=1,23)
42020  CONTINUE
      ENDIF
C
      IF(IWORK1(22) .GT. 0) THEN
       DO 42030 LVBL = 1,IWORK1(22)
        CALL OSDUMP(0,LVBL,IFLAG)
        IF(IFLAG .NE. 0) THEN
         RETURN
        ENDIF
42030  CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C *** PREPARE THE STATISTICS FOR THE DAY FOR THE SUMMARY FILE
      CALL MIXCNT
      RCNTR = RCNTR + 1
      DASTAT(RCNTR,1) = IWORK1(900)
      DASTAT(RCNTR,2) = IWORK1(915)
      DASTAT(RCNTR,3) = IWORK1(916)
      DASTAT(RCNTR,4) = IWORK1(20)
      DASTAT(RCNTR,5) = IWORK1(31)
      DASTAT(RCNTR,6) = IWORK1(21)
      DASTAT(RCNTR,7) = IWORK1(22)
      WR2RPT = .FALSE.
C
C *** WRITE THE STATISTICS WHEN 10 DAYS HAVE BEEN ACCUMULATED
C
      IF(RCNTR .EQ. 10) THEN
       WRITE(IRD4,510) (DASTAT(JV,2),DASTAT(JV,3),JV=1,RCNTR)
       WRITE(IRD4,515) (DASTAT(JV,4),JV=1,RCNTR)
       WRITE(IRD4,520) (DASTAT(JV,5),JV=1,RCNTR)
       WRITE(IRD4,525) (DASTAT(JV,6),JV=1,RCNTR)
       WRITE(IRD4,530) (DASTAT(JV,7),JV=1,RCNTR)
C
C *** REINITIALIZE EVERYTHING; WR2RPT INDICATES THE STATS HAVE BEEN
C      WRITTEN; ADD 1 TO THE PAGE COUNTER (PCNTR)
C
       DO 9040 JV = 1,10
        DO 9041 NV = 1,7
         DASTAT(JV,NV) = 0
 9041   CONTINUE
 9040  CONTINUE
C
       RCNTR = 0
       WR2RPT = .TRUE.
       PCNTR = PCNTR + 1
      ENDIF
C
C *** START A NEW PAGE WHEN 8 SETS OF 10-DAY STATISTICS (= 1 PAGE) HAVE
C      BEEN WRITTEN
C
       IF(PCNTR .EQ. 8) THEN
        CALL BANNER ( IRD4 )
        WRITE(IRD4,211)
        PCNTR = 0
       ENDIF
C
C-----------------------------------------------------------------------
C- REPOSITION THE UPPER AIR DATA IF WE DID NOT ENCOUNTER AN END-OF-FILE
C
      IF(.NOT. UAEOF) THEN
C
C *** NO END OF FILE ENCOUNTERED
C
       MIXP1 = MIXC1
       MIXP2 = MIXC2
       IF(CUDAY) THEN
C
C *** THERE IS A CURRENT DAY SOUNDING
C
        IF(NXDAY) THEN
C
C *** THE LAST SOUNDING IN UAOBS IS FOR THE 'NEXT DAY', USE THE NEXT
C      TO LAST SDG AS THE PREVIOUS DAY SDG
C
         UAYR(1) = UAYR(IWORK1(20)-1)
         UAMO(1) = UAMO(IWORK1(20)-1)
         UADAY(1) = UADAY(IWORK1(20)-1)
         UAHR(1) = UAHR(IWORK1(20)-1)
         UALEV(1) = UALEV(IWORK1(20)-1)
         DO 9050 LEV2 = 1,UALEV(1)
          DO 9051 IVBL = 1,UAMV
           UAOBS(1,LEV2,IVBL) = UAOBS(IWORK1(20)-1,LEV2,IVBL)
 9051     CONTINUE
 9050    CONTINUE
         NXDAY = .FALSE.
        ELSE
C
C *** THERE WAS NO NEXT DAY SOUNDING, USE THE LAST SDG IN UAOBS
C
         UAYR(1) = UAYR(IWORK1(20))
         UAMO(1) = UAMO(IWORK1(20))
         UADAY(1) = UADAY(IWORK1(20))
         UAHR(1) = UAHR(IWORK1(20))
         UALEV(1) = UALEV(IWORK1(20))
         DO 9060 LEV2 = 1,UALEV(1)
          DO 9061 IVBL = 1,UAMV
           UAOBS(1,LEV2,IVBL) = UAOBS(IWORK1(20),LEV2,IVBL)
 9061     CONTINUE
 9060    CONTINUE
        ENDIF
        IWORK1(20) = 1
        PRDAY = .TRUE.
        CUDAY = .FALSE.
       ELSE
C
C*** THERE WAS NO CURRENT DAY SOUNDING
C
        IWORK1(20) = 0
        PRDAY = .FALSE.
       ENDIF
C
C *** FLUSH THE REMAINDER OF THE ARRAYS
C
       DO 9072 IHR = IWORK1(20)+1,UAMH
        UAYR(IHR)  = -9
        UAMO(IHR)  = -9
        UADAY(IHR) = -9
        UAHR(IHR)  = -9
        DO 9071 LEV2 = 1,UAML
         DO 9070 IVBL = 1,UAMV
          UAOBS(IHR,LEV2,IVBL) = UAQA(IVBL,2)
 9070    CONTINUE
 9071   CONTINUE
 9072  CONTINUE
       MIXC1 = UAQA(11,2)
       MIXC2 = UAQA(12,2)
       MIXN1 = UAQA(11,2)
       MIXN2 = UAQA(12,2)
      ELSE
C
C ***  IF THERE IS AN EOF ON THE UPPER AIR INPUT FILE, SET EVERYTHING
C       TO MISSING
C
       MIXP1 = UAQA(11,2)
       MIXP2 = UAQA(12,2)
       MIXC1 = UAQA(11,2)
       MIXC2 = UAQA(12,2)
       MIXN1 = UAQA(11,2)
       MIXN2 = UAQA(12,2)
       IWORK1(20) = 0
      ENDIF
C
      DO 9082 IHR = IWORK1(20)+1,UAMH
       UAYR(IHR)  = -9
       UAMO(IHR)  = -9
       UADAY(IHR) = -9
       UAHR(IHR)  = -9
       DO 9081 LEV2 = 1,UAML
        DO 9080 IVBL = 1,UAMV
         UAOBS(IHR,LEV2,IVBL) = UAQA(IVBL,2)
 9080   CONTINUE
 9081  CONTINUE
 9082 CONTINUE
C
      CALL FLSFC(SFMH)
      CALL FLOS(OSMH)
      IWORK1(21) = 0
      IWORK1(22) = 0
      IWORK1(31) = 0
C
 9000 CONTINUE
C
C=======================================================================
C ***  WRITE THE FINAL STATISTICS AND MESSAGE AND
C       RETURN TO THE CALLING PROGRAM
C
      IF( .NOT. WR2RPT) THEN
       WRITE(IRD4,510) (DASTAT(JV,2),DASTAT(JV,3),JV=1,RCNTR)
       WRITE(IRD4,515) (DASTAT(JV,4),JV=1,RCNTR)
       WRITE(IRD4,520) (DASTAT(JV,5),JV=1,RCNTR)
       WRITE(IRD4,525) (DASTAT(JV,6),JV=1,RCNTR)
       WRITE(IRD4,530) (DASTAT(JV,7),JV=1,RCNTR)
      ENDIF
      WRITE(IRD4,501) UARED,SFRED,OSRED
C
      RETURN
C
C=======================================================================
C-FORMAT STATEMENTS
C
  130 FORMAT(/19X,'***** USER INPUT PARAMETERS FOR MERGE *****',/)
  135 FORMAT(' ERROR PROCESSING HEADERS, NO MERGE')
  140 FORMAT(' CHRONOLOGICAL DAY <0 ON UNIT',I2,', RETURN')
  141 FORMAT(' YEAR OR JULIAN DAY <0 FOR MERGE, RETURN')
  142 FORMAT(' INPUT CHRONOLOGICAL DAY <0 , RETURN')
  143 FORMAT(' NO DATA, NO CHRONOLOGICAL DAYS-RETURN')
  144 FORMAT(' CHRON DAYS FROM INPUT DATA:',I5,' -',I5)
  200 FORMAT(22X,'MERGED DATA BEGIN ON (MONTH-DAY-YEAR) ',
     &       I2.2,'-',I2.2,'-',I4,/
     &       22X,'          AND END ON (MONTH-DAY-YEAR) ',
     &       I2.2,'-',I2.2,'-',I4,/
     &     //22X,'THE ON-SITE LATITUDE AND LONGITUDE ARE:',
     &     //22X,'         LATITUDE            ',A8,
     &      /22X,'        LONGITUDE            ',A8/)

  210 FORMAT(/19X,'***** DAILY OUTPUT STATISTICS *****')
  211 FORMAT(/,19X,'***** DAILY OUTPUT STATISTICS, CONTINUED *****')
  255 FORMAT(' ERROR READING SDG. HDR., IOST=',I8)
  256 FORMAT(' ERROR READING SDG. LEVEL, IOST=',I8)
  257 FORMAT(' EOF READING LVL OF DATA, IOST=',I8)
  267 FORMAT(' ERROR READING SFC OBS., IOST=',I8)
  271 FORMAT(' ERROR READING OS DATA, IOST=',I8)
  400 FORMAT('MR *** MERGE, VERSION: ',A8)
  501 FORMAT(/6X,'UPPER AIR OBS. READ:',I5,/6X,'SURFACE OBS. READ:  ',
     &        I5,/6X,'ON-SITE OBS. READ:  ',I5,
     &        //19X,'***** MERGE PROCESS COMPLETED *****')
  510 FORMAT(15X,'MO/DA',10(1X,I2,'/',I2))
  515 FORMAT(1X,'NWS  UPPER AIR  SDGS',2X,9(I2,4X),I2)
  520 FORMAT(1X,'NCDC MIXING  HEIGHTS',3X,9(I1,5X),I1)
  525 FORMAT(1X,'NWS SFC OBSERVATIONS',2X,9(I2,4X),I2)
  530 FORMAT(1X,'ON-SITE OBSERVATIONS',2X,9(I2,4X),I2,/)
 1350 FORMAT(1X,4I2,I5,2(1X,I5))
 1355 FORMAT(6(1X,I5))
C
 2265 FORMAT(1X,4I2,4(1X,I5),6(1X,I5.5),/,8X,5(1X,I5.5),7(1X,I5),1X,i6)
C
      END
C
C=======================================================================
C***** SUBROUTINE UAMOVE                           22 OCTOBER 1987 *****
C
C        THIS SUBROUTINE
C           MOVES THE UPPER AIR DATA FROM THE TEMPORARY WORK
C           ARRAY AND VARIABLES TO THE PERMANENT VARIABLES
C-----------------------------------------------------------------------
      SUBROUTINE UAMOVE
       INTEGER LEV1, IVBL, IOST, ISTAT
C
       INCLUDE 'UA1.INC'
       INCLUDE 'UA2.INC'
       INCLUDE 'WORK1.INC'
C-----------------------------------------------------------------------
       UAYR(IWORK1(20))   = UAGYR
       UAMO(IWORK1(20))   = UAGMO
       UADAY(IWORK1(20))  = UAGDY
       UAHR(IWORK1(20))   = UAGHR
       UALEV(IWORK1(20))  = IWORK1(11)
       DO 138 LEV1 = 1,UALEV(IWORK1(20))
        DO 139 IVBL = 1,UAMV
         UAOBS(IWORK1(20),LEV1,IVBL) = IWORK2(LEV1,IVBL)
  139   CONTINUE
  138  CONTINUE
      RETURN
C
      END
C
C=======================================================================
C- THIS ROUTINE COMPUTES THE NUMBER OF NONMISSING MIXING HEIGHTS
C   WRITTEN FOR THE DAY
C
C   IWORK1(30) COUNTS THE NUMBER OF MISSING VALUES
C   IWORK1(31) COUNTS THE NUMBER OF NONMISSING VALUES
C
C-----------------------------------------------------------------------
      SUBROUTINE MIXCNT
C
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'
C
      IWORK1(30) = 0
      IWORK1(31) = 0
C
      IF( MIXP1. EQ. UAQA(11,2) )THEN
       IWORK1(30) = IWORK1(30) + 1
      ELSE
       IWORK1(31) = IWORK1(31) + 1
      ENDIF
C
      IF( MIXP2. EQ. UAQA(12,2) )THEN
       IWORK1(30) = IWORK1(30) + 1
      ELSE
       IWORK1(31) = IWORK1(31) + 1
      ENDIF
C
      IF( MIXC1. EQ. UAQA(11,2) )THEN
       IWORK1(30) = IWORK1(30) + 1
      ELSE
       IWORK1(31) = IWORK1(31) + 1
      ENDIF
C
      IF( MIXC2. EQ. UAQA(12,2) )THEN
       IWORK1(30) = IWORK1(30) + 1
      ELSE
       IWORK1(31) = IWORK1(31) + 1
      ENDIF
C
      IF( MIXN1. EQ. UAQA(11,2) )THEN
       IWORK1(30) = IWORK1(30) + 1
      ELSE
       IWORK1(31) = IWORK1(31) + 1
      ENDIF
C
      IF( MIXN2. EQ. UAQA(12,2) )THEN
       IWORK1(30) = IWORK1(30) + 1
      ELSE
       IWORK1(31) = IWORK1(31) + 1
      ENDIF
C
      RETURN
C
      END
C
C=======================================================================
        SUBROUTINE OSDUMP( NUMBER,HOUR,IFLAG )
C
C        PURPOSE:         ROUTINE WRITES DATA STORED IN 'HOUR'
C                         POSITION OF OSSOBS AND OSVOBS TO
C                         MERGE OUTPUT FILE.
C

C        LOCAL VARIABLES
C
        INTEGER NUMBER,IFLAG,HOUR
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
C        NUMBER  COUNTER FOR OBSERVATION BEING PROCESSED TO
C                OUTPUT FILE
C        IFLAG   INITIALLY = 0, IF ERRORS OCCUR IN WRITING TO OUTPUT
C                FILE, RESET TO -1.  IF IFLAG = -1, NO MORE ATTEMPTS ARE
C                MADE TO WRITE TO OUTPUT FILE.
C        HOUR    POSITION WITHIN OS-SCALAR AND OS-VECTOR MASTER DATA
C                ARRAYS TO BE WRITTEN TO MERGE OUTPUT DEVICE.
C
C
        PATH = PATHWD(5)
        LOC  = 'OSDUMP'
C
C        CHECK IFLAG
C
        IF( IFLAG.EQ.-1 ) THEN
           RETURN
        END IF
C
C        1.  SWAP DATA TO BUFFER FOR OUTPUT PROCESSING
C        SWAP BUFFER IN FOR PROCESSING
C
C        IWORK1(1300+52) = OSGDY
C        IWORK1(1300+53) = OSGMO
C        IWORK1(1300+54) = OSGYR
C        IWORK1(1300+55) = OSGHR
C        IWORK1(1300+56) = OSGMI
C
        IWORK1(1300+56) = OSDAY(HOUR)
        IWORK1(1300+57) = OSMO(HOUR)
        IWORK1(1300+58) = OSYR(HOUR)
        IWORK1(1300+59) = OSHR(HOUR)
        IWORK1(1300+60) = OSMN(HOUR)
C
        DO 10 I=1,14
           WORK1(1300+I) = OSSOBS(HOUR,I)
   10   CONTINUE
C
        DO 20 I=30,52
           WORK1(1300+I) = OSSOBS(HOUR,I-15)
   20   CONTINUE
C
        DO 40 I=15,29
           DO 30 J=1,OSML
              WORK2(150+J,I-14) = OSVOBS(HOUR,J,I-14)
   30      CONTINUE
   40   CONTINUE
C
C        2.  WRITE FIRST RECORD
C
C        2A.  FIRST PROCESS SCALAR VALUES
C
        IF( OSDNUM(1).EQ.OSTIME) THEN
           WRITE( DEV40,IOSTAT=IRD5 )
     &                ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME )
        ELSE
           DO 50 J=OSTIME+1,OSDNUM(1)
C
              IF( OSDVAR(1,J,2) .LE. 0 ) THEN
C                SCALAR VARIABLE
                 WORK1(J) = WORK1(1300+OSDVAR(1,J,1))
              ELSE
C                VECTOR VARIABLE
                 WORK1(J) = WORK2(150+OSDVAR(1,J,2),OSDVAR(1,J,1)-14)
              END IF
C
   50      CONTINUE
C
           WRITE( DEV40,IOSTAT=IRD5 )
     &          ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME ),
     &          ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
        END IF
C
C       CHECK WRITE STATUS
C
        IF( IRD5 .NE.0 ) THEN
C
           MESS = BLNK40
           ECODE = 'E65'
           WRITE( MESS,1000 ) IRD5
1000       FORMAT(1X,'IOSTAT=',I8,', WRITING OS DATA RECORD')
           CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
C
C          RESET IFLAG
C
           IFLAG = -1
           RETURN
C
        END IF
C
C       LOOP ON REST OF RECORDS
C
        DO 70 I=2,OSDCRD
           DO 60 J=1,OSDNUM(I)
C
              IF( OSDVAR(I,J,2) .LE. 0 ) THEN
C                SCALAR VARIABLE
                 WORK1(J) = WORK1(1300+OSDVAR(I,J,1))
              ELSE
C                VECTOR VARIABLE
                 WORK1(J) = WORK2(150+OSDVAR(I,J,2),OSDVAR(I,J,1)-14)
              END IF
C
   60      CONTINUE
C
           WRITE( DEV40,IOSTAT=IRD5 )
     &              ( WORK1(J),J=1,OSDNUM(I) )
C
C          CHECK WRITE STATUS
C
           IF( IRD5.NE.0 ) THEN
              MESS = BLNK40
              ECODE = 'E65'
              WRITE( MESS,1000 ) IRD5
              CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
              IFLAG = -1
              RETURN
           END IF
C
   70   CONTINUE
C
C       APPARENTLY WE MADE IT!
C
        RETURN
        END
