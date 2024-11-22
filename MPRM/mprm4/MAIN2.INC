C     Include File:  MAIN2.INC

C       WE ANTICIPATE THESE WILL BE USEFUL ONLY DURING
C       PREPROCESSING

      INTEGER   IC1,IC2,STATUS,JBSTAT,UASTAT,SFSTAT,OSSTAT,MRSTAT
      INTEGER   STABLK
      CHARACTER PATHWD*10, FORM*4

      COMMON /BANK4/ PATHWD(6), FORM(3)
      COMMON /BANK5/ IC1(30),IC2(30),STATUS(6,29),JBSTAT,UASTAT,
     1               SFSTAT,OSSTAT,MRSTAT,STABLK(6)

C       PATHWD          DATA PROCESSING PATHWAY NAMES (JB,UA,SF,OS,MR,MP)

C       FORM            DEFINES THE TYPE OF INPUT DEVICE (DISK, TAPE ETC)

C       IC1             DEFINES POSSIBLE BEGINNING POSITIONS OF 
C                       ENTRIES ON USER INPUT SETUP DATA IMAGES
C       IC2             DEFINES POSSIBLE ENDING POSITIONS OF ENTRIES
C                       ON USER INPUT SETUP DATA IMAGES

C       STATUS          KEEPS TRACK OF PROGRESS MADE IN READING AND 
C                       PROCESSING VARIOUS POSIBLE KEYWORDS THAT MIGHT
C                       BE ENCOUNTERED ALONG EACH DATA PATHWAY.
C                       The number of elements in the array equals
C                       (# of paths) * (# of keywords (NKEYWD=29)).

C                         0 KEYWORD NOT SEEN
C                         1 KEYWORD SEEN, ERRORS DETECTED
C                         2 KEYWORD SEEN, NO ERRORS DETECTED
C                         3 KEYWORK SEEN, NO ERRORS DETECTED (may not be used)

C       STABK           Same as STATUS, but for path block cards
C
C       JBSTAT          STATUS WORDS THAT ARE SET BASED ON FINAL
C       UASTAT          ASSESSMENT OF USER DEFINED SETUP DATA.
C       SFSTAT
C       OSSTAT
C                        -1  ERROR CONDITION SET FOR THIS PATH
C                         0  NULL PATHWAY
C                         1  EXTRACT DATA ONLY
C                         2  QA DATA ONLY
C                         3  EXTRACT AND QA, BUT DO NOT MERGE
C                         4  MERGE ONLY
C                         5  EXTRACT AND MERGE, BUT DO NOT QA
C                         6  QA AND MERGE ONLY
C                         7  EXTRACT, QA AND MERGE


C       MRSTAT          MERGE STATUS

C                         0  NO MERGE (INITIAL STATE)
C                         2  MERGE SUBROUTINE CALLED
C                          
