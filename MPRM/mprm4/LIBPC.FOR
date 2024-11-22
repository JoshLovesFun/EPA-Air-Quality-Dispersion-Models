C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE CHCONV                                  
C
C  PURPOSE
C     THIS IS A DUMMY SUBROUTINE TO WORK WITH THE UPPER AIR AND SURFACE
C     EXTRACT PROGRAMS ON IBM-COMPATIBLE PERSONAL COMPUTERS.  The
C     purpose of the IBM mainframe version (in LIBIBM.FOR) is to convert
C     characters from EBCDIC to ASCII.  This conversion is not necessary
C     on a PC.
C
C  CALLED BY: UAEXT, SFEXT
C
C  VERSION DATE: 15 AUGUST 1987
C
C*****************  VARIABLE DESCRIPTIONS  ***********************
C  BUF      CHARACTER ARRAY TO BE TRANSLATED
C  I        LOOP COUNTER FOR NUMBER OF CHARACTERS IN BUF
C  L        NUMBER OF CHARACTERS IN BUF
C
C*****************  VARIABLE DECLARATIONS  ***********************
C
      SUBROUTINE CHCONV
      INTEGER I, L
      CHARACTER BUF*(*)
C
C*****************  BEGIN SUBROUTINE EXECUTION  ******************
C
C- ASCII TO EBCDIC
      ENTRY ASCEBC(L,BUF)
      RETURN
C
C- EBCDIC TO ASCII
      ENTRY EBCASC (L,BUF)
      RETURN
C
      END

      SUBROUTINE DATER ( DCALL, TCALL )
C=====================================================================**
C          DATER Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To return the system date and time.  This routine is for
C               IBM or compatible personal computers using the Lahey 
C               Fortran 90 compiler
C
C     Called by: BANNER
C
C     Input:
C        <none>
C
C     Output:
C        DCALL    = 18-byte string indicating the system date
C        TCALL    =  8-byte string indicating the system time
C
C     Initial Release: December 15, 1992   PES
C
C     Revision History:
C        Revised December 15, 1999 to output four digit year (Desmond Bailey)
C
C
C-----------------------------------------------------------------------


      CHARACTER*9  MONTH(12), CPTMON
      CHARACTER    DCALL*18, TCALL*8
      CHARACTER    CDATE*8, CTIME*10, CZONE*5

      INTEGER  :: IDATETIME(8)
      INTEGER  :: IPTYR, IPTMON, IPTDAY, IPTHR, IPTMIN, IPTSEC


      DATA MONTH/'  JANUARY', ' FEBRUARY', '    MARCH', '    APRIL',
     &           '      MAY', '     JUNE', '     JULY', '   AUGUST',
     &           'SEPTEMBER', '  OCTOBER', ' NOVEMBER', ' DECEMBER' /

      DCALL = ' '
      TCALL = ' '

C     Call date and time routine
      CALL DATE_AND_TIME (CDATE, CTIME, CZONE, IDATETIME)

      IPTYR  = IDATETIME(1)
      IPTMON = IDATETIME(2)
      IPTDAY = IDATETIME(3)
      CPTMON = MONTH(IPTMON)

      IPTHR  = IDATETIME(5)
      IPTMIN = IDATETIME(6)
      IPTSEC = IDATETIME(7)

C     Write Date and Time to Character Variables, DCALL & TCALL
      WRITE(DCALL,1000) CPTMON, IPTDAY, IPTYR
      WRITE(TCALL, '(2(I2.2,":"),I2.2)' ) IPTHR, IPTMIN, IPTSEC

 1000 FORMAT(A9, I3, ',',  I5)

      RETURN
      END



C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE EHANDL ( IERNO )
C
C  PURPOSE:      TO PROVIDE A DUMMY ROUTINE, CORRESPONDING TO THE ONE
C                FOR THE IBM MAINFRAME, FOR THE MPRM PROCESSOR TO CALL
C                (THE IBM VERSION OVERRIDES DEFAULT VALUES IN THE ERROR
C                 OPTION TABLE)
C
C  ARGUMENTS:    IERNO    = IBM ERROR NUMBER
C
C  VERSION DATE: 12 JULY 1988
C
C  CALLED BY:    ATAPE, MPPROC(MP2XFOR), MPMET(MP3XFOR), RDMERG 
C
C  CALLS TO:     -NONE-
C
C=======================================================================
      SUBROUTINE EHANDL (IERNO)
C
      INTEGER IERNO
C
      RETURN
      END
