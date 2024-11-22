       subroutine GETANS(IN,CR,GONE)
*
*  Version for the IBM PC and compatibles
*
*  Modified 19-August 1987 by Robert G. M. Hammarstrand,
*  Sonoma Technology Inc. (STI).
*
*  This subroutine checks to see if a carriage return or "GOTO"
*  has been entered and sets the value of CR and GONE accordingly.
*
       CHARACTER*1 IN(80)
       LOGICAL*1 CR,GONE
*
*     initialise
*
       DO 10 I = 1,80
       IN(I) = ' '
  10   CONTINUE
       CR = .TRUE.  
       GONE = .FALSE.
*      GET IT
       READ(5,100) (in(i), i = 1, 80)
 100   FORMAT(80A1)
*
*  If all 80 characters are blank, it is assumed a carriage return was
*  entered and CR is left true.  Any non-blank character will cause
*  the flag CR to be set false.
*
       DO 600 I=1,80
        IF(IN(I) .NE.' ') CR = .FALSE.
 600   CONTINUE
       IF (  CR  ) RETURN    
*
*  Check to see if the first four characters are "GOTO" 
*  and if so, set the flag GONE true.
*
       IF((IN(1).EQ.'G').AND.(IN(2).EQ.'O').AND.(IN(3).EQ.'T')
     1    .AND.(IN(4).EQ.'O')) GONE = .TRUE.
       IF((IN(1).EQ.'g').AND.(IN(2).EQ.'o').AND.(IN(3).EQ.'t')
     1    .AND.(IN(4).EQ.'o')) GONE = .TRUE.
       RETURN
       END
