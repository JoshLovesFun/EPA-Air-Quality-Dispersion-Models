C  PROGRAM TO CONVERT AN ME FILE TO BINARY FORM WITH NO HEADER RECORD
      CHARACTER*60 FILE

      INTEGER*4 N,L
      REAL*4 X,Y

      PRINT *, 'NAME OF ASCII .ME FILE: '
      READ(*,1) FILE
1     FORMAT(A60)
      OPEN(1,FILE=FILE,FORM='FORMATTED',STATUS='OLD')

      PRINT *, 'NAME OF NEW BINARY .ME FILE: '
      READ(*,1) FILE
      OPEN(2,FILE=FILE,FORM='UNFORMATTED',STATUS='NEW',
     X  ACCESS='DIRECT',RECL=8)
      IREC=0

      READ(1,1) FILE
      PRINT *, FILE

100   READ(1,*) N,L
      IF(N.EQ.0) GO TO 1000
      M=IABS(N)
      PRINT *, M,L
      IREC=IREC+1
      WRITE(2,REC=IREC) N,L
      DO 200 J=1,M
      READ(1,*) X,Y
      IREC=IREC+1
      WRITE(2,REC=IREC) X,Y
200   CONTINUE
      GO TO 100

1000  IREC=IREC+1
      WRITE(2,REC=IREC) N,L
      STOP
      END
