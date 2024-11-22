	SUBROUTINE SORTS(TABLE)
c
C........................................................
C
C	TIME SORT SUPERVISOR
C
c........................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	include 'DEG3.prm'
C
	COMMON
     ./CNOBS/	NOBS
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		ycLOW
     ./SORTa/	TCc(maxnob,maxnt),TCcSTR(maxnob,maxnt),
     .		Tyc(maxnob,maxnt),Trho(maxnob,maxnt),
     .		Tgamma(maxnob,maxnt),Ttemp(maxnob,maxnt)
     ./SORTb/	TSY(maxnob,maxnt),TSZ(maxnob,maxnt),TB(maxnob,maxnt),
     .		TDIST0(maxnob,maxnt),TDIST(maxnob,maxnt),KSUB(maxnt)
     ./SORTIN/	TIM(maxnt),NTIM,ISTRT
     ./SSCON/	NREC(maxnob,2),T0(maxnob),XV(maxnob)
C
	real*4 tcc,tccstr,tyc,trho,tgamma,ttemp,tsy,tsz,tb,tdist0

	DIMENSION TABLE(1)
C
c
	CALL GETT
C
C
C... TABLE(I) VALUES...........................................................
c
C        I              PARAMETER
C       --              _________
C       1  11           DIST		1 TO 10 CURRENT READ
c       2  12           Yc
C       3  13           Cc		11 TO 20 PREVIOUS READ
c       4  14           rho
c       5  15           gamma
c       6  16           temp
C       7  17           SZ
C       8  18           SY
C       9  19           B
C       10 20           TS
C
C       21              DIST0
C       22              INTERPOLATION FRACTION
C
c.........................................................................
c
c... outer loop
c
	DO 1000 I = 1,NOBS
C
c... zero the table
c
	DO 100 J=1,20
  100	TABLE(J) = 0.
c
c... read the first record
c
	read(9,*) (table(k1),k1=1,9)
	table(10) = ts( t0(I), table(1) )
	table(21) = table(1)
c
	II = NREC(I,1)
	if( ii .eq. 0) goto 500
c
c... loop through and read each record even if not pertinent
c
	DO 400 J = 2,II
C
c... save the previous record before reading the next record
c
	    DO 200 K1 = 1,10
	    KK = K1 + 10
	    TABLE(KK) = TABLE(K1)
  200	    continue
C
	READ(9,*) (TABLE(K1),K1=1,9)
	TABLE(10) = TS( T0(I), TABLE(1) )
C
	itl = int( (table(10)-tim(1)) / (tim(2)-tim(1)) + 0.9999999 )
	itl = min( ntim, itl)
	itf = int( (table(20)-tim(1)) / (tim(2)-tim(1)) + 0.9999999 ) + 1
	itf = max( 1, itf)

C
C... RECORD AN INTERPOLATED, TIME SORTED POINT.
C
	do 300 it = itf, itl		! do all points in range
	KSUB(IT) = KSUB(IT) + 1
C
	TABLE(22) = (TIM(IT) - TABLE(20))/(TABLE(10) - TABLE(20))
C
	TDIST0(I,IT) = TABLE(21)
	TDIST(I,IT)  = TABLE(11) + (TABLE(1) - TABLE(11)) * TABLE(22)
	Tyc(I,IT)    = TABLE(12) + (TABLE(2) - TABLE(12)) * TABLE(22)
	TCc(I,IT)    = TABLE(13) + (TABLE(3) - TABLE(13)) * TABLE(22)
	Trho(I,IT)   = TABLE(14) + (TABLE(4) - TABLE(14)) * TABLE(22)
	Tgamma(I,IT) = TABLE(15) + (TABLE(5) - TABLE(15)) * TABLE(22)
	Ttemp(I,IT)  = TABLE(16) + (TABLE(6) - TABLE(16)) * TABLE(22)
	TSZ(I,IT)    = TABLE(17) + (TABLE(7) - TABLE(17)) * TABLE(22)
	TSY(I,IT)    = TABLE(18) + (TABLE(8) - TABLE(18)) * TABLE(22)
	TB(I,IT)     = TABLE(19) + (TABLE(9) - TABLE(19)) * TABLE(22)
C
  300	continue
  400   CONTINUE
C
C... Read the entries where b=0
c
  500	continue
	II = NREC(I,2)
	IF(II .EQ. 0) GO TO 1000

	DO 800 J=1,II
C
c... save the previous record before reading the next record
c
	    DO 600 K1 = 1,10
	    KK = K1 + 10
	    TABLE(KK) = TABLE(K1)
  600	    continue
C
c... read the next record
c
	READ(9,*) (TABLE(K1),K1=1,7)
C
	TABLE(8) = RT2*DELTAy*(TABLE(1) + XV(I))**BETAy
	TABLE(9) = 0.D0
	TABLE(10) = TS(T0(I),TABLE(1))
C
	itl = int( (table(10)-tim(1)) / (tim(2)-tim(1)) + 0.9999999 )
	itl = min( ntim, itl)
	itf = int( (table(20)-tim(1)) / (tim(2)-tim(1)) + 0.9999999 ) + 1
	itf = max( 1, itf)
C
C... RECORD A TIME SORTED VALUE
C
	do 700 it = itf, itl		! do all points in range
	KSUB(IT) = KSUB(IT) + 1
	TABLE(22) = (TIM(IT) - TABLE(20))/(TABLE(10) - TABLE(20))
C
	TDIST0(I,IT) = TABLE(21)
	TDIST(I,IT)  = TABLE(11) + (TABLE(1) - TABLE(11)) * TABLE(22)
	Tyc(I,IT)    = TABLE(12) + (TABLE(2) - TABLE(12)) * TABLE(22)
	TCc(I,IT)    = TABLE(13) + (TABLE(3) - TABLE(13)) * TABLE(22)
	Trho(I,IT)   = TABLE(14) + (TABLE(4) - TABLE(14)) * TABLE(22)
	Tgamma(I,IT) = TABLE(15) + (TABLE(5) - TABLE(15)) * TABLE(22)
	Ttemp(I,IT)  = TABLE(16) + (TABLE(6) - TABLE(16)) * TABLE(22)
	TSZ(I,IT)    = TABLE(17) + (TABLE(7) - TABLE(17)) * TABLE(22)
	TSY(I,IT)    = TABLE(18) + (TABLE(8) - TABLE(18)) * TABLE(22)
	TB(I,IT)     = TABLE(19) + (TABLE(9) - TABLE(19)) * TABLE(22)
C
  700	continue
  800	CONTINUE
C
 1000	CONTINUE
C
	CALL SORTS1(TABLE)
C
	RETURN
	END
