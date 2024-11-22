	SUBROUTINE CRFG(TABLE,NTAB,rer)
c
C..............................................................
C
C	SUBROUTINE TO CREATE RADG,QSTR,srcden,srcwc,srcwa,srcen DATA LISTS
C
C	PARAMETERS:	TABLE - WORKSPACE VECTOR
C			NTAB  - DIMENSION OF TABLE DIVIDED BY iousrc
C			RER   - RELATIVE ERROR BOUND OF CREATED
C				DATA PAIRS BY LINEAR INTERPOLATION
C
C	VALUES OF TIME, RADG, height, QSTR, SZ0, yc, ya, rho, Ri,
c		wc,wa,enthalpy,temp
c	ARE READ INTO TABLE(1) TO TABLE(13), RESPECTIVELY.
c
c	TABLE is used to store intermediate points which were not recorded
c	in the output vectors (RADG et al.).  When a new entry is read into
c	TABLE, each entry in TABLE is checked to see if this new entry
c	would be appropriate as an endpoint for a linear interpolation
c	with the last point stored in the output vectors (RADG et al.).
c	When a new entry in TABLE fails to meet this criteria, the previous
c	entry in TABLE is recorded in the output vectors.  This process
c	guarantees that all points in th integration record will be
c	represented by linear interpolation of the output vectors.
C
C................................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	DIMENSION TABLE(1)
C
	include 'DEG1.prm'
	parameter (zero= 1.D-20)
C
	COMMON
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./GEN3/	radg(2,maxl),qstr(2,maxl),srcden(2,maxl),srcwc(2,maxl),
     .		srcwa(2,maxl),srcen(2,maxl)
     ./NEND/	POUND
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
C
	DATA NI/1/
c
	data iti/1/	! time - element no 1 in record
	data irg/2/	! Radg - element no 2 in record
	data iqs/4/	! Qstar - element no 4 in record
	data idn/8/	! rho - element no 8 in record
	data iwc/10/	! wc - element no 10 in record
	data iwa/11/	! wa - element no 11 in record
	data ien/12/	! enthalpy - element no 12 in record
c
C
C... OUTPUT CREATED VECTORS TO A PRINT FILE; set up headings
C
C
	READ(9,*) (TABLE(J),J=1,iousrc)
C
	WRITE(8,1111)
	WRITE(8,1105)
	if(isofl.eq. 1) then
	    WRITE(8,1102)
	    WRITE(8,1103)
	    WRITE(8,1140) (TABLE(J),J=1,6),table(8),table(9)
	else
	    WRITE(8,1100)
	    WRITE(8,1104)
	    WRITE(8,1140) (TABLE(J),J=1,6),table(8),table(13),table(9)
	endif
	ispace = 1
C
 1100   FORMAT(/,5X,'Time',5X,2x,'Gas Radius',2x,4X,'Height',4X,
     .		4x,'Qstar',5x,2x,'SZ(x=L/2.)',2x,1X,'Mole frac C',2x,
     .		3x,'Density',4x,1x,'Temperature',2x,3x,'Rich No.',3x)
 1102   FORMAT(/,5X,'Time',5X,2x,'Gas Radius',2x,4X,'Height',4X,
     .		4x,'Qstar',5x,2x,'SZ(x=L/2.)',2x,1X,'Mole frac C',2x,
     .		3x,'Density',4x,3x,'Rich No.',3x)
 1103	FORMAT(1H ,4X,'sec',6X,6X,'m',7X,6X,'m',7X,
     .		2X,'kg/m**2/s',3X,6X,'m',7X,14x,3x,'kg/m**3',4x,14x,/)
 1104	FORMAT(1H ,4X,'sec',6X,6X,'m',7X,6X,'m',7X,2X,'kg/m**2/s',3X,
     .		6X,'m',7X,14x,3x,'kg/m**3',4x,6x,'K',7x,14x,/)
 1105	FORMAT(1H ,23X,'*****',21X,'CALCULATED SOURCE PARAMETERS',21X,
     .		'*****')
C
c... start the output vectors with time=0.0 results
c
	RADG(1,1) = 0.D0
	RADG(2,1) = TABLE(2)
	QSTR(1,1) = 0.D0
	QSTR(2,1) = TABLE(4)
	srcden(1,1) = 0.D0
	srcden(2,1) = table(8)
	srcwc(1,1) = 0.D0
	srcwc(2,1) = table(10)
	srcwa(1,1) = 0.D0
	srcwa(2,1) = table(11)
	srcen(1,1) = 0.D0
	srcen(2,1) = table(12)
C
	READ(9,*) (TABLE(J),J=1,iousrc)
	L = 2
C
C... L IS THE NUMBER OF RECORDS WHICH HAVE BEEN READ
C
  100   CONTINUE
	DO 120 I=2,NTAB
C
C... MOVE LAST RECORD READ INTO THE LAST ACTIVE POSITION OF TABLE
C
	DO 130 J = 1,iousrc
	KK = iousrc * (I-1) + J
  130   TABLE(KK) = TABLE(J)
	KK = iousrc * I
C
C... READ THE NEXT RECORD.  INCREMENT L.
C
	L = L + 1
	READ(9,*,END=900) (TABLE(J),J=1,iousrc)
C
c... loop through the records included in TABLE
C
	DO 140 Kkk = 2,I
C
	KT    = iousrc*(Kkk-1) + iti
	KRG   = iousrc*(Kkk-1) + irg
	KQSTR = iousrc*(Kkk-1) + iqs
	KCA   = iousrc*(Kkk-1) + idn
	Kwc   = iousrc*(Kkk-1) + iwc
	Kwa   = iousrc*(Kkk-1) + iwa
	Ken   = iousrc*(Kkk-1) + ien
C
	timeslot = radg(1,ni)
	ratio = (table(kt) - timeslot) / (table(iti) - timeslot)
c
	ANSRG = (TABLE(irg) - RADG(2,NI)) * ratio + RADG(2,NI)
	ANSQ = (TABLE(iqs) - QSTR(2,NI)) * ratio + QSTR(2,NI)
	ANSCA = (TABLE(idn) - srcden(2,NI)) * ratio + srcden(2,NI)
	ANSwC = (TABLE(iwc) - srcwc(2,NI)) * ratio + srcwc(2,NI)
	ANSwa = (TABLE(iwa) - srcwa(2,NI)) * ratio + srcwa(2,NI)
	ANSen = (TABLE(ien) - srcen(2,NI)) * ratio + srcen(2,NI)
C
	ERRG = ABS(ANSRG - TABLE(KRG))/TABLE(KRG)
	ERQSTR = ABS(ANSQ - TABLE(KQSTR))/(TABLE(KQSTR)+zero)
	ER0 = dMAX1(ERRG,ERQSTR)
c
	ERCA = ABS(ANSCA - TABLE(KCA))/TABLE(KCA)
	ER0 = dMAX1(ER0,ERCA)
c
	ERwC = ABS(ANSwC - TABLE(KwC))/(TABLE(KwC)+ zero)
	ER0 = dMAX1(ER0,ERwC)
	ERwa = ABS(ANSwa - TABLE(Kwa))/(TABLE(Kwa)+ zero)
	ER0 = dMAX1(ER0,ERwa)
	ERen = ABS(ANSen - TABLE(Ken))/(TABLE(Ken)+ zero)
	ER0 = dMAX1(ER0,ERen)
C
	IF(ER0 .GT. RER) GO TO 150
  140   CONTINUE
  120   CONTINUE
C
c... The entire TABLE has been filled without the selection of a point for
c	the output vectors.  This is no big problem; it just means that
c	if the TABLE had been bigger, CRFG might have chosen fewer points
c	for the output vectors, but the simulation results should be unchanged.
c
	WRITE(lunlog,1110)
C
C... RECORD NEXT DATA PAIR. SINCE ERROR is now EXCEEDED, RECORD THE LAST
C	DATA PAIR WHICH SATISFIED THE ERROR CRITERIA WHICH IS STORED
C	IN TABLE(KK-(iousrc-1)) TO TABLE(KK)
C
  150   NI = NI + 1
c
	KT    = KK - iousrc + iti
	KRG   = KK - iousrc + irg
	KQSTR = KK - iousrc + iqs
	KCA   = KK - iousrc + idn
	KwC   = KK - iousrc + iwc
	Kwa   = KK - iousrc + iwa
	Ken   = KK - iousrc + ien
c
	IF(NI .GT. MAXL) then
	    write(lunlog,*) ' CRFG? Time out: ',table(kt)
	    CALL trap(5,0)
	endif
C
	RADG(1,NI)   = TABLE(KT)
	RADG(2,NI)   = TABLE(KRG)
	QSTR(1,NI)   = TABLE(KT)
	QSTR(2,NI)   = TABLE(KQSTR)
	srcden(1,NI) = TABLE(KT)
	srcden(2,NI) = TABLE(KCA)
	srcwc(1,NI)  = TABLE(KT)
	srcwc(2,NI)  = TABLE(KWC)
	srcwa(1,NI)  = TABLE(KT)
	srcwa(2,NI)  = TABLE(KWA)
	srcen(1,NI)  = TABLE(KT)
	srcen(2,NI)  = TABLE(KEN)
C
C... WRITE THE POINTS JUST RECORDED TO UNIT=8
C
	if(isofl.eq. 1) then
	    WRITE(8,1140) (TABLE(J),J=kt,kt+5),table(kt+7),table(kt+8)
	else
	    WRITE(8,1140) (TABLE(J),J=kt,kt+5),table(kt+7),
     .			table(kt+12),table(kt+8)
	endif
	ispace = ispace + 1
	if(ispace.eq. 3) then
	    ispace = 0
	    write(8,1111)
	endif
C
	GO TO 100
C
  900	CONTINUE		! EOF encountered
C
	NI   = NI   + 1
	IF(NI+1 .GT. MAXL) then
	    write(lunlog,*) ' CRFG? Time out: ',table(iti)
	    CALL trap(5,0)
	endif
c
	RADG(1,NI)   = TABLE(iti)
	RADG(2,NI)   = TABLE(irg)
	QSTR(1,NI)   = TABLE(iti)
	QSTR(2,NI)   = TABLE(iqs)
	srcden(1,NI) = TABLE(iti)
	srcden(2,NI) = TABLE(idn)
	srcwc(1,NI)  = TABLE(iti)
	srcwc(2,NI)  = TABLE(iwc)
	srcwa(1,NI)  = TABLE(iti)
	srcwa(2,NI)  = TABLE(iwa)
	srcen(1,NI)  = TABLE(iti)
	srcen(2,NI)  = TABLE(ien)
c
	if(isofl.eq. 1) then
	    WRITE(8,1140) (TABLE(J),J=1,6),table(8),table(9)
	else
	    WRITE(8,1140) (TABLE(J),J=1,6),table(8),table(13),table(9)
	endif
	ispace = ispace + 1
	if(ispace.eq. 3) then
	    ispace = 0
	    write(8,1111)
	endif
c
c... put the end-of-listing indicator (POUND) in the output vectors
c
	NI   = NI   + 1
	DO 910 I =1,2
	RADG(I,NI)    = POUND
	QSTR(I,NI)    = POUND
	srcden(I,NI)  = POUND
	srcwc(I,NI)   = POUND
	srcwa(I,NI)   = POUND
	srcen(I,NI)   = POUND
  910	CONTINUE
C
	RETURN
C
 1110	FORMAT(' ?CRFG? TABLE exceeded without point selection ',
     .		'- execution continuing')
 1111	FORMAT(1H )
 1140	FORMAT(1H ,9(1PG13.6,1X))
	END
