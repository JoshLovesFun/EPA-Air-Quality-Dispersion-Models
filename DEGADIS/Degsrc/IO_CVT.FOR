C..........................................................
C
C	ROUTINE to convert old INP files to the new format
C
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	include 'DEG1.prm'
c
	dimension PTIME(igen), ET(igen), R1T(igen), PWC(igen)
	dimension PTEMP(igen),PFRACV(igen)
	dimension DEN(5,igen)
C
	character*80 TITLE(4), string
	character*24 TINP
	character*3 gas_name
C
	logical check1,check2,again,check3,check4,check5
C
	character*40 OPNRUP
	character*1 crml(2)

	data crml/'D','N'/

c
  1	continue
	write(6,*) 'Enter an old INP file name.'
	read(5,'(a40)') opnrup
	nchar = index(opnrup,' ') - 1
	opnrup = opnrup(1:nchar) // '.inp'
C
	OPEN(UNIT=9, file=OPNRUP, status='OLD')
C
	DO 90 I=1,4
	READ(9,2000) TITLE(I)
   90	CONTINUE
 2000	FORMAT(A80)
C
	READ(9,*) U0,Z0,zr
	read(9,*) istab
	read(9,*) oodist, avtime
	READ(9,*) DELTAy,BETAy,rml
	frml = rml
	read(9,*) sigx_coeff,sigx_pow,sigx_min_dist
	read(9,*) tamb,pamb,humid, relhum
	humsrc = 0.
	read(9,*) isofl,tsurf
	read(9,*) ihtfl,htco
	read(9,*) iwtfl,wtco
	read(9,2020) gas_name
 2020	FORMAT(A3)
	read(9,*) gas_mw,gas_temp,gas_rhoe
	read(9,*) gas_cpk,gas_cpp
	read(9,*) gas_ufl,gas_lfl,gas_zsp
C
	if(isofl .eq. 0) goto 105
	READ(9,*) Nden
	DO 100 I=1,Nden
  100	READ(9,*) DEN(1,I),DEN(2,I),den(3,I),den(4,i),den(5,i)
C
  105	READ(9,*) ycLOW
C
	read(9,*) gmass0
	READ(9,*) Nsrc
	DO 110 I=1,Nsrc
	READ(9,*) PTIME(I),ET(I),R1T(I), PWC(I),PTEMP(I),PFRACV(I)
 110	CONTINUE
C
	read(9,*) check1,check2,again,check3,check4,check5
c
	READ(9,2010) TINP
 2010	format(a24)
C
	if(check4) read(9,*) ess,slen,swid
c
	CLOSE(UNIT=9)

c
c... Get the default values for RML
c
	call atmdef(zr, istab, avtime, drml, deltay, betay, 
     .		deltaz, betaz, gammaz, sigxco, sigxp, sigxmd)
c
c... Use the default Monin-Obukhov length or set it?
c
c--------------------------------------------------------------------------- rml

  460	continue
		write(6,3461) frml
 3461		format(//,' The file showed the Monin-Obukhov ',
     .			'length to be: ',1pg13.5,' m.')
	    write(6,3463) drml
 3463	    format(/,' The default Monin-Obukhov ',
     .			'length is: ',1pg13.5,' m.')
	indvel = 1
	if(rml .ne. drml) indvel = 2

	write(6,3466) crml(indvel)
 3466	format(/,' Do you wish to: (D) use the default ',
     .			'Monin-Obukhov length; or (N) not?',//,
     .			' [D,N,/,?] <',a1,'> ',$)
	read(5,3102) string
 3102	format(a80)
	nchar = index(string,' ')
	if(nchar.eq.1) string = crml(indvel)

	IF(STRING.eq.'N' .or. string.eq.'n') then
	    indvel = 2
  490	    iq = iquery(
     .		'Enter the Monin-Obukhov length (m); 0. for infinity',
     .		rml,rml)
	    if(iq .lt. 0) then
		goto 460
	    else if(iq .gt. 0) then
		write(6,6490)
 6490		format(/,' The Monin-Obukhov length is a length scale ',
     .				'which quantifies the vertical',/,
     .			' stability in the atmosphere; it is a ',
     .				'parameter used in the windspeed',/,
     .			' profile.  Unstable atmospheric stabilities ',
     .				'(A, B, and C) correspond to',/,
     .			' negative Monin-Obukhov lengths, while ',
     .				'stable atmospheric stabilities',/,
     .			' (E and F) correspond to positive lengths.  ',
     .				'Neutral atmospheric stability',/,
     .			' (D) corresponds to an infinite Monin-Obukhov',
     .				' length scale (which is',/,
     .			' represented in DEGADIS with a value of ',
     .				'zero input to the Monin-Obukhov',/,
     .			' length).',//,
     .			' If the Monin-Obukhov length scale is ',
     .				'unknown, DEGADIS estimates it from',/,
     .			' the atmospheric stability and the ',
     .				'surface roughness.')
		goto 490
	    endif
	    goto 530
	else if(string.eq.'D' .or. string.eq.'d') then
	    indvel = 1
	    rml = 0.D0
	    goto 530
	else if(string.eq.'?') then
	    write(6,6490)
	    goto 460
	else
	    goto 460
	endif
 530	continue

c
c... prepare the new file...............................................
c
	OPEN(UNIT=8, file=OPNRUP, status='OLD')
	do 1005 iii=1,4
 1005	WRITE(8,'(a80)') TITLE(iii)

	WRITE(8,*) U0,Z0,ZR
	WRITE(8,*) istab
	oodist = 0.D0
	write(8,*) oodist,avtime
	WRITE(8,*) indvel,rml
	write(8,1030) tamb,pamb,humid,relhum
	write(8,*) isofl,tsurf
	write(8,*) ihtfl,htco
	write(8,*) iwtfl,wtco
	write(8,'(a3)') gas_name
	WRITE(8,*) gas_mw,  gas_temp, gas_rhoe
	write(8,*) gas_cpk, gas_cpp
	WRITE(8,*) gas_ufl, gas_lfl, gas_zsp
	if(isofl.eq.1) then
	    write(8,*) nden
	    do 1010 i=1,nden
 1010	    WRITE(8,1030) DEN(1,I),DEN(2,I),DEN(3,I),Den(4,I),den(5,i)
	endif
	WRITE(8,*) ycLOW
	write(8,*) gmass0
	WRITE(8,*) Nsrc
	DO 1020 I=1,Nsrc
	WRITE(8,1030) PTIME(I),ET(I),R1T(I), PWC(I),PTEMP(I),PFRACV(I)
 1020	continue
	write(8,*) check4
	WRITE(8,'(a24)') TINP

	CLOSE(UNIT=8)
C
 1030	format(1x,5(1pg14.7,1x),1pg14.7)
c
c
	goto 1
	END
