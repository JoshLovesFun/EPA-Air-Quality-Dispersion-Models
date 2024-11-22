	SUBROUTINE HEAD(gmass)
c
c.................................................................
c
c	Subroutine to output the DEGADIS simulation heading
c
c.................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG1.prm'
C
	common
     ./ALP/	ALPHA,alpha1
     ./alpcom/	ialpfl,alpco
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csprd/	ce, delrmn
     ./ERROR/	STPIN,ERBND,WTRG,WTtm,WTya,wtyc,wteb,wtmb,XLI,
     .		XRI,EPS,ZLOW,STPINZ,ERBNDZ,SRCOER,srcss,srccut,
     .		ERNOBL,NOBLpt,crfger,epsilon
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./GEN2/	DEN(5,igen)
     ./GEN3/	radg(2,maxl),qstr(2,maxl),srcden(2,maxl),srcwc(2,maxl),
     .		srcwa(2,maxl),srcen(2,maxl)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./TITL/	TITLE
c
	character*3 gasnam
	character*80 TITLE(4)
	character*24 TSRC,TINP,TOBS,TSRT
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
c
	character*1 stabil(6)
c
	data stabil/'A','B','C','D','E','F'/
	data iparm/0/
C
c
	if(iparm .eq. 1) goto 190
	WRITE(8,1100)
 1100	FORMAT(1H0,'*******************',9X,'U O A _ D E G A D I S ',
     .		2X,'M O D E L ',2X,'O U T P U T ',2X,'- - ',2X,
     .		'V E R S I O N ',2X,'2.1',9X,'*******************')
C
	WRITE(8,1111)
c
	WRITE(8,1102) tsrc
 1102	FORMAT(1H ,'***************',23X,'***************',1X,
     .		a24,1X,'***************',23X,'***************')
C
	WRITE(8,1111)
C
	WRITE(8,1112) TINP
	WRITE(8,1114) TSRC
	IF(tOBS(1:2).NE.'  ' .and. .not.check4) WRITE(8,1116) TOBS
	IF(tOBS(1:2).NE.'  ' .and. check4) WRITE(8,1117) TOBS
	IF(tSRT(1:2) .NE. '  ') WRITE(8,1118) TSRT
 1112	FORMAT(1H ,'Data input on',22X,a24)
 1114	FORMAT(1H ,'Source program run on',14X,a24)
 1116	FORMAT(1H ,'Pseudo Steady-State program run on ',a24)
 1117	FORMAT(1H ,'Steady-State program run on',7x,a24)
 1118	FORMAT(1H ,'Time sort program run on',11X,a24)
	WRITE(8,1111)
C
	WRITE(8,1110)
	WRITE(8,1111)
C
 1110	FORMAT(1H0,10X,'TITLE BLOCK')
 1111	FORMAT(1H )
C
	DO 100 I = 1,4
	WRITE(8,1120) TITLE(I)
  100	CONTINUE
C
 1120	FORMAT(1H ,A80)
C
	WRITE(8,1111)
	WRITE(8,1130) U0
	WRITE(8,1140) Z0
	WRITE(8,1150) ZR
	write(8,1155) stabil(istab)
	if(rml.ne. 0.) then
	    WRITE(8,1160) rml
	else
	    write(8,1161)
	endif
	write(8,1168) avtime
	WRITE(8,1170) DELTAy
	WRITE(8,1180) BETAy
	WRITE(8,1190) ALPHA
	WRITE(8,1192) USTAR
	WRITE(8,1194) tamb
	if(isofl.eq.0 .and. ihtfl.ne.0) write(8,1195) tsurf
	WRITE(8,1196) pamb
	WRITE(8,1198) humid
	vaporp = watvp(tamb)		! atm
	relhumid = 100.D0* humid/(wmw/wma*vaporp / (pamb- vaporp))
	write(8,1199) relhumid
C
 1130	FORMAT(1H ,5X,'Wind velocity at reference height ',20X,F6.2,2X,
     .		'm/s')
 1140	FORMAT(1H ,5X,'Reference height ',37X,F6.2,2X,'m')
 1150	FORMAT(1H0,5X,'Surface roughness length ',25X,1PG10.3,2X,'m')
 1155	FORMAT(1H0,5X,'Pasquill Stability class ',25X,4x,a1)
 1160	FORMAT(1H0,5X,'Monin-Obukhov length ',29X,1PG10.3,2X,'m')
 1161	FORMAT(1H0,5X,'Monin-Obukhov length ',31X,'infinite')
 1168	FORMAT(1H ,5X,'Gaussian distribution constants ',/,
     .		1H ,5x,8x,9x,'Specified averaging time',10X,F9.2,2X,'s')
 1170	FORMAT(1H ,5X,32x,3X,'Deltay',10X,F9.5)
 1180	FORMAT(1H ,5X,32X,3X,' Betay',10X,F9.5)
 1190	FORMAT(1H0,5X,'Wind velocity power law constant',4X,'Alpha',
     .		10X,F9.5)
 1192	FORMAT(1H ,5X,'Friction velocity',15X,4X,5X,10X,F9.5,2X,'m/s')
 1194	FORMAT(1H0,5X,'Ambient Temperature ',34X,F6.2,2X,'K')
 1195	FORMAT(1H0,5X,'Surface Temperature ',34X,F6.2,2X,'K')
 1196	FORMAT(1H ,5X,'Ambient Pressure ',37X,F6.3,2X,'atm')
 1198	FORMAT(1H ,5X,'Ambient Absolute Humidity',25X,1PG10.3,2X,
     .		'kg/kg BDA')
 1199	FORMAT(1H ,5X,'Ambient Relative Humidity',25X,4X,F6.2,2X,'%')
C
	WRITE(8,1111)
C
	if(isofl .eq. 0) goto 135
	WRITE(8,1200)
	WRITE(8,1205)
	ii = -1
	DO 130 I = 1,igen
	IF(DEN(1,I).gt. 1.) goTO 148
	ii = ii+1
	if(ii.eq. 3) then
	    write(8,1211)
	    ii = 0
	endif
  130	WRITE(8,1210) DEN(1,I),DEN(2,I),den(3,i)
	goto 148
  135	write(8,1207)
	write(8,1208)
	ii = -1
	DO 138 I = 1,igen
	IF(DEN(1,I).gt. 1.) goTO 148
	ii = ii+1
	if(ii.eq. 3) then
	    write(8,1211)
	    ii = 0
	endif
  138	WRITE(8,1212) DEN(1,I),DEN(2,I),den(3,i),den(4,i),den(5,i)
  148	continue
C
 1200	FORMAT(1H ,5X,'Input:    ',6X,3x,'Mole fraction',4x,
     .		'CONCENTRATION OF C',6X,'GAS DENSITY')
 1205	FORMAT(1H ,14X,20x,2(13X,'kg/m**3'))
 1207	FORMAT(1H ,5X,'Adiabatic Mixing:',3x,'Mole fraction',3x,
     .		'CONCENTRATION OF C',6X,'GAS DENSITY',5x,
     .		6x,'Enthalpy',6x,4x,'Temperature')
 1208	FORMAT(1H ,14X,20x,2(13X,'kg/m**3'),7x,8x,'J/kg',8x,9x,'K')
 1210	FORMAT(1H ,14X,3(12X,F8.5))
 1211	format(1H )
 1212	FORMAT(1H ,14X,3(12X,F8.5),6x,3x,1pg13.5,7x,1pg13.5)
C
	WRITE(8,1111)
	write(8,1233) gasmw,gastem,gasrho
	if(gascpp .ne. 1.D0) then
	    write(8,1234) gascpk,gascpp
	else
	    cp = (3.33D4 + gascpk)/gasmw
	    write(8,1235) cp
	endif
	write(8,1236) gasulc,gasllc,gaszzc
	WRITE(8,1111)
	WRITE(8,1220) gmass
	WRITE(8,1230)
C
	DO 150 I=1,IGEN
	IF(PTIME(I).EQ.POUND) GO TO 160
  150	WRITE(8,1231) PTIME(I),ET(I),R1T(I), PWC(I),PTEMP(I),
     .			PENTH(I)
  160	CONTINUE
C
 1220	FORMAT(1H ,'Source input data points',//,
     .		1h ,15x,'Initial (pure contaminant) mass in cloud: ',
     .		1pg13.5,' kg',//,
     .		1h ,7X,'Time',10X,'Contaminant',
     .		7X,'Source Radius',2x,3x,'Contaminant',4x,
     .		3x,'Temperature',4x,5x,'Enthalpy',5x,/,
     .		1x,18x,4x,'Mass Rate',5x,18x,2x,'Mass Fraction',3x)
 1230	FORMAT(1H ,9X,'s',15X,'kg/s',16X,'m',8x,' kg contam/kg mix ',8x,
     .		'K',9x,7x,'J/kg',7x)
 1231	FORMAT(1H ,6(3X,1PG12.5,3X))
 1233	Format(1H0,5x,'Specified Gas Properties:',//,
     .		10x,'Molecular weight:',T65,1pg13.5,/,
     .		10x,'Release temperature:',T65,1pg13.5,'K',/,
     .		10x,'Density at release temperature and ambient ',
     .		'pressure:',T65,1pg13.5,'kg/m**3')
 1234	Format(	10x,'Mean heat capacity constant:',T65,1pg13.5,/,
     .		10x,'Mean heat capacity power:',T65,1pg13.5)
 1235	Format(	10x,'Average heat capacity:',T65,1pg13.5,'J/kg K')
 1236	Format(	10x,'Upper mole fraction contour:',T65,1pg13.5,/,
     .		10x,'Lower mole fraction contour:',T65,1pg13.5,/,
     .		10x,'Height for isopleths:',T65,1pg13.5,'m',/)
 1241	format(1h0,5x,'Calculation procedure for ALPHA: ',I2)
 1242	format(1h0,5x,'Entrainment prescription for PHI: ',I2)
 1244	format(1h0,5x,'Layer thickness ratio used for average depth: ',
     .		1pg13.5)
 1250	format(1h0,5x,'Air entrainment coefficient used: ',f5.3)
 1251	format(1h0,5x,'NON Isothermal calculation')
 1252	format(1h0,5x,'Gravity slumping velocity coefficient used: ',f5.3)
 1253	format(1h0,5x,'Heat transfer calculated with fixed coefficient: ',
     .		1pg13.5,' J/m**2/s/K')
 1254	format(1h0,5x,'Heat transfer not included')
 1255	format(1h0,5x,'Heat transfer calculated with correlation: ',I2)
 1256	format(1h0,5x,'Isothermal calculation')
 1257	format(1h0,5x,'Water transfer calculated with fixed coefficient: '
     .		,1pg13.5,' /m**2/s/atm')
 1258	format(1h0,5x,'Water transfer not included')
 1259	format(1h0,5x,'Water transfer calculated with correlation')
C
	WRITE(8,1111)
	write(8,1241) ialpfl
	write(8,1242) iphifl
	write(8,1244) dellay
	write(8,1250) epsilon
	write(8,1252) ce
	if(isofl.eq. 0) write(8,1251)
	if(isofl.ne. 0) write(8,1256)
	if(ihtfl.lt. 0) write(8,1253) htco
	if(ihtfl.eq. 0) write(8,1254)
	if(ihtfl.gt. 0) write(8,1255) ihtfl
	if(iwtfl.lt. 0) write(8,1257) wtco
	if(iwtfl.eq. 0) write(8,1258)
	if(iwtfl.gt. 0) write(8,1259)
	WRITE(8,1111)
	iparm =1
	return
c
  190	continue
	if(.not.check4) return
	RAD = SQRT(2.D0*SLEN*SWID/pi)
	WRITE(8,1300) ESS,RAD
	WRITE(8,1320) SLEN,SWID
	qstar = ess/outl/outb/2.D0
	WRITE(8,1340) OUTCc,OUTSZ,qstar
	write(8,1350) swcl,swal,senl,srhl
	WRITE(8,1360) OUTL,OUTB
C
C
C
 1300	FORMAT(1H0,'Source strength [kg/s] : ',18X,1PG13.5,T60,
     .		'Equivalent Primary source radius [m] : ',5x,1PG13.5)
 1320	FORMAT(1H ,'Equivalent Primary source length [m] : ',4X,1PG13.5,
     .	T60,'Equivalent Primary source half-width [m] : ',1X,1PG13.5)
 1340	FORMAT(/,' Secondary source concentration [kg/m**3] : ',
     .	1PG13.5,T60,'Secondary source SZ [m] : ',18X,1PG13.5,//,
     .		' Contaminant flux rate: ',1pg13.5,/)
 1350	format(/,' Secondary source mass fractions... contaminant: ',
     .		1pg13.6,2x,' air: ',1pg13.5,/,' ',10x,' Enthalpy: ',
     .		1pg13.5,5x,' Density: ',1pg13.5,/)
 1360	FORMAT(1H ,'Secondary source length [m] : ',13X,1PG13.5,T60,
     .		'Secondary source half-width [m] : ',10X,1PG13.5)
C
C
	RETURN
	END
