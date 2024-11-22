C     Include File:  BLOCK1.INC

C  The variables in the PARAMETER statement are used in data statements
        INTEGER MULTI,SFH4
        PARAMETER (MULTI=OSML*OSMVEC, SFH4=SFHLY*4)
C
        DATA PCNTRL /0/
        DATA PGSCHM /0,0,0,0,0,0,0/
        DATA WSCLSS /3,6,10,16,21/
        DATA ITEM/'WIND','TEMP','MHGT','STAB','TGRD',
     1            'TURB','HFLX','USTR','HGTS'/
        DATA ACTION/'NWSWXX','ONSITE','SESITE','SASITE','WNDWXX',
     1              'TTDIFF','USERIN','ACTUAL','PIXMIX'/                DTBMAY93
C
        DATA CONTRL/1,1,1,1,1,1,1,1,1/
        DATA MDSTAT/5/
        DATA WSNUM/3*0/
        DATA WDNUM/2*0/
        DATA PGNUM/2*0/
        DATA ZINUM/28*0/
        DATA TTNUM/2*0/
        DATA ZIFREQ/98*0.0/
        DATA FREQ/1296*0.0/
        DATA ANEHGT/10.0/
        DATA TMPHGT/2.0/
        DATA ISCVAR/1/
        DATA DISPMD/'BLP     ','RAM     ','MPTER   ','CRSTER  ',
     1              'COMPLEX1','SHORTZ  ','CALINE-3','RTDM    ',
     1              'VALLEY  ','LONGZ   ','ISCLT   ','CDM16   ',
     1              'CDM36   ','ISCST   ','ISCSTDRY','ISCSTWET',
     1              'ISCGASD ','ISCGASW '/                              jopMAY96
C
C     Version date: December 15, 1999
      DATA IVDATE/99349/

      DATA DEVIN/5/,DEVIO/6/,DEV40/40/,DEV50/50/,DEV60/60/,DEV70/70/
      DATA DEV80/80/

      DATA NUMKEY/6,13,13,18,4,9/
      DATA (KEYWRD(1,I),I=1,5)  /'STA','FIN','OUT','RUN','ERR'/
      DATA (KEYWRD(1,I),I=6,10) /'END','   ','   ','   ','   '/
      DATA (KEYWRD(1,I),I=11,15)/'   ','   ','   ','   ','   '/
      DATA (KEYWRD(1,I),I=16,20)/'   ','   ','   ','   ','   '/
      DATA (KEYWRD(2,I),I=1,5)  /'STA','FIN','EXT','IQA','OQA'/
      DATA (KEYWRD(2,I),I=6,10) /'LOC','TOP','IN1','IN2','OFF'/
      DATA (KEYWRD(2,I),I=11,15)/'CHK','AUD','TRA','   ','   '/
      DATA (KEYWRD(2,I),I=16,20)/'   ','   ','   ','   ','   '/
      DATA (KEYWRD(3,I),I=1,5)  /'STA','FIN','EXT','IQA','OQA'/
      DATA (KEYWRD(3,I),I=6,10) /'LOC','IN2','CHK','AUD','TRA'/
      DATA (KEYWRD(3,I),I=11,15)/'IN1','IN3','   ','   ','   '/
      DATA (KEYWRD(3,I),I=16,20)/'   ','   ','   ','   ','   '/
      DATA (KEYWRD(4,I),I=1,5)  /'STA','FIN','EXT','IQA','OQA'/
      DATA (KEYWRD(4,I),I=6,10) /'LOC','AVG','DT1','DT2','DT3'/
      DATA (KEYWRD(4,I),I=11,15)/'HGT','CLM','CHK','MAP','FMT'/
      DATA (KEYWRD(4,I),I=16,20)/'SFC','AUD','TRA','   ','   '/
      DATA (KEYWRD(5,I),I=1,5)  /'STA','FIN','EXT','OUT','   '/
      DATA (KEYWRD(5,I),I=6,10) /'   ','   ','   ','   ','   '/
      DATA (KEYWRD(5,I),I=11,15)/'   ','   ','   ','   ','   '/
      DATA (KEYWRD(5,I),I=16,20)/'   ','   ','   ','   ','   '/
      DATA (KEYWRD(6,I),I=1,5)  /'STA','FIN','MET','MMP','EXT'/
      DATA (KEYWRD(6,I),I=6,10) /'VBL','CHK','TRA','LST','   '/
      DATA (KEYWRD(6,I),I=11,15)/'   ','   ','   ','   ','   '/
      DATA (KEYWRD(6,I),I=16,20)/'   ','   ','   ','   ','   '/
C
      DATA (PATHWD(J),J=1,6) /'JB','UA','SF','OS','MR','MP'/
      DATA (FORM(J),J=1,3)    /'TAPE','DISK','USER'/
C
      DATA DEV10/10/,DEV11/11/,DEV12/12/,DEV13/13/
      DATA UATOP/5000/, UAINC/500/
      DATA UALOC1/'        '/,ZILOC1/'        '/
      DATA UALOC/'        '/
      DATA (UAQA(1,M),M=1,4) /1,-9999,5000,10999/
      DATA (UAQA(2,M),M=1,4) /2,-9999,0,5000/
      DATA (UAQA(3,M),M=1,4) /1,-9999,-350,+350/
      DATA (UAQA(4,M),M=1,4) /1,-9999,-350,+350/
      DATA (UAQA(5,M),M=1,4) /2,-9999,0,360/
      DATA (UAQA(6,M),M=1,4) /1,-9999,0,500/
      DATA (UAQA(7,M),M=1,4) /2,-9999,0,5/
      DATA (UAQA(8,M),M=1,4) /2,-9999,0,90/
      DATA (UAQA(9,M),M=1,4) /2,-9999,-2,5/
      DATA (UAQA(10,M),M=1,4) /2,-9999,0,2/
      DATA (UAQA(11,M),M=1,4) /2, -9999, 50, 2500/                      DTBAUG94
      DATA (UAQA(12,M),M=1,4) /2, -9999, 50, 4500/                      DTBAUG94

C UABSIZ  Number of characters to skip at the beginning
C         of each data block on a *magnetic tape*.
C         UABSIZ = 4 if the tapes are TD-5600 format and
C                    pre-date TD-6200 format
C                = 0 if tapes are TD-5600 format and ordered
C                    after the introduction of TD-6200 format

      DATA UABSIZ/0/

C  Initialize the path/keyword STATUS and path status (STABLK) arrays
      DATA STATUS/174*0/, STABLK/6*0/
C
C  Zero UA-pathway audit arrays
      DATA (UAVAUD(M),M=1,10) /10*0/
      DATA (((UAAUD2(II,J,M),II=1,10),J=1,10),M=0,3) /400*0/
      DATA ((UAAUD1(II,M),II=1,2),M=0,3) /8*0/

C  Set UA-pathway default audit selections
      DATA UASAUD(1) /1/, UASAUD(2) /1/

C  Zero UA-pathway trace arrays
      DATA (UAVTRA(M),M=1,10) /10*0/
      DATA UASTRA(1) /0/, UASTRA(2) /0/
C
      DATA (UAVAR(M),M=1,4)  /'UAPR','UAHT','UATT','UATD'/
      DATA (UAVAR(M),M=5,8)  /'UAWD','UAWS','UASS','UADS'/
      DATA (UAVAR(M),M=9,12) /'UALR','UADD','UAM1','UAM2'/
C
      DATA (VNAMES(K),K=1,4)  /'HFLX','USTR','MHGT','ZOHT'/
      DATA (VNAMES(K),K=5,8)  /'SAMT','PAMT','INSO','NRAD'/
      DATA (VNAMES(K),K=9,12) /'DT01','DT02','DT03','US01'/
      DATA (VNAMES(K),K=13,16)/'US02','US03','HT  ','SA  '/
      DATA (VNAMES(K),K=17,20)/'SE  ','SV  ','SW  ','SU  '/
      DATA (VNAMES(K),K=21,24)/'TT  ','WD  ','WS  ','VV  '/
      DATA (VNAMES(K),K=25,28)/'DP  ','RH  ','V1  ','V2  '/
      DATA (VNAMES(K),K=29,32)/'V3  ','ALTP','SLVP','PRES'/
      DATA (VNAMES(K),K=33,36)/'CLHT','TSKC','C2C3','CLC1'/
      DATA (VNAMES(K),K=37,40)/'CLC2','CLC3','CLC4','CLT1'/
      DATA (VNAMES(K),K=41,44)/'CLT2','CLT3','CLT4','PWTH'/
      DATA (VNAMES(K),K=45,48)/'HZVS','TMPD','TMPW','DPTP'/
      DATA (VNAMES(K),K=49,52)/'RHUM','WD16','WIND','PRCP'/
      DATA (VNAMES(K),K=53,56)/'SF01','SF02','SF03','OSDY'/
      DATA (VNAMES(K),K=57,60)/'OSMO','OSYR','OSHR','OSMN'/
C
      DATA DEV20/20/, DEV21/21/, DEV22/22/, DEV25/25/
      DATA SFLOC1/'        '/, SFLOC/'        '/, PPTLOC/'        '/

      DATA (SFQA(1,L),L=1,4)  /1,999,-50,800/
      DATA (SFQA(2,L),L=1,4)  /1,999,0,2/
      DATA (SFQA(3,L),L=1,4)  /1,9999,0,4000/
      DATA (SFQA(4,L),L=1,4)  /1,999,0,2/
      DATA (SFQA(5,L),L=1,4)  /2,999,0,250/
      DATA (SFQA(6,L),L=1,4)  /2,999,0,100/
      DATA (SFQA(7,L),L=1,4)  /1,9999,0,1250/
      DATA (SFQA(8,L),L=1,4)  /1,999,-50,800/
      DATA (SFQA(9,L),L=1,4)  /1, -999, -2, 5/                          DTBAUG94
      DATA (SFQA(10,L),L=1,4) /1, -999, -2, 5/                          DTBAUG94
      DATA (SFQA(11,L),L=1,4) /1, -999, -2, 5/                          DTBAUG94
      DATA (SFQA(12,L),L=1,4) /1,999,0,100/
      DATA (SFQA(13,L),L=1,4) /1,999,0,100/
      DATA (SFQA(14,L),L=1,4) /1,999,0,100/
      DATA (SFQA(15,L),L=1,4) /1,9999,0,4000/
      DATA (SFQA(16,L),L=1,4) /1,99,0,35/
      DATA (SFQA(17,L),L=1,4) /1,99,0,25/
      DATA (SFQA(18,L),L=1,4) /1,99,0,3/
      DATA (SFQA(19,L),L=1,4) /1,99,0,3/
      DATA (SFQA(20,L),L=1,4) /1,99,0,3/
      DATA (SFQA(21,L),L=1,4) /1,99,-30,35/
      DATA (SFQA(22,L),L=1,4) /2,999,0,360/
      DATA (SFQA(23,L),L=1,4) /1,999,0,50/
      DATA (SFQA(24,L),L=1,4) /1,999,0,5/
      DATA (SFQA(25,L),L=1,4) /1,99,-65,35/
      DATA (SFQA(26,L),L=1,4) /2,999,0,100/
      DATA (SFQA(27,L),L=1,4) /1,999,0,100/
      DATA (SFQA(28,L),L=1,4) /1,999,0,100/
      DATA (SFQA(29,L),L=1,4) /1,999,0,100/
      DATA (SFQA(30,L),L=1,4) /2,-9999,2700,3200/
      DATA (SFQA(31,L),L=1,4) /1,-9999,9000,10999/
      DATA (SFQA(32,L),L=1,4) /1,-9999,9000,10999/
      DATA (SFQA(33,L),L=1,4) /2,-9999,0,300/
      DATA (SFQA(34,L),L=1,4) /2,9999,0,1010/

      DATA (OSTSKY(L),L=1,4)  /2,99,0,10/

      DATA (SFQA(35,L),L=1,4) /2,9999,0,1010/
      DATA (SFQA(36,L),L=1,4) /2,9999,0,910/
      DATA (SFQA(37,L),L=1,4) /2,9999,0,910/
      DATA (SFQA(38,L),L=1,4) /2,9999,0,910/
      DATA (SFQA(39,L),L=1,4) /2,9999,0,910/
      DATA (SFQA(40,L),L=1,4) /2,99999,0,98300/
      DATA (SFQA(41,L),L=1,4) /2,99999,0,98300/
      DATA (SFQA(42,L),L=1,4) /2,99999,0,98300/
      DATA (SFQA(43,L),L=1,4) /2,99999,0,98300/
      DATA (SFQA(44,L),L=1,4) /2,9999,0,9292/
      DATA (SFQA(45,L),L=1,4) /2,-9999,0,1640/
      DATA (SFQA(46,L),L=1,4) /1,-9999,-300,350/
      DATA (SFQA(47,L),L=1,4) /1,-9999,-650,350/
      DATA (SFQA(48,L),L=1,4) /1,-9999,-650,350/
      DATA (SFQA(49,L),L=1,4) /2,-9999,0,100/
      DATA (SFQA(50,L),L=1,4) /2,-9999,0,36/
      DATA (SFQA(51,L),L=1,4) /2, -9999, 0, 500/                        DTBAUG94
      DATA (SFQA(52,L),L=1,4) /2,-9,0,25400/                            JOPOCT95

C  The next three are not used at this time; they are present to
C  allow expanding the number of variables more easily
      DATA (SFQA(53,L),L=1,4) /1,999,0,100/
      DATA (SFQA(54,L),L=1,4) /1,999,0,100/
      DATA (SFQA(55,L),L=1,4) /1,999,0,100/

C  ONSITE day, month, year, hour and minute
      DATA (SFQA(56,L),L=1,4) /2,-9,1,31/
      DATA (SFQA(57,L),L=1,4) /2,-9,1,12/
      DATA (SFQA(58,L),L=1,4) /2,-9,0,99/
      DATA (SFQA(59,L),L=1,4) /2,-9,0,24/
      DATA (SFQA(60,L),L=1,4) /2,-9,0,60/

C  Set default SF-pathway audit selections
C  (Default variables: SLP, STN. P, CEILING, SKY COVER,
C                        HORIZ. VIS., DRY BULB T.,
C                        WIND DIRECTION, WIND SPEED)

      DATA (SFSAUD(L),L=1,11) /0,1,1,1,1,0,0,0,0,0,0/
      DATA (SFSAUD(L),L=12,SFHLY) /0,0,0,0,1,1,0,0,0,1,1,0/

      DATA (SFSTRA(L),L=1,11) /0,0,0,0,0,0,0,0,0,0,0/
      DATA (SFSTRA(L),L=12,SFHLY) /0,0,0,0,0,0,0,0,0,0,0,0/

      DATA ((SFAUD(II,L),II=1,SFHLY),L=0,3) /SFH4*0/
      DATA ((SFAUD1(II,L),II=1,11),L=0,3) /44*0/
      DATA ((SFAUD2(II,L),II=1,11),L=0,3) /44*0/
C
      DATA OSCALM/1.0/
C
      DATA DEV31/31/,DEV32/32/
      DATA OSLOC/'       '/
      DATA OSAVG/1/,OSMIN/1/
      DATA OSDAY1/-9/,OSDAY2/-9/
      DATA OSNWDS/1/

C  Onsite default roughness length(3), bowen ratio(2), albedo(1)
C     for the first sector (by default, 0-360 degrees)
C     OSSFC(frequency,sector,parameter)
      DATA (OSSFC(II,1,1),II=1,12) /12*0.25/
      DATA (OSSFC(II,1,2),II=1,12) /12*0.75/
      DATA (OSSFC(II,1,3),II=1,12) /12*0.15/

C  Onsite default values for parameters required by ISCST3
C     roughness length at the application (source) site (4),
C     minimum Obukhov length for a stable atmosphere (5),
C     fraction of the net radiation absorbed by ground (6),
C     anthropogenic heat flux (7), and leaf area index (8)
      DATA (OSSFC(II,1,4),II=1,12) /12*0.15/
      DATA (OSSFC(II,1,5),II=1,12) /12*2.00/
      DATA (OSSFC(II,1,6),II=1,12) /12*0.15/
      DATA (OSSFC(II,1,7),II=1,12) /12*0.00/
      DATA (OSSFC(II,1,8),II=1,12) /12*3.00/                            JOP 7/22/96

C  Zero the remaining sectors
      DATA ((OSSFC(II,N,1),II=1,12),N=2,12) /132*0.0/
      DATA ((OSSFC(II,N,2),II=1,12),N=2,12) /132*0.0/
      DATA ((OSSFC(II,N,3),II=1,12),N=2,12) /132*0.0/
      DATA ((OSSFC(II,N,4),II=1,12),N=2,12) /132*0.0/
      DATA ((OSSFC(II,N,5),II=1,12),N=2,12) /132*0.0/
      DATA ((OSSFC(II,N,6),II=1,12),N=2,12) /132*0.0/
      DATA ((OSSFC(II,N,7),II=1,12),N=2,12) /132*0.0/
      DATA ((OSSFC(II,N,8),II=1,12),N=2,12) /132*0.0/                   JOP 7/22/96

      DATA OSWDS(1,1) /0.0/
      DATA OSWDS(1,2) /360.0/
      DATA ((OSWDS(II,N),II=2,12),N=1,2) /22*0.0/

C  Zero OS-pathway audit arrays
      DATA ((OSVAUD(L,N),L=1,OSML),N=1,OSMVEC) /MULTI*0/
      DATA (OSAUD1(II,0),II=1,OSMV) /OSMV*0/
      DATA (OSAUD1(II,1),II=1,OSMV) /OSMV*0/
      DATA (OSAUD1(II,2),II=1,OSMV) /OSMV*0/
      DATA (OSAUD1(II,3),II=1,OSMV) /OSMV*0/
      DATA ((OSAUD2(L,II,0),II=1,OSMVEC),L=1,OSML) /MULTI*0/
      DATA ((OSAUD2(L,II,1),II=1,OSMVEC),L=1,OSML) /MULTI*0/
      DATA ((OSAUD2(L,II,2),II=1,OSMVEC),L=1,OSML) /MULTI*0/
      DATA ((OSAUD2(L,II,3),II=1,OSMVEC),L=1,OSML) /MULTI*0/

C  Set OS-pathway default audit selections
C      DATA (OSSAUD(N),N=1,OSMV) /OSMV*0/  
C     SCALARS: MHGT
C     VECTORS: SA  ,SE  ,TT  ,WD  ,WS
C
      DATA OSSAUD(1) /0/
      DATA OSSAUD(2) /0/
      DATA OSSAUD(3) /1/
      DATA (OSSAUD(N),N=4,15) /12*0/
      DATA OSSAUD(16) /1/
      DATA OSSAUD(17) /1/
      DATA OSSAUD(18) /0/
      DATA OSSAUD(19) /0/
      DATA OSSAUD(20) /0/
      DATA OSSAUD(21) /1/
      DATA OSSAUD(22) /1/
      DATA OSSAUD(23) /1/
      DATA (OSSAUD(N),N=24,OSMV) /37*0/

C  Zero OS-pathway trace array
      DATA (OSSTRA(N),N=1,OSMV) /OSMV*0/

C  Initialize date variables for the merge

      DATA MRYR1/0/,  MRYR2/0/,
     &     MRGMO1/0/, MRGDY1/0/,
     &     MRGMO2/0/, MRGDY2/0/
C
C *** The following data statement controls whether to backspace on
C     a file when processing header records on the output file.
C     Current usage is in subroutine FLHEAD
C     For VAX      set BACK40  to .FALSE.
C     For IBM-PC   set BACK40  to .TRUE.
C     For IBM-3090 set BACK40  to .TRUE.
C
      DATA BACK40 /.TRUE./

      DATA RAD2DG /57.29578/
