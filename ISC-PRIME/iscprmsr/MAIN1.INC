C***********************************************************************
C     MAIN1.INC
C     ISCST2 Model Data - Array Names, Array Limits, Named Common Blocks
C                         Global Data for All Modules
C
C     MODIFIED - 12/15/93  For New Settling Routines, and Gridded Terrain
C     MODIFIED - 11/8/93   For Wet Deposition Routines
C     MODIFIED - 7/7/93    For Area Sources
C     MODIFIED - 2/15/93   For Revised Deposition Routines
C     MODIFIED - 7/19/94   For OPENPIT Algorithms (JAH/PES)
C     MODIFIED - 7/27/94   Removed ZDM, displacement height, from METVAR
C     MODIFIED - 7/27/94   Added Variable for 10m Reference Wind Speed
C     MODIFIED - 4/17/95   Output CONC/DEPOS/DDEP/WDEP in same model run
C***********************************************************************

c ----------------------------------------------------------------------
c --- ISC-PRIME Version 1.0
c
c     ISCST3 modified to accept additional building data required by
c     the PRIME building downwash modules
c
c     Changes are denoted in comment fields
c
c     Prepared by    Earth Tech, Inc
c     Prepared for   EPRI under contract WO3527-01
c ----------------------------------------------------------------------

C***********************************************************************
C     User Specified Model Parameters for Array Dimensions
C***********************************************************************

C     The following commented lines provide the array limits for the
C     Microsoft executable file.
CMICRO      PARAMETER (NSRC= 100, NREC= 500, NGRP= 2, NAVE= 2, NVAL= 2,
CMICRO     &           NTYP= 2,
CMICRO     &           NMAX= 50, NSEC= 36, NQF= 24, NPDMAX= 10, NWSCAT= 6,
      PARAMETER (NSRC= 300, NREC= 1200, NGRP= 6, NAVE= 4, NVAL= 6,
     &           NTYP= 4,
     &           NMAX= 50, NSEC= 36, NQF= 96, NPDMAX= 20, NWSCAT= 6,
     &           NKST= 6, IXM=100, IYM=100, NNET= 5, NHR= 1, NYR = 1,
     &           NPAIR= 100, NWET=2)

C**   NSRC   = Max Number of Sources
C**   NREC   = Max Number of Receptors
C**   NGRP   = Max Number of Source Groups
C**   NAVE   = Max Number of Short Term Averaging Periods
C**   NVAL   = Max Number of High Values by Receptor (RECTABLE Keyword);
C**                Also Controls Number of Highest Annual Averages Printed
C**   NTYP   = Max Number of Output Types per Run (CONC, DEPOS, DDEP and WDEP)
C**   NMAX   = Max Number of Overall Maximum Values (MAXTABLE Keyword)
C**   NSEC   = Number of Sectors for Building Dimensions
C**   NQF    = Number of Variable Emission Rate Factors Per Source
C**   NPDMAX = Max Number of Particle Diameter Categories Per Source
C**   NWSCAT = Number of Wind Speed Categories
C**   NKST   = Number of Stability Categories
C**   IXM    = Max Number of X-coord (Distance) Values Per Receptor Network
C**   IYM    = Max Number of Y-coord (Direction) Values Per Receptor Network
C**   NNET   = Max Number of Cartesian and/or Polar Receptor Networks
C**   NHR    = Number of Hours in Met Data Loop (Not Used in Current Design)
C**   NYR    = Max Number of Years in Single Run (Not Used in Current Design)
C**   NPAIR  = Number of Pairs of TXCONC and IDCONC for TOXXFILE Output
C**   NWET   = Number of phases of precipitation (2 = liquid,frozen)


c ----------------------------------------------------------------------
c     Additional parameters for header of a binary concentration file
c     in BLP format
c ----------------------------------------------------------------------
      parameter (mxpnts= 300, mxlines= 20 )
      parameter (mxpntp1=mxpnts+1, mxlinp1=mxlines+1)

c**   MXPNTS = Max Number of point sources allowed for BLP format Conc file
c**   MXLINES= Max Number of Line sources (NOT used by ISC -- only necessary
c**            for the header record of binary BLP format Conc file)
c ----------------------------------------------------------------------

C***********************************************************************
C     Model Constants Specified as Parameters
C***********************************************************************

      PARAMETER (PI = 3.141593, TWOPI = 6.283185, RTOFPI = 1.772454,
     &           SRT2PI = 2.506628, BETA = 0.6,  G = 9.80616,
     &           DTORAD = 0.017453293,  RTODEG = 57.29578, VK = 0.4,
     &           RTPIBY2 = 1.2533141)

C**   PI     = PI
C**   TWOPI  = 2.*PI
C**   RTOFPI = SQRT(PI)
C**   SRT2PI = SQRT(2.*PI)
C**   BETA   = Entrainment Coefficient for BLP Rise
C**   G      = Acceleration Due to Gravity (m/s**2)
C**   DTORAD = Degrees to Radians Conversion Factor
C**   RTODEG = Radians to Degrees Conversion Factor
C**   VK     = von-Karman Constant
C**   RTPIBY2= SQRT(PI/2.)


C***********************************************************************
C     Programmer Specified Model Parameters
C***********************************************************************

c --- PRIME Modification --------------------------------------
c prm PARAMETER (IFMAX=40, IPN=7, IKN=58, ISTRG=132, IERRN=123)
      PARAMETER (IFMAX=40, IPN=7, IKN=61, ISTRG=132, IERRN=132)
c -------------------------------------------------------------

C**   IFMAX  = Max Number of Fields Per Runstream Record
C**   IPN    = Number of Pathway IDs (Includes '**')
C**   IKN    = Number of Keywords
C**   ISTRG  = Length of Runstream Image Record
C**   IERRN  = Number of Error/Warning/Informational Messages


C***********************************************************************
C     Common Block for Input/Output File Units (Initialized in BLOCK DATA)
C***********************************************************************

      COMMON /FUNITS/ INUNIT, IOUNIT, MFUNIT, IERUNT, IERWRT,
     &                IDPUNT, IDPUN2, IRSUNT, IEVUNT, ITEVUT,
     &                IZUNIT

C*----   ISCSTM Modification: allow for hourly emissions - jah 11/3/94
     &               ,IHREMI
C*#


C**   INUNIT = Input Runstream File Unit (Initialized to 5)
C**   IOUNIT = Main Printed Output File Unit (Initialized to 6)
C**   MFUNIT = Input Meteorological Data File Unit (Initialized to 19)
C**   IERUNT = Temporary Error/Message File Unit (Initialized to 10)
C**   IERWRT = Permanent Detailed Error/Message File Unit (Init. to 11)
C**   IDPUNT = Main SAVEFILE Unit for Re-start Option (Init. to 12)
C**   IDPUN2 = Secondary SAVEFILE Unit for Re-start Option (Init. to 14)
C**   IRSUNT = INITFILE Unit for Re-start Option (Initialized to 15)
C**   IEVUNT = Event File Unit for Use With ISC2EV Model (Init. to 17)
C**   ITEVUT = Temporary Event File Used to Store High Value Events for
C**            Summary Tables and for EVENTFIL Option (Initialized to 18)
C**   IZUNIT = Gridded Terrain File Used (Initialized to 13)
C**   IHREMI = Hourly Emission Parameters File Unit (Init. to 13)

C***********************************************************************
C     This is The Global Variable Definition Block for Runstream Data
C***********************************************************************

      LOGICAL BLINE, INFLD, MARK, ECHO

      CHARACTER PATH*2, PPATH*2, KEYWRD*8, PKEYWD*8, KEYWD*8, KTYPE*5,
     &          FIELD*40, RUNST*1, RUNST1*132, INPFIL*40, OUTFIL*40

      COMMON /FIELDS/ LOCB(IFMAX), LOCE(IFMAX), IFC, IDC1, IPNUM, IPPNUM
      COMMON /FLDCHR/ FIELD(IFMAX)
      COMMON /LOGIN1/ BLINE, INFLD, MARK, ECHO
      COMMON /SETCHR/ PATH, KEYWRD, PPATH, PKEYWD, KTYPE, KEYWD(IKN)
      COMMON /RUNSTR/ RUNST(ISTRG), RUNST1
      COMMON /FILCHR/ INPFIL, OUTFIL


C***********************************************************************
C     This is The Global Variable Definition Block for Error Handling
C***********************************************************************

      LOGICAL FATAL, ISTART, IFINIS, RECERR, ERRLST, EOF

      CHARACTER ERRMSG*50, ERRCOD*3, MSGFIL*40, VERSN*5

      COMMON /ERRLOG/ FATAL, ISTART, IFINIS, RECERR, ERRLST, EOF
      COMMON /ERRCHR/ ERRMSG(IERRN), ERRCOD(IERRN), MSGFIL, VERSN
      COMMON /ERRQA1/ ILINE, IERROR, IFTL, IWRN, INFO,
     &                ICLM, IMSG, NFATAL, IPAGE
      COMMON /ERRQA2/ EXPLIM

c --- PRIME Modification --------------------------------------------
c prm COMMON /INSTAT/ ICSTAT(20), ISSTAT(20), IRSTAT(20), IMSTAT(20),
      COMMON /INSTAT/ ICSTAT(20), ISSTAT(23), IRSTAT(20), IMSTAT(20),
     &                IOSTAT(20), ITSTAT(20)
c -------------------------------------------------------------------

      COMMON /REERR1/ INCSET, IXYSET, IEVSET, IFGSET


C***********************************************************************
C     This is The Global Variable Definition Block for COntrol Pathway
C***********************************************************************

      LOGICAL DFAULT, CONC, DEPOS, DDEP, WDEP, RURAL, URBAN,
     &        GRDRIS, NOSTD, NOBID, NOCALM, CLMPRO, MSGPRO,
     &        PERIOD, ANNUAL, MONTH, FLAT, ELEV, FLGPOL,
     &        RUN, EVENTS, RSTSAV, RSTINP, DAYTAB, MXFILE, PPFILE,
     &        PLFILE, ANPOST, ANPLOT, STATOK, DEBUG, FSTREC, MULTYR,
     &        TXFILE, DDPLETE, WDPLETE, NOSMPL, NOCMPL, FSTCMP

C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94
     &       ,NOCHKD
C*#

      CHARACTER ELTYPE*6, TITLE1*68, TITLE2*68, SAVFIL*40, SAVFL2*40,
     &          INIFIL*40, EVFILE*40, EVPARM*6, CHRAVE*5, CHIDEP*4,
     &          MODOPS*6,
     &          SOELEV*6, REELEV*6, TGELEV*6

      COMMON /CNTINT/ KAVE(NAVE), NHIVAL, NMXVAL, NDUMP
      COMMON /CNTLOG/ DFAULT, CONC, DEPOS, DDEP, WDEP, RURAL, URBAN,
     &                GRDRIS, NOSTD, NOBID, NOCALM, CLMPRO,
     &                MSGPRO, PERIOD, ANNUAL, MONTH,
     &                FLAT, ELEV, FLGPOL, RUN, EVENTS, RSTSAV, RSTINP,
     &                DAYTAB, MXFILE, PPFILE, PLFILE, ANPOST, ANPLOT,
     &                STATOK, DEBUG, FSTREC, MULTYR,
     &                TXFILE, DDPLETE, WDPLETE, NOSMPL, NOCMPL, FSTCMP

C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94
     &                ,NOCHKD
C*#

      COMMON /CNTCHR/ SAVFIL, SAVFL2, INIFIL, EVFILE, TITLE1, TITLE2,
     &                ELTYPE, EVPARM, CHRAVE(NAVE), CHIDEP(6,NTYP),
     &                MODOPS(17),
     &                SOELEV, REELEV, TGELEV



C***********************************************************************
C     This is The Global Variable Definition Block for SOurce Pathway
C***********************************************************************

      CHARACTER SRCID*8, SRCTYP*8, SOPCRD*1, GRPID*8, EMILBL*40,
     &          OUTLBL*40, POLLUT*8, PSOID*8, QFLAG*6

C*----   ISCSTM Modification: allow for hourly emissions - jah 11/3/94
     &         ,HRFILE*40
C*#

      LOGICAL LDPART,LWPART,LWGAS

      COMMON /SOURC1/ AXS(NSRC), AYS(NSRC), AZS(NSRC), AQS(NSRC),
     &                AHS(NSRC), ATS(NSRC), AVS(NSRC), ADS(NSRC),
     &                ASYINI(NSRC), ASZINI(NSRC)

c --- PRIME Modification ----------------------------------
      COMMON /SOURC2/ ADSBH(NSEC,NSRC), ADSBW(NSEC,NSRC),
c prm&                IDSWAK(NSEC,NSRC)
     &                IDSWAK(NSEC,NSRC), adsbl(nsec,nsrc),
     &                adsxadj(nsec,nsrc),adsyadj(nsec,nsrc)
c ---------------------------------------------------------

      COMMON /SOURC3/ QFACT(NQF,NSRC)
      COMMON /SOURC4/ EMIFAC(NTYP), EMICON, HAFLIF, DECOEF, INPD(NSRC),
     &                LDPART, LWPART, LWGAS,
     &                APDIAM(NPDMAX,NSRC), APHI(NPDMAX,NSRC),
     &                APDENS(NPDMAX,NSRC), ASC(NPDMAX,NSRC),
     &                AVGRAV(NPDMAX,NSRC), ATSTOP(NPDMAX,NSRC),
     &                APSLIQ(NPDMAX,NSRC), APSICE(NPDMAX,NSRC),
     &                AGSCAV(NWET,NSRC),
     &                VAIRMS, ZRDEP, VDPHOR

C*----   ISCSTM Modification: allow for hourly emissions - jah 11/3/94
      COMMON /SOURC5/ HRQS, HRTS, HRVS, KURHRQ
C*----
C*#

      COMMON /SORGRP/ IGROUP(NSRC,NGRP)
      COMMON /SORCHR/ SRCID(NSRC), SRCTYP(NSRC), SOPCRD(NSRC),
     &                GRPID(NGRP), QFLAG(NSRC), EMILBL(NTYP),
     &                OUTLBL(NTYP), POLLUT, PSOID

C*----   ISCSTM Modification: allow for hourly emissions - jah 11/3/94
     &               ,HRFILE
C**   KURHRQ  Date/Time Stamp for Hourly Emissions Data
C**   HRFILE  Hourly Emissions Data File Name
C*#

C***********************************************************************
C     This is The Global Variable Definition Block for the New Area
C     Source Algorithm - 7/7/93
C
C*    Added XVERT,YVERT - Jayant Hardikar, PES, 7/20/94
C***********************************************************************

      PARAMETER (K1 = 5, JMAX1 =10, ITMAX =100, EPS = 1.0E-4,
     &           EPS2 = 1.0E-10, NVMAX = 6, NVMAX2 = 12)

C**   K1    = Order of Extrapolating Polynomial
C**   JMAX1 = Maximum Number of Iterations in Halving Interval
C**   ITMAX = Maximum Number of Integral Iterations
C**   EPS   = Tolerance Limit for Convergence of the Integral
C**   EPS2  = Lower Threshold Limit for the Value of the Integral
C**   NVMAX = Maximum Number of Vertices in a Single Area Source + 2
C**   NVMAX2= NVMAX * 2

      LOGICAL LSEG

      COMMON /VERTEX/ IVERT, NVERT, UVERT(NVMAX), VVERT(NVMAX),
     &                VNVERT(NVMAX), WVERT(NVMAX)
      COMMON /SEGTBL/ NSEGS, UASEGS(NVMAX), UBSEGS(NVMAX)
      COMMON /BSIDE1/ LSEG
      COMMON /BSIDE2/ SPA(NVMAX,2)
      COMMON /SRAREA/ AXINIT(NSRC), AYINIT(NSRC), AANGLE(NSRC),
     &                AXVERT(NVMAX,NSRC), AYVERT(NVMAX,NSRC),
     &                XVERT(NVMAX), YVERT(NVMAX)


C***********************************************************************
C     This is The Global Variable Definition Block for the New OPENPIT
C     Source Algorithm - 7/19/94
C***********************************************************************

      PARAMETER (ALPHA = 0.029)
      REAL AALPHA(NSRC), APDEFF(NSRC), AVOLUM(NSRC)
      REAL EFRAC(NPDMAX), QPART(NPDMAX)
      REAL PALPHA, THETA, PDEFF, PDREL, PITFRA, QEFF
      REAL PITLEN, PITWID, PITL, EMIHGT


C*    ALPHA     = Proportionality Constant for determining Escape Fraction
C*    AALPHA    = Array of Angles of Long Pit Dimension w.r.t.
C*                North for Each Source
C*    PALPHA    = Angle of Long Pit Dimension w.r.t. North for
C*                the Current Source
C*    THETA     = Wind Direction angle w.r.t Long Axis
C*                of the Pit
C*    APDEFF    = Array of Effective Pit Depths for Each Source
C*    PDEFF     = Effective Pit Depths for Current Source
C*    PDREL     = Relative Pit Depth
C*    AVOLUM    = Array of Volume of the OPENPIT Sources
C*    EFRAC     = Array of Escape Fractions
C*    QPART     = Array of Adjusted Emission Rates
C*    PITFRA    = Fractional Size of the Effective Pit Area
C*    PITLEN    = Length of the Pit
C*    PITWID    = Width of the Pit
C*    PITL      = Along-Wind Length of the Pit
C*    EMIHGT    = Height of Emissions Above Base of Pit

      COMMON /PITSRC/ AALPHA, APDEFF, AVOLUM, EFRAC, QPART,
     &        PALPHA, THETA, PDEFF, PDREL, PITFRA, QEFF,
     &        PITLEN, PITWID, PITL, EMIHGT



C***********************************************************************
C     This is The Global Variable Definition Block for REceptor Pathway
C***********************************************************************

      LOGICAL ISTA, IEND, NEWID

      CHARACTER NETID*8, NETIDT*8, PNETID*8, NTID*8, NTTYP*8,
     &          RECTYP*2, PXSOID*8, PESOID*8

      COMMON /RECEPT/ AXR(NREC), AYR(NREC), AZELEV(NREC),
     &                AZFLAG(NREC), IREF(NREC)
      COMMON /RECLOG/ ISTA, IEND, NEWID
      COMMON /RECCHR/ NETID(NREC), RECTYP(NREC), NTID(NNET),
     &                NTTYP(NNET), PXSOID, PESOID, NETIDT, PNETID
      COMMON /COORD1/ ICOUNT, JCOUNT, IZE, IZF, IRZE, IRZF, IRXR, IRYR,
     &                IBND, IBELEV, INNET, XINT, YINT
      COMMON /COORD2/ XCOORD(IXM,NNET), YCOORD(IYM,NNET),
     &                NETSTA(NNET), NETEND(NNET),
     &                NUMXPT(NNET), NUMYPT(NNET),
     &                XORIG(NNET), YORIG(NNET)


C***********************************************************************
C     This is The Global Variable Definition Block for MEteorology Pathway
C***********************************************************************

      CHARACTER METFRM*60, METINP*40, SFNAME*40, UANAME*40

      COMMON /METEOR/ ISDATE, IEDATE, ISYR, ISMN, ISDY, ISHR, IEYR,
     &                IEMN, IEDY, IEHR, IPROC(366), PUSER(NKST,NWSCAT),
     &                DTUSER(NKST,NWSCAT), ISYEAR, IUYEAR, IDSURF,
     &                IDUAIR, ZREF, ROTANG, ISJDAY, IEJDAY, PURB(NKST),
     &                PRUR(NKST), DTURB(NKST), DTRUR(NKST), NDAYS,
     &                UCAT(5), INCRST, UMIN
      COMMON /METCHR/ METFRM, METINP, SFNAME, UANAME


C***********************************************************************
C     This is The Global Variable Definition Block for METEXT
C***********************************************************************

      LOGICAL CLMHR, MSGHR, UNSTAB, NEUTRL, STABLE,
     &        RUNERR, NEWDAY, ENDMON, USERP, USERDT

      COMMON /METLOG/ CLMHR, MSGHR, UNSTAB, NEUTRL, STABLE,
     &                RUNERR, NEWDAY, ENDMON, USERP, USERDT
      COMMON /METMSG/ KSTMSG
      COMMON /METDAY/ IHOUR, IYEAR, IMONTH, IDAY, KURDAT, JDAY,
     &                IPHOUR, IPDATE, IPYEAR(NYR)
      COMMON /METVAR/ AFV, UREF, TA, KST, ZI, ZIRUR, ZIURB,
     &                AFVLST, AFV24, S, RTOFS,
     &                USTAR, EL, Z0M, IPCODE, PRATE, UREF10


C***********************************************************************
C     This is The Global Variable Definition Block for Terrain Grid Pathway
C***********************************************************************

      CHARACTER TERINP*40
      LOGICAL LTGRID

      COMMON /TG/ TGX0, TGY0, GRDXLL, GRDXUR, GRDYLL, GRDYUR, XYINT,
     &            LTGRID
      COMMON /TGCHR/  TERINP


C***********************************************************************
C     This is The Global Variable Definition Block for Calculation
C***********************************************************************

      LOGICAL CALCS, WAKE, WAKESS, WAKLOW, BUOYNT, TALL, SQUAT, SSQUAT

      COMMON /INDEXS/ IREC,   ISRC,   IGRP,   IAVE,   ITYP,  ISET,
     &                NUMREC, NUMSRC, NUMGRP, NUMAVE, NUMTYP,
     &                NUMYR, ICYEAR

c --- PRIME Modification -------------------------------------------
      COMMON /CALCS1/ XS, YS, ZS, QS, HS, DS, VS, TS, SYINIT,
     &                SZINIT, XINIT, YINIT, ANGLE, DSBH, DSBW,
     &                dsbl, xadj, yadj,
     &                IFVSEC, D, V(NTYP), VD, E, WDRAD, WDSIN, WDCOS
c ------------------------------------------------------------------

      COMMON /CALCS2/ CALCS, WAKE, WAKESS, WAKLOW, BUOYNT, TALL,
     &                SQUAT, SSQUAT
      COMMON /CALCS3/ NPD, PDIAM(NPDMAX), PHI(NPDMAX), PDENS(NPDMAX),
     &                SC(NPDMAX), VGRAV(NPDMAX), TSTOP(NPDMAX),
     &                VDEP(NPDMAX), WQCORG, WQCORGC,
     &                WQCOR(NPDMAX), WQCORC(NPDMAX),
     &                SZCOR(NPDMAX), SZCORC(NPDMAX), SZMIN(NPDMAX),
     &                DQCOR(NPDMAX), PCORZR(NPDMAX), PCORZD(NPDMAX),
     &                DQCORC(NPDMAX), PCORZRC(NPDMAX), PCORZDC(NPDMAX),
     &                PSCAV(NPDMAX,NWET), GSCAV(NWET),
     &                PSCVRT(NPDMAX), GSCVRT
      COMMON /CALCS4/ XR, YR, X, Y, ZELEV, ZFLAG, DISTR
      COMMON /CALCS5/ HE, HSP, HEFLAT, HTER, HEMWAK, ZB, ZM, IUCAT,
     &                XY, XZ, SBID, FM, FB, DTDZ, DHF, DHP, DELT,
     &                DHPB, DHPM, XF, XFM, XFB, XRAD
      COMMON /CALCS6/ US, SY, SZ, DA, ZLY, ZLB, X3LB, RINIT,
     &                CB, CM, QTK, P


C***********************************************************************
C     This is The Global Variable Definition Block for Complex Terrain
C***********************************************************************

      LOGICAL SIMPLE, INTERM, COMPLX

      COMMON /CMPLOG/ SIMPLE, INTERM, COMPLX
      COMMON /CMPDAT/ TCF(NKST), ZMIN, HECMP1, HECOMP, HESETL,
     &                CORR, DELTHP, XZCMP1, SZCMP1, SBCMP1,
     &                DHPCMP, DHFCMP,
     &                SIMCON, COMCON


C***********************************************************************
C     This is The Global Variable Definition Block for OUtput Pathway
C***********************************************************************

      CHARACTER THRFIL*40, PSTFIL*40, PLTFIL*40, ANNPST*40, ANNPLT*40,
     &          THRFRM*60, PSTFRM*60, PLTFRM*60, TOXFIL*40

      COMMON /OUTPT1/ NHIAVE(NVAL,NAVE), MAXAVE(NAVE), IMXVAL(NAVE),
     &                IDYTAB(NAVE), MAXFLE(NGRP,NAVE),THRESH(NGRP,NAVE),
     &                IPSTFL(NGRP,NAVE), IPLTFL(NVAL,NGRP,NAVE),
     &                IANPST(NGRP), IANPLT(NGRP), INHI(NAVE),
     &                ITOXFL(NAVE), TOXTHR(NAVE)
      COMMON /OUTPT2/ IMXUNT(NGRP,NAVE), IPSUNT(NGRP,NAVE),
     &                IPSFRM(NGRP,NAVE), IPLUNT(NVAL,NGRP,NAVE),
     &                IAPUNT(NGRP), IANFRM(NGRP), IPPUNT(NGRP),
     &                ITXUNT(NAVE)
      COMMON /OUTCHR/ THRFIL(NGRP,NAVE), PSTFIL(NGRP,NAVE),
     &                PLTFIL(NVAL,NGRP,NAVE), ANNPST(NGRP),
     &                ANNPLT(NGRP), THRFRM, PSTFRM, PLTFRM,
     &                TOXFIL(NAVE)
      COMMON /TOXWRK/ IDCONC(NAVE,NPAIR), TXCONC(NAVE,NPAIR),
     &                ITAB, NXTOX, NYTOX, NHOURS, IPAIR


c --- BLP Output, with file compression (EMI   2/15/96) ---------
      logical lcompr
      common/ blp1/ iss,lcompr,ubar,wd2,ist,hm,day,ihr,wd4
c ---------------------------------------------------------------

C***********************************************************************
C     This is The Global Variable Definition Block for Working Space
C***********************************************************************

      CHARACTER WORKID*8, DUMMY*8

      COMMON /WRKCHR/ WORKID(NSRC), DUMMY

c --- PRIME Modification ----------------------
c prm COMMON /SRCWRK/ IWRK2(NSRC,9)
      COMMON /SRCWRK/ IWRK2(NSRC,13)
c ---------------------------------------------


      SAVE
