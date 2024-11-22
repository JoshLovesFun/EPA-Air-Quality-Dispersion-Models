C***********************************************************************
C     MAIN.INC
C     SCREEN2 Model Data - Array Names, Array Limits, Named Common Blocks
C                          Global Data for All Modules
C***********************************************************************

C***********************************************************************
C     Model Constants Specified as Parameters
C***********************************************************************

      PARAMETER (PI = 3.141593, TWOPI = 6.283185, RTOFPI = 1.772454,
     &           SRT2PI = 2.506628, BETA = 0.6,  G = 9.80616,
     &           DTORAD = 0.017453293,  RTODEG = 57.29578,
     &           EXPLIM = -50.0, CP = 1004., VK = 0.4, NPDMAX = 20)

C**   PI     = PI
C**   TWOPI  = 2.*PI
C**   RTOFPI = SQRT(PI)
C**   SRT2PI = SQRT(2.*PI)
C**   BETA   = Entrainment Coefficient for BLP Rise
C**   G      = Acceleration Due to Gravity (m/s**2)
C**   DTORAD = Degrees to Radians Conversion Factor
C**   RTODEG = Radians to Degrees Conversion Factor
C**   EXPLIM = Lower Limit for EXP Argument
C**   CP     = Heat constant
C**   VK     = von Karman constant
c**   NPDMAX = Maximum number of particle size categories


      LOGICAL WSINP, KSTINP, STP, CONTIN, POINT, FLARE, AREA, VOLUME,
     &        DISC, DEBUG
      REAL XAUTO(51), HT(50), RMIN(50), RMAX(50)
c      REAL CAVCHI(2), XR(2)
      REAL CAVCHI(4), XR(4)
      INTEGER IST(6)
      CHARACTER*80 OPTG, OPTU
      CHARACTER*1  QUERY, QUERY2, KOPT
      CHARACTER*79 TITLE
      CHARACTER*4  SOURCE
      CHARACTER*5  KPRT, VERSN
      CHARACTER*12 INPFIL, OUTFIL
      CHARACTER*2  DWASH(5)
      CHARACTER*8  RUNDAT, RUNTIM

      COMMON /INP1/ Q, ZR, HB, HL, HW, HWP
      COMMON /INP2/ IRD, IPRT, IDAT, IOUT, IOPT, IDBG
      COMMON /DATTIM/ RUNDAT, RUNTIM
      COMMON /TER/ HT, RMIN, RMAX
      COMMON /VAL/ CMAXCT, XMAXCT, TMAXCT
      COMMON /FLRINP/ HSTK, H

      COMMON /LOGS/ WSINP, KSTINP, STP, CONTIN, POINT, FLARE,
     &              AREA, VOLUME, DISC, DEBUG
      COMMON /CHRSCR/ SOURCE, QUERY, QUERY2, TITLE, OUTFIL, DWASH,
     &                OPTG, OPTU, KOPT, KPRT, VERSN, INPFIL
      COMMON /OTHSCR/ XAUTO, CAVCHI, XR, IST


C***********************************************************************
C     This is The Global Variable Definition Block for COntrol Variables
C***********************************************************************

      LOGICAL RURAL, URBAN, GRDRIS, NOSTD, NOBID, FLAT, ELEV, FSTREC

      COMMON /CNTLOG/ RURAL, URBAN, GRDRIS, NOSTD, NOBID, FLAT, ELEV,
     &                FSTREC


C***********************************************************************
C     This is The Global Variable Definition Block for Met Data
C***********************************************************************

      LOGICAL UNSTAB, NEUTRL, STABLE

      COMMON /METLOG/ UNSTAB, NEUTRL, STABLE
      COMMON /METVAR/ AFV, UREF, TA, KST, ZI, S, RTOFS, ZREF, HANE, ICI,
     &                ZIMIN(4), ZIFACT(4),
     &                USTAR, EL, VAIRMS, VDPHOR, Z0M, ZRDEP,
     &                RHO,
     &                KMAX, UINP



C***********************************************************************
C     This is The Global Variable Definition Block for Calculation
C***********************************************************************

      LOGICAL WAKE, WAKESS, WAKLOW, BUOYNT, TALL, SQUAT, SSQUAT,
     &        LDEP, DPLETE, CONC, DEPOS

      COMMON /CALCS1/ XS, YS, ZS, QS, HS, DS, VS, TS, SYINIT,
     &                SZINIT, XINIT, YINIT, ANGLE, DSBH, DSBW,
     &                IFVSEC, D, V, VD, E, WDRAD, WDSIN, WDCOS,
     &                HRVAL, NVS, VM, VF
      COMMON /CALCS2/ WAKE, WAKESS, WAKLOW, BUOYNT, TALL,
     &                SQUAT, SSQUAT, LDEP, DPLETE, CONC, DEPOS
      COMMON /CALCS3/ NPD, PDIAM(NPDMAX), PHI(NPDMAX), PDENS(NPDMAX),
     &                SC(NPDMAX), VGRAV(NPDMAX), TSTOP(NPDMAX),
     &                VDEP(NPDMAX), QCOR(NPDMAX), PCORZR(NPDMAX),
     &                PCORZD(NPDMAX)
      COMMON /CALCS4/ X, Y, ZELEV, ZFLAG, DISTR
      COMMON /CALCS5/ HE, HSP, HEFLAT, HTER, HEMWAK, ZB, ZM, IUCAT,
     &                XY, XZ, FM, FB, DTDZ, DHF, DHP, DELT,
     &                DHPB, DHPM, XF, XFM, XFB, XRAD, SBID
      COMMON /CALCS6/ US, SY, SZ, DA, ZLY, ZLB, X3LB, RINIT,
     &                CB, CM, QTK, P
      COMMON /CALCMX/ CHIMAX, UMAX, USMAX, HEMAX, ZIMAX, KSTMAX,
     &                SYMAX, SZMAX, WDMAX, IFGMAX,
     &                DEPMAX, UMAXD, USMAXD, HEMAXD, ZIMAXD, KSTMXD,
     &                SYMAXD, SZMAXD, WDMAXD, IFGMXD,
     &                CHICNT, UCNT, USCNT, HECNT, ZICNT, KSTCNT,
     &                SYCNT, SZCNT, WDCNT, IFGCNT,
     &                DEPCNT, UCNTD, USCNTD, HECNTD, ZICNTD, KSTCTD,
     &                SYCNTD, SZCNTD, WDCNTD, IFGCTD,
     &                CONSEC, DEPSEC
      COMMON /CALCSV/ KSTSAV, KSTSVD, DX


C***********************************************************************
C     This is The Global Variable Definition Block for the New Area
C     Source Algorithm - 7/7/93
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

      LOGICAL LSEG, MAXWD, FSTCAL

      COMMON /VERTEX/ IVERT, NVERT, UVERT(NVMAX), VVERT(NVMAX),
     &                VNVERT(NVMAX), WVERT(NVMAX)
      COMMON /SEGTBL/ NSEGS, UASEGS(NVMAX), UBSEGS(NVMAX)
      COMMON /BDSIDE/ LSEG, MAXWD, FSTCAL, SPA(NVMAX,2)
CISC2      COMMON /SRAREA/ AXINIT(NSRC), AYINIT(NSRC), AANGLE(NSRC),
CISC2     &                AXVERT(NSRC,NVMAX), AYVERT(NSRC,NVMAX)
      COMMON /SRAREA/ AXVERT(NVMAX), AYVERT(NVMAX)

CTMP      COMMON /WDS/ WDMAX

      INTEGER   MAXROW, MAXCOL, NDXROW, NDXCOL, MINDIR, MAXDIR
      PARAMETER ( MAXCOL=22, MAXROW=56)
      COMMON /MXTBLS/ ASPECT, TBLASP(MAXCOL), TBLDIS(MAXROW)
      COMMON /MXDATA/ MXRURA(MAXCOL,MAXROW), MXRURB(MAXCOL,MAXROW),
     &                MXRURC(MAXCOL,MAXROW), MXRURD(MAXCOL,MAXROW),
     &                MXRURE(MAXCOL,MAXROW), MXRURF(MAXCOL,MAXROW),
     &                MXURBA(MAXCOL,MAXROW), MXURBC(MAXCOL,MAXROW),
     &                MXURBD(MAXCOL,MAXROW), MXURBF(MAXCOL,MAXROW)

      COMMON /MXWDS/ NDXROW, NDXCOL, MINDIR, MAXDIR

      SAVE
