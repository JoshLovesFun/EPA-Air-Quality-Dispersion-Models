CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOPN00010
      SUBROUTINE OPENA                                                  OPN00020
C                                                                       OPN00030
C  OPENA.FOR                                                            OPN00040
C                                                                       OPN00050
C  OPEN INPUT AND OUTPUT FILES                                          OPN00060
C                                                                       OPN00070
C  PLUVUE II                                                            OPN00080
C  Version 92243   Level 1.0                                            OPN00090
C                                                                       OPN00100
C  U.S. Environmental Protection Agency                                 OPN00110
C  Office of Air Quality Planning and Standards                         OPN00120
C  Research Triangle Park, North Carolina                               OPN00130
C                                                                       OPN00140
C  Modifications:                                                       OPN00150
C                                                                       OPN00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOPN00170
C                                                                       OPN00180
C********************************************************************** OPN00190
C                                                                       OPN00200
C-----This is a revised version of opena to clean up a very messy       OPN00210
C-----original                                                          OPN00220
C                                                                       OPN00230
C     Modified by Sigma Research 8/92                                   OPN00240
C                                                                       OPN00250
C     Further modified to use hard coded file names for I/O             OPN00252
C
C     Modified by Peter Eckhoff, EPA on 8/13
C
C********************************************************************** OPN00260
C                                                                       OPN00270
       CHARACTER*40 IPATH                                               OPN00280
                                                                        OPN00290
C                                                                       OPN00300
C-----Open the input and output files                                   OPN00310
C                                                                       OPN00320
C   Modified by PAE  8/13/2013                                          OPN00330
C                                                                       OPN00340
C   The PLUVUE II input file is entered via a hard-coded file name,     OPN00350
C     Pluvue.inp                                                        OPN00360
C   The output file is written to Pluvue.out which must be renamed      OPN00370
C   by the user or it will be over written on the next run.             OPN00380
C   
C   The user can name 3 files using the first 3 lines of Pluvue.inp.
C   The files, with example names, are:
C       Mietbl output file: Mietbl.inp
C       PLT file: Pluvue7.bin
C       PLOT file: Pluvue8.bin
      open (5,file='Pluvue2.inp',iostat=istat,err=5)                    OPN00390
5     if (istat .ne. 0) then                                            OPN00400
        print *, 'Error reading Pluvue.inp. ErrMsg. = ', istat          OPN00410
        stop                                                            OPN00420
      endif                                                             OPN00430
C      read(5,'(a)') ipath                                               OPN00440
C      close (5)                                                         OPN00450
C      open (5,file=ipath,iostat=istat,err=10)                           OPN00460
c      open (5,file='pluvue2.inp',iostat=istat,err=10)                   OPN00470
C10    if (istat .ne. 0) then                                            OPN00480
C         print *, '...cannot open PLUVUE II input file'                 OPN00490
C         stop                                                           OPN00500
C      endif                                                             OPN00510
      open (6,file='Pluvue2.out',status='unknown')                      OPN00520
C                                                                       OPN00530
C-----Opens the particle size distribution file on unit 9               OPN00540
C                                                                       OPN00550
      read (5,'(a)') ipath                                              OPN00560
      open (9,file=ipath,iostat=istat,err=30)                           OPN00570
30    if (istat .ne. 0) then                                            OPN00580
         print *, '...cannot open ',ipath                               OPN00590
         stop                                                           OPN00600
      endif                                                             OPN00610
C                                                                       OPN00620
C-----Opens the ***.bin binary output file on unit 7                    OPN00630
C                                                                       OPN00640
      read (5,'(a)') ipath                                              OPN00650
      open (7,file=ipath,form='unformatted',iostat=istat,err=40)        OPN00660
40    if (istat .ne. 0) then                                            OPN00670
        print *, '...cannot open ',ipath                                OPN00680
        stop                                                            OPN00690
      endif                                                             OPN00700
C                                                                       OPN00710
C-----Opens the ***.bin binary output file on unit 8                    OPN00720
C                                                                       OPN00730
      read (5,'(a)') ipath                                              OPN00740
      open (8,file=ipath,form='unformatted',iostat=istat,err=50)        OPN00750
50    if (istat .ne. 0) then                                            OPN00760
        print *, '...cannot open ',ipath                                OPN00770
        stop                                                            OPN00780
      endif                                                             OPN00790
                                                                        OPN00800
      RETURN                                                            OPN00810
      END                                                               OPN00820
