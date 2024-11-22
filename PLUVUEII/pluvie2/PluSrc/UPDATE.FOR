CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine update
C                                                                              C
C  UPDATE.FOR                                                                  C
C                                                                              C
C  Update PLUVUE II file names                                                 C
C                                                                              C
C  RUNPLUVU                                                                    C
C  DCD, Sigma Research Corporation                                             C
C                                                                              C
C  Version 92213   Level 1.0                                                   C
C                                                                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      integer unit5, unit6, unit7, unit8
      character*5 cop/'copy '/
      character*9 scr/'scratch3 '/
      character*80 line
      character*100 pluvueinp, mietbl
      character*140 ramfile,copy

      common/ units/ unit5, unit6, unit7, unit8

C  Input the PLUVUE II input and MIETBL filenames
      rewind( unit7)
      read( unit7, 25) pluvueinp
      read( unit7, 25) mietbl
      close (unit7)

C  Update PLUVUE II input file with mie table filename
      open(unit7,file=pluvueinp,iostat=ioval)
      open(unit8,file='scratch3',status='unknown')

      read(unit7,25) line
      write(unit8,25) mietbl
200   read(unit7,25,end=300) line
      write(unit8,25) line
      goto 200

300   close( unit7)
      close( unit8)

C   Copy SCRATCH3 file back into PLUVUEINP
      write( ramfile,*) cop,scr,pluvueinp
      read( ramfile,25) copy
      call system( copy)

      close( unit7)
      close( unit8)

25    format( a)

      return
      end
