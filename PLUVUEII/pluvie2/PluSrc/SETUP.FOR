CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine setup
C                                                                              C
C  SETUP.FOR                                                                   C
C                                                                              C
C  Output Initial PLUVUE II Run Command System Text                            C
C                                                                              C
C  RUNPLUVU                                                                    C
C  DCD, Sigma Research Corporation                                             C
C
C  Version 92213   Level 1.0                                                   C
C                                                                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      integer unit5, unit6, unit7, unit8
      character*1 cr, gone, in(80)
      character*8 thetime, thedate

      common/ units/ unit5, unit6, unit7, unit8

      call system ("CLS")
      call date_and_time (thedate, thetime)

      write (unit6, 10) thetime,thedate
10    format(//,14x,a8,38x,a8)

      write (unit6,15)
15    format(///, 20x,' PLUVUE II Run Command System',//,
     >  ' A program designed to assist the user with the application',/
     >  ' of the PLUVUE II visibility model by allowing the user to:',//,
     >  '  1)  Prepare an input file or to specify a previously',
     >  ' prepared file',//,
     >  '  2)  Select or create a library of Mie calculations',
     >  ' for input to PLUVUE II',//
     >  '  3)  Run PLUVUE II',///,
     >  ' Press ENTER to Continue')

      call getans (in, cr, gone)

      return
      end
