CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine getmie
C                                                                              C
C  GETMIE.FOR                                                                  C
C                                                                              C
C  Select of create a library of Mie calculations for input to PLUVUE II       C
C                                                                              C
C  RUNPLUVU                                                                    C
C  DCD, Sigma Research Corporation                                             C
C
C  Version 92213   Level 1.0                                                   C
C                                                                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      integer unit5, unit6, unit7, unit8
      character*1 ans, cr, gone, in(80)
      character*100 input

      common/ units/ unit5, unit6, unit7, unit8

C   Select or create a library of Mie calculations for input to PLUVUE II
39    call system ("CLS")
      write( unit6, 40)
40    format( ' The default Mie library as input to PLUVUE II ',
     >        'contains the following: ',///)
      call system ("TYPE DEFAULTM.LST")
      write( unit6,45)
45    format(' Do you wish to use the default Mie library (Y or N)? ')
      read( unit5,25) ans
      if( ans .eq. 'Y' .OR. ans .eq. 'y') then
         write( unit7,50)
50       format('default.mie')
      else
54       write( unit6, 55)
55       format(' Do you wish to supply a Mie library file (Y or N)? ')
         read( unit5,25) ans
         if( ans .eq. 'Y' .OR. ans .eq. 'y') then
            write(unit6, 60)
60          format(' Enter the Mie library file name ',
     >      '(up to 24 characters):',/,
     >      'XXXXXXXXXXXXXXXXXXXXXXXX = 24 characters',/)
            read(unit5,25) input
C    Check if Mie library file exists
            open(1,file=input,status='old',iostat=ioval,err=63)
63          if( ioval .gt. 0.) then
               write(unit5,64)
64             format( ' Error opening file.  File does not exist.',
     >               '  Try again')
               close( 1)
               goto 54
            endif
            close( 1)
            write(unit7,25) input
         else
            write( unit6, 65)
65          format(' Do you wish to create a Mie library file',
     >              '(Y or N)? ')
            read( unit5,25) ans
            if( ans .eq. 'Y' .OR. ans .eq. 'y') then
               write( unit6, 66)
66             format( ////,' CAUTION:  The creation of a Mie Library ',
     >             'file can take hours depending upon ',/,11x,'the ',
     >             'computer being used.',/,11x,'Mie calculations ',
     >             'for large particles take much longer',
     >             /,11x,'than for ',
     >             'small particles.  ',/,11x,'Therefore, the time ',
     >             'required for the calculations for the first ',/,
     >             11x,'particle sizes in a given histogram are much ',
     >             'shorter than',/,11x,'for the last ones.',///)
               write( unit6, 75)
75             format( ' Press ENTER to Continue')
               call getans (in, cr, gone)
               call system('MIETBL.EXE')
               open( unit8, file='scratch2',status='unknown')
               read( unit8, 25) input
               write( unit7,25) input
               close (unit8)
            else
               write( unit6,70)
70             format( ' You have run out of options.',
     >                 '  You must select one of the following:',//,
     >                 ' 1)  Use the default Mie library',/,
     >                 ' 2)  Supply a Mie library file',/,
     >                 ' 3)  Create a Mie library file',//,
     >                 ' You will be prompted again.',///)
               write( unit6, 75)
               call getans (in, cr, gone)
               goto 39
            endif
         endif
      endif

25    format( a)

      return
      end
