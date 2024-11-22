CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine getinp
C                                                                              C
C  GETINP.FOR                                                                  C
C                                                                              C
C  Prepare an input file for PLUVUE II or specify a previously prepared file   C
C                                                                              C
C  RUNPLUVU                                                                    C
C  DCD, Sigma Research Corporation                                             C
C                                                                              C
C  Version 92213   Level 1.0                                                   C
C                                                                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      integer unit5, unit6, unit7, unit8
      character*1 ans
      character*100 input

      common/ units/ unit5, unit6, unit7, unit8

      open( unit7, file='scratch',status='unknown')

C   Prepare an input file for PLUVUE II or specify a previously prepared file
10    call system ("CLS")
      write ( unit6, 20)
20    format(' Do you wish to modify an existing',
     >   ' PLUVUE II input file (Y or N)? ')
      read( unit5,25) ans
25    format( a)
      if( ans .eq. 'Y' .OR. ans .eq. 'y') then
         call system("PLUIN2.EXE")
         open( unit8,file='scratch2',status='unknown')
         read( unit8,25) input
         write( unit7,25) input
         close( unit8)
      else
         write( unit6,35)
35       format(' Do you wish to use an existing PLUVUE II input file',
     >          ' (Y or N)? ')
         read( unit5,25) ans
         if( ans .eq. 'Y' .OR. ans .eq. 'y') then
44          write( unit6,45)
45          format(/,' Enter the PLUVUE II input file name ',
     >          '(up to 24 characters):',/,
     >          'XXXXXXXXXXXXXXXXXXXXXXXX = 24 characters',/)
            read( unit5,25) input
C    Check to see if file exists
            open( 1,file=input,status='old',iostat=ioval,err=50)
50          if( ioval .gt. 0) then
                write( unit6,55)
55              format( ' Error opening file.  File does not exist. ',
     >                 ' Try again.')
                close( 1)
                goto 44
            else
                close( 1)
            endif
            write( unit7,25) input
         else
            write( unit6,70)
70          format(//,' You have run out of INPUT options.',/,
     >            ' You must either modify an existing PLUVUE II ',
     >            'input file or',/,' specify a file for input to ',
     >            'PLUVUE II.  ',//,' Do you wish to exit (Y or N)? ')
            read( unit5,25) ans
            if( ans .EQ. 'Y' .OR. ans .EQ. 'y') then
               stop 'Exiting RUNPLUVU'
            else
              goto 10
            endif
         endif
      endif

      return
      end
