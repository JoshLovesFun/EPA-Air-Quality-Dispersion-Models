C=======================================================================
      MODULE StartVars
C     Variable declarations for run start date/time 
C
C     Contains the following procedures: RunStart

C=======================================================================

      implicit none
      save

      integer (kind=4)              :: run_start(8)  ! start date/time
      integer (kind=4)              :: iYr2          ! 2-digit year, current run
      integer (kind=4), parameter   :: ModLen = 20   ! Max length of ModNam
      character (len=ModLen)        :: ModNam        ! Module/Procedure Name
      
      contains
      
      SUBROUTINE RunStart
      
C     Get date and time at run start
      call date_and_time(values=run_start)

C     Get 2-digit year for runtime/date
      iYr2 = run_start(1) - 100 * INT( run_start(1)/100 )
      
      END SUBROUTINE RunStart
      
      END MODULE StartVars