      Function IRNDX(cldcv,ceiln,sunng)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IRNDX Version 01.0                          ---
CC--- Subroutine description - calculate the net radiation index.   ---
CC---------------------------------------------------------------------
CC
CC        cldcv - cloud cover in 8'ths of the celestial dome.
CC        ceiln - cloud ceiling height in meters.
CC        sunng - sun angle in degrees.
CC
CC**********************************************************************
CC
      integer*2 im
CC
CC*******************************************************************
CC
      IRNDX = 0
      if (cldcv .lt. 8.0.or.ceiln .ge. 2133.6) then
        if (sunng .le. 0.0) then
CC          3.2 8'ths or 4 10'ths
          if (cldcv .le. 3.2) then
            IRNDX = -2
          else
            IRNDX = -1
          end if
        else
          if (sunng .le. 15.0) then
            IRNDX = 1
          else if (sunng .le. 35.0) then
            IRNDX = 2
          else if (sunng .le. 60.0) then
            IRNDX = 3
          else
            IRNDX = 4
          end if
CC          4 8'ths or 5 10'ths
          if (cldcv .gt. 4.0) then
            im = 0
            if (ceiln .lt. 2133.6) then
              im = 2
            else
              if (ceiln .lt. 4876.8) then
                im = 1
              end if
              if (cldcv .ge. 8.0) then
                im = im + 1
              end if
            end if
            IRNDX = IRNDX - im
            if (im .ne. 0.and.IRNDX .lt. 1) then
              IRNDX = 1
            end if
          end if
        end if
      end if
      return
      end
