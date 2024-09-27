c
c     -------------------------------------------------------------------
c
      subroutine jul2dat(ijul,iy,im,id)
c
c  Convert julian day to calendar date
c
      integer mon(12,2), ijul,iy,im,id
c
      data mon /1,32,60,91,121,152,182,213,244,274,305,335,
     +          1,32,61,92,122,153,183,214,245,275,306,336/
c
c Convert julian day
c
      il = 1
      if(mod(iy,4) .eq. 0 .or. iy .eq. 0) il = 2 ! incorrect for 1900, 2100
      do ii = 1, 12
        i = 12 - ii + 1
        if(ijul.ge.mon(i,il)) then
           im = i
           id = ijul - mon(i,il) + 1
           return
        end if
      end do
      write(*,*) '\njul2dat (in timesubs): Bad julian day'
      write(*,'(a,i4,a,i3)') 'Year = ',iy,' Julian = ',ijul
      stop ' '
c
      end
c
c     -------------------------------------------------------------------
c
      function julian(iy,im,id)
c
      integer mon(12)
      data mon/0,31,59,90,120,151,181,212,243,273,304,334/
c Next line will fail year 2100
      if((mod(iy,4) .eq. 0 .or. iy .eq. 0) .and. im .gt. 2) then 
         julian = mon(im) + 1 + id
      else
         julian = mon(im) + id
      end if
      return
      end
c
c     -------------------------------------------------------------------
c
      subroutine legal_timestamp(iy,im,id,ih,iMaxHr)

c     Makes sure the time is a legal time, either 0-23 (if iMaxHr = 23) 
c     or 1-24 (if iMaxHr = 24).  

ckjr      do while (ih .lt. 0)
      do while (ih .le. 0)
         ih = ih + 24
         id = id - 1
         if(id .lt. 1) then
            im = im - 1
            if (im .eq. 0) then
               im = 12
               iy = iy - 1
               if (iy .lt. 0) iy = iy+100 ! may be 2-digit year
            end if
            id = id + iDaysInMth(im,iy)
         end if
      end do

      do while (ih .gt. iMaxHr)
         ih = ih - 24
         id = id + 1
         if (id .gt. iDaysInMth(im,iy)) then
            im = im + 1
            if (im .gt. 12) then
               im = 1
               iy = iy + 1
               if (iy .eq. 100) iy = 0 ! may be 2-digit year
            end if
            id = 1
         end if
      end do
      return
      end 
c
c     -------------------------------------------------------------------
c
      integer function idate8(idate)
      
c     Given an idate in YYYYMMDDHH format, return it in YYMMDDHH format.

      call idate2ymdh(idate,iy,im,id,ih)
      if (iy .gt. 99) then
         if (iy .ge. 2000) then
            iy = iy - 2000
         else
            iy = iy - 1900
         end if
         idate8 = iymdh2idate(iy,im,id,ih)
      else
         idate8 = idate
      endif

      return
      end

c
c     -------------------------------------------------------------------
c
      integer function idate10(idate)
      
c     Given an idate in YYMMDDHH format, return it in YYYYMMDDHH format.

      call idate2ymdh(idate,iy,im,id,ih)
      if (iy .le. 99) then
         if (iy .gt. 50) then
            iy = 1900 + iy
         else
            iy = 2000 + iy
         end if
         idate10 = iymdh2idate(iy,im,id,ih)
      else
         idate10 = idate
      endif

      return
      end

c
c     -------------------------------------------------------------------
c
      subroutine add_hour(iy,im,id,ih,iAdd)

c     Correctly increments a time-stamp YyMmDdHr by iAdd hours.
c     Assumes hour is 1-24.

      ih = ih + iAdd            ! iAdd may be negative
      call legal_timestamp(iy,im,id,ih,24)

      return
      end 
c
c     -------------------------------------------------------------------
c
      integer function iDaysInMth(im,iy)

      INTEGER mon(12), iy,im
      data mon/31,28,31,30,31,30,31,31,30,31,30,31/

      if((mod(iy,4).eq.0 .or. iy.eq.0) .and. im.eq.2) then ! leap year
         iDaysInMth = 29
      else
         iDaysInMth = mon(im)
      end if

      return
      end
c
c     -------------------------------------------------------------------
c
      subroutine Mth_by_num(im,uppercase,Mth)

      integer im        ! input month (1-12)
      logical uppercase ! input logical
      character*3 Mth   ! output like "JAN" or "jan"

      character*3 mon_up(12), mon_dn(12)
      data mon_up/"JAN","FEB","MAR","APR","MAY","JUN",
     &            "JUL","AUG","SEP","OCT","NOV","DEC"/
      data mon_dn/"jan","feb","mar","apr","may","jun",
     &            "jul","aug","sep","oct","nov","dec"/

      if (uppercase) then
         Mth = mon_up(im)
      else
         Mth = mon_dn(im)
      endif

      return
      end
c
c     -------------------------------------------------------------------
c
      integer function iymdh2idate(iy,im,id,ih)
      
      integer iy,im,id,ih, idate

      iymdh2idate = iy*1000000 + im*10000 + id*100 + ih
      
      return
      end

c
c     -------------------------------------------------------------------
c
      subroutine idate2ymdh(idate,iy,im,id,ih)
      
      integer iy,im,id,ih, idate

      iy = idate / 1000000
      im = (idate - iy*1000000) / 10000
      id = (idate - iy*1000000 - im*10000) / 100
      ih = (idate - iy*1000000 - im*10000 - id*100) 

      return
      end

c
c     -------------------------------------------------------------------
c
      subroutine delhrs(iy1,ij1,ih1,iy2,ij2,ih2,nhrs)
!
!  adapted from CALMET calutils.f
!
      implicit none

      integer iy1,ij1,ih1,iy2,ij2,ih2,nhrs,iymin
      integer j1, j2, iy1m1, iy2m1,i

      iymin=min(iy1,iy2)
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the first date/hour
      if(iy1.eq.iymin)then
         j1 = 0
      else
         j1 = 0
         iy1m1 = iy1 - 1
         do 10 i = iymin, iy1m1
         if(mod(i,4).eq.0)then
            j1 = j1 + 8784
         else
            j1 = j1 + 8760
         endif
10       continue
      endif
      j1 = j1 + (ij1-1)*24 + ih1
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the second date/hour
      if(iy2.eq.iymin)then
         j2 = 0
      else
         j2 = 0
         iy2m1 = iy2 - 1
         do 20 i = iymin, iy2m1
         if(mod(i,4).eq.0)then
            j2 = j2 + 8784
         else
            j2 = j2 + 8760
         endif
20       continue
      endif
      j2 = j2 + (ij2-1)*24 + ih2

      nhrs = j2 - j1       ! difference (in hours)
c
      return
      end
