!Contains:
!
! subroutine jul2dat(ijul,iy,im,id)
! subroutine dat2jul(iy,im,id,ijul)
! subroutine legal_timestamp(iy,im,id,ih,iMaxHr)
! subroutine add_hour(iy,im,id,ih,iAdd)
! subroutine Mth_by_num(im,uppercase,Mth)
! subroutine idate2ymdh(idate,iy,im,id,ih)
! subroutine ymdh2nDatHr(iy,im,id,ih,nDatHr)
! subroutine nDatHr2iDate10(nDatHr,idate)
! subroutine nDatHr2ymdh(nDatHr,iy,im,id,ih,iMaxHr)
! subroutine TimeStamp2ymdh(TimeStamp,iy,im,id,ih)
! subroutine TimeStampDiff(TimeStamp1,TimeStamp2,hrs)
! subroutine TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, hrs)
! 
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     This file contains various time-related subroutines and functions.
!
!     Development History:
!     2009-05-26  Original Development (ENVIRON International Corp.)
!     2011-09-30  Minor enhancements
!     2012-03-07  Added ymdh2nDatHr, increased usage of Leap_yr().
!     2013-05-02  Bug fix in TimeDiff(), needed to call Leap_yr(i) inside loop.
!     2013-07-16  Bug fix in legal_timestamp(), was returning range 0-24, when
!                 it should return either range 0-23 or 1-24.
!
!------------------------------------------------------------------------------
!
subroutine jul2dat(ijul,iy,im,id)
!
!-----Convert Julian day to calendar date
!
  integer mon(12,2),ijul,iy,im,id
  logical Leap_yr
  data mon /1,32,60,91,121,152,182,213,244,274,305,335, &
            1,32,61,92,122,153,183,214,245,275,306,336/

  il = 1
  if (Leap_yr(iy)) il = 2
  do ii = 1,12
     i = 12 - ii + 1
     if (ijul.ge.mon(i,il)) then
        im = i
        id = ijul - mon(i,il) + 1
        return
     end if
  end do
  write(*,*) 'jul2dat (in timesubs.f90): Bad julian day'
  write(*,'(a,i4,a,i3)') 'Year = ',iy,' Julian = ',ijul
  stop

end subroutine jul2dat
!
!------------------------------------------------------------------------------
!
subroutine dat2jul(iy,im,id,ijul)
!
!-----Convert calendar date to Julian date
!
  integer iy,im,id,ijul,jday(12)
  logical Leap_yr
  data jday /0,31,59,90,120,151,181,212,243,273,304,334/

  ijul = jday(im) + id
  if (im <= 2) return
  if (Leap_yr(iy)) ijul = ijul + 1

  return
end subroutine dat2jul
!
!------------------------------------------------------------------------------
!
integer function julian(iy,im,id)
!
!-----Convert calendar date to Julian date
!
  integer mon(12)
  logical Leap_yr
  data mon/0,31,59,90,120,151,181,212,243,273,304,334/

  if (Leap_yr(iy) .and. im > 2) then
     julian = mon(im) + id + 1
  else
     julian = mon(im) + id
  end if

  return
end function julian
!
!------------------------------------------------------------------------------
!
subroutine legal_timestamp(iy,im,id,ih,iMaxHr)
!
!-----Makes sure the time is a legal time, either 0-23 (if iMaxHr = 23) 
!     or 1-24 (if iMaxHr = 24).  
!
  do while (ih < iMaxHr-23)
     ih = ih + 24
     id = id - 1
     if (id < 1) then
        im = im - 1
        if (im == 0) then
           im = 12
           iy = iy - 1
           if (iy < 0) iy = iy + 100    ! May be 2-digit year
        end if
        id = id + iDaysInMth(im,iy)
     end if
  end do

  do while (ih > iMaxHr)
     ih = ih - 24
     id = id + 1
     if (id > iDaysInMth(im,iy)) then
        im = im + 1
        if (im > 12) then
           im = 1
           iy = iy + 1
           if (iy == 100) iy = 0        ! May be 2-digit year
        end if
        id = 1
     end if
  end do
  return

end subroutine legal_timestamp
!
!------------------------------------------------------------------------------
!
integer function idate8(idate)
!
!-----Given an idate in YYYYMMDDHH format, return it in YYMMDDHH format.
!
  call idate2ymdh(idate,iy,im,id,ih)
  if (iy.gt.99) then
     if (iy.ge.2000) then
        iy = iy - 2000
     else
        iy = iy - 1900
     end if
     idate8 = iymdh2idate(iy,im,id,ih)
  else
     idate8 = idate
  endif
  return

end function idate8
!
!------------------------------------------------------------------------------
!
integer function idate10(idate)
!
!-----Given an idate in YYMMDDHH format, return it in YYYYMMDDHH format.
!
  call idate2ymdh(idate,iy,im,id,ih)
  if (iy.le.99) then
     if (iy.gt.50) then
        iy = 1900 + iy
     else
        iy = 2000 + iy
     end if
     idate10 = iymdh2idate(iy,im,id,ih)
  else
     idate10 = idate
  endif
  return

end function idate10
!
!------------------------------------------------------------------------------
!
subroutine add_hour(iy,im,id,ih,iAdd)
!
!-----Correctly increments a time-stamp YYMMDDHH by iAdd hours.
!     Assumes hour is 0-23, not 1-24.
!
  ih = ih + iAdd                                      ! iAdd may be negative
  call legal_timestamp(iy,im,id,ih,23)
  return

end subroutine add_hour
!
!------------------------------------------------------------------------------
!
logical function leap_yr(Yr) ! Y2K correct.

  integer yr

! If integer divide and float divide give the same result, then it's
! evenly divisible.  Could have used mod() here, but I think this is
! more portable.  Might give problems on some old Pentium systems
! with that pesky 4./2. = 1.9999998 error.

! The year is 365.2422 days long.
! Rules for leap years: if year is evenly divisible by 4, then it's
! a leap year, except those evenly divisible by 100, but there is a
! leap year in those evenly divisible by 400.  This will give a mean
! year 365.2425 days long.  Error is .0003 days (25.92 seconds) so
! it will take 3333 years for the calendar to get off by one day.

  if ((float(Yr/4) .eq. Yr/4.) .and. ((float(Yr/100) .ne. Yr/100.) &
       .or. (float(Yr/400) .eq. Yr/400.))) then
     leap_yr = .true.
  else
     leap_yr = .false.
  end if
  
  return
end function leap_yr
!
!------------------------------------------------------------------------------
!
integer function iDaysInMth(im,iy)
!
!-----Sets the number of days in the month
!
  integer mon(12),iy,im
  logical Leap_yr
  data mon/31,28,31,30,31,30,31,31,30,31,30,31/

  if (Leap_yr(iy) .and. im == 2) then
     iDaysInMth = 29
  else
     iDaysInMth = mon(im)
  end if
  return

end function iDaysInMth
!
!------------------------------------------------------------------------------
!
subroutine Mth_by_num(im,uppercase,Mth)
!
!-----Returns a 3-character name for an input numerical month
!
  integer im        ! input month (1-12)
  logical uppercase ! input logical
  character*3 Mth   ! output like "JAN" or "jan"

  character*3 mon_up(12),mon_dn(12)
  data mon_up/"JAN","FEB","MAR","APR","MAY","JUN",   &
              "JUL","AUG","SEP","OCT","NOV","DEC"/
  data mon_dn/"jan","feb","mar","apr","may","jun",   &
              "jul","aug","sep","oct","nov","dec"/

  if (uppercase) then
     Mth = mon_up(im)
  else
     Mth = mon_dn(im)
  endif
  return

end subroutine Mth_by_num
!
!------------------------------------------------------------------------------
!
integer function iymdh2idate(iy,im,id,ih)
!
!-----Returns YYMMDDHH stamp from year, month, day, hour input
!
  integer iy,im,id,ih

  iymdh2idate = iy*1000000 + im*10000 + id*100 + ih
  return

end function iymdh2idate
!
!------------------------------------------------------------------------------
!
subroutine idate2ymdh(idate,iy,im,id,ih)
!
!-----Returns year, month, day, hour from YYMMDDHH or YYYYMMDDHH input
!
  integer iy,im,id,ih, idate

  iy = idate/1000000
  im = (idate - iy*1000000)/10000
  id = (idate - iy*1000000 - im*10000)/100
  ih = (idate - iy*1000000 - im*10000 - id*100) 
  return

end subroutine idate2ymdh
!
!------------------------------------------------------------------------------
!
subroutine ymdh2nDatHr(iy,im,id,ih,nDatHr)
!
!-----Returns YYJJJHH from YYYY,MM,DD,HH
!
  integer iy,im,id,ih,nDatHr, jday

  call dat2jul(iy,im,id,jday)
  nDatHr = iy*100000 + jday*100 + ih
  return
end subroutine ymdh2nDatHr
!
!------------------------------------------------------------------------------
!
subroutine nDatHr2iDate10(nDatHr,idate)
!
!------Returns YYYYMMDDHH from YYJJJHH
!
  integer nDatHr,idate,jday,iy,im,id,ih

  iy   = nDatHr/100000
  jday = (nDatHr - iy*100000)/100
  ih   = nDatHr - iy*100000 - jday*100
  call jul2dat(jday,iy,im,id)
  idate = iymdh2idate(iy,im,id,ih)
  return
end subroutine nDatHr2iDate10
!
!------------------------------------------------------------------------------
!
subroutine nDatHr2ymdh(nDatHr,iy,im,id,ih,iMaxHr)
!
!------Returns YYYY,MM,DD,HH from YYJJJHH
!      if iMaxHr = 23, uses hours 0-23 (CALMET format)
!      if iMaxhr = 24, uses hours 1-24 (AERMOD format)
!
  integer nDatHr,jday,iy,im,id,ih

  iy   = nDatHr/100000
  jday = (nDatHr - iy*100000)/100
  ih   = nDatHr - iy*100000 - jday*100
  call jul2dat(jday,iy,im,id)

  call legal_timestamp(iy,im,id,ih,iMaxHr)
  if (ih == 0 .and. iMaxHr == 24) then ! Turn Hour=0 to Hour=24 previous day
     ih = ih -1
     call legal_timestamp(iy,im,id,ih,23)
     ih = ih + 1
     call legal_timestamp(iy,im,id,ih,24)
  endif

  return

end subroutine nDatHr2ymdh
!
!------------------------------------------------------------------------------
!
subroutine TimeStamp2ymdh(TimeStamp,iy,im,id,ih)
!
!------Returns year, month, day, hour from a WRF/MM5 time-stamp string, 
!      e.g. 2001-01-01_10:00:00 (WRF) or 2004-12-31_14:00:00.0000 (MM5)
!
  character (len=*), intent(in) :: TimeStamp
  integer, intent(out) :: iy,im,id,ih

  read(TimeStamp,'(i4,x,i2,x,i2,x,i2)') iy, im, id, ih

end subroutine TimeStamp2ymdh
!
!------------------------------------------------------------------------------
!
subroutine TimeStampDiff(TimeStamp1,TimeStamp2,hrs)
!
!------Returns the number of hours between two WRF/MM5 time-stamp strings
!
  character (len=*), intent(in) :: TimeStamp1, TimeStamp2
  integer,          intent(out) :: hrs
  integer iy1,im1,id1,ih1, iy2,im2,id2,ih2 ! local variables

  call TimeStamp2ymdh(TimeStamp1,iy1,im1,id1,ih1)
  call TimeStamp2ymdh(TimeStamp2,iy2,im2,id2,ih2)

  call TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, hrs)

  return
end subroutine TimeStampDiff
!
!------------------------------------------------------------------------------
!
subroutine TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, hrs)
!
!------Returns the number of hours between two WRF/MM5 time-stamp strings
!
  integer, intent(in)  :: iy1,im1,id1,ih1, iy2,im2,id2,ih2
  integer, intent(out) :: hrs
  integer minyr, hrs1,hrs2 ! local variales
  logical Leap_yr
!
!-----Calculate the number of hours since the beginning of the earliest year:
!
  minyr = min(iy1,iy2) - 1

  hrs1 = 24*(id1-1) + ih1 ! days of this month, plus hours for today
  do i = minyr, iy1-1     ! possibly zero times through the loop
     hrs1 = hrs1 + 8760   ! add the hours for all the years until this year
     if (Leap_yr(i)) hrs1 = hrs1 + 24
  end do
  if (im1 > 1) then       ! add the hours for all the months until last month
     do i = 1, im1-1
        hrs1 = hrs1 + iDaysInMth(i,iy1) * 24
     end do
  end if

  hrs2 = 24*(id2-1) + ih2 ! days of this month, plus hours for today
  do i = minyr, iy2-1     ! possibly zero times through the loop
     hrs2 = hrs2 + 8760   ! add the hours for all the years until this year
     if (Leap_yr(i)) hrs2 = hrs2 + 24
  end do
  if (im2 > 1) then       ! add the hours for all the months until last month
     do i = 1, im2-1
        hrs2 = hrs2 + iDaysInMth(i,iy2) * 24
     end do
  end if

  hrs = hrs2 - hrs1       ! answer is the difference

end subroutine TimeDiff
!
!------------------------------------------------------------------------------
!
