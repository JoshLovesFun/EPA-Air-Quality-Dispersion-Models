      subroutine caldate(iyr,idat)
  
c-----CALDATE converts date from Julian (JJJ) format to calender
c     (MMDD) format
c                            
c     Input arguments:  
c        iyr                year (YYYY)
c        idat               julian date (JJJ)  
c     Output arguments:  
c        idat               calender date (MMDD)  
              
      implicit none

      integer iyr,idat

      integer jday,mday,imonth,iday
      integer nday(12)

      data nday/31,28,31,30,31,30,31,31,30,31,30,31/

      jday = idat
      nday(2) = 28
      if (mod(iyr,4).eq.0) nday(2) = 29
      mday = 0
      do 10 imonth = 1,12
        mday = mday + nday(imonth)
        if (mday.ge.jday) go to 20
 10   continue
 20   iday = jday - (mday - nday(imonth))
      idat = imonth*100 + iday

 9999 return
      end
