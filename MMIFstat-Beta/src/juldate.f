      subroutine juldate(iyr,idate)

c-----JULDATE converts date from calender (MMDD) format to Julian
c     (JJJ) format
                          
      implicit none

      integer iyr,idate

      integer imonth,iday,mday,n,jday
      integer ndys(12)

      data ndys/31,28,31,30,31,30,31,31,30,31,30,31/
      
      imonth = idate/100 
      iday = mod(idate,100)

      ndys(2) = 28
      if (mod(iyr,4).eq.0) ndys(2) = 29
      mday = 0
      do 10 n = 1,imonth-1
        mday = mday + ndys(n)
 10   continue
      jday = mday + iday

      idate = jday

      return
      end
