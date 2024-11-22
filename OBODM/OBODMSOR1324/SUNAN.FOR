      Function SUNAN(ihour,imint,tzone,ifdst,juldy,along,alati)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SUNAN Version 01.0                          ---
CC--- Subroutine description - calculate sun elevation angle.       ---
CC---------------------------------------------------------------------
CC
CC        ihour - hour (2 digit hour, 00 to 23)
CC        imint - minute (0 to 59)
CC        tzone - time zone (difference between GMT and your location)
CC                 positive for west longitude. zero if ihour is GMT.
CC        ifdst - day light savings time flag.
CC                  0 - standard time or if ihour is GMT.
CC                 -1 - daylight savings time.
CC        juldy - julian day
CC        along - west longitude (deg)
CC        alati - north latitude (deg)
CC
CC**********************************************************************
CC
      integer*2 jday
CC
CC*******************************************************************
CC
      jday = juldy
      time = ihour + tzone + ifdst + FLOAT(imint) * 0.01666667
      if (time .ge. 24.0) then
        jday = jday + 1
        time = time - 24.0
      end if
      cphi = alati * 0.01745329
      sphi = SIN(cphi)
      cphi = COS(cphi)
CC        0.9856479 = 360.0/365.242
      d1 = (jday - 1.0) * 0.9856479
      d2 = d1 * 0.01745329
      d3 = d2 + d2
      cd1 = COS(d2)
      sd1 = SIN(d2)
      s2d1 = SIN(d3)
      c2d1 = COS(d3)
      gam = 279.9348 + d1 + 1.914827 * sd1 - 0.079525 * cd1 + 0.019938 *
     *      s2d1 - 0.00162 * c2d1
      sd = 0.39785 * SIN(gam * 0.01745329)
      cd = SQRT(ABS(1.0 - sd * sd))
      tm = 12.0 + 0.12357 * sd1 - 0.004289 * cd1 + 0.153809 * s2d1 +
     *     0.060783 * c2d1
      alpha = sphi * sd + cphi * cd * COS(0.01745329 * (15.0 *
     *        (time - tm) - along))
      if (alpha .gt. 1.0) alpha = 1.0
      if (alpha .lt. -1.0) alpha = -1.0
CC     note:  ASIN(x) = ATAN(x / SQRT(1.0 - x * x))
      SUNAN = ARTAN(alpha,SQRT(ABS(1.0 - alpha * alpha))) * 57.29578
      if (SUNAN .gt. 90.0) SUNAN = SUNAN - 360.0
      return
      end
