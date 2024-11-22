CC-OBODM particle related routines
      Function TRMVS(radus,k)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - TRMVS Version 01.1                          ---
CC--- Subroutine description - calculate particle settling velocity ---
CC---                          (cm/sec).                            ---
CC---------------------------------------------------------------------
CC
CC        argument -
CC        radus - particle radius (cm).
CC
CC        common variables used -
CC        densty - density of the particle material (g/cm**3).
CC        airden - air density (g/cm**3).
CC        airvsc - air viscosity (poise).
CC        iotfil - print output logical units number.
CC        llines - print output line counter.
CC
CC        returned -
CC        TRMVS  - particle settling velocity (cm/sec).
CC                 >=0 - settling velocity.
CC                  <0 - error.
CC
CC**********************************************************************
CC
      integer*2 l
CC
      dimension rcst(44)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'ONAMES.INC'
CC
      data rcst/1.0,2.0,4.0,6.0,10.0,20.0,40.0,60.0,100.0,
     *     200.0,400.0,600.0,1.0e3,2.0e3,4.0e3,6.0e3,1.0e4,2.0e4,
     *     4.0e4,6.0e4,1.0e5,2.0e5,24.0,64.0,140.0,230.0,420.0,
     *     1160.0,2880.0,5400.0,1.2e4,3.2e4,9.76e4,1.94e5,4.6e5,
     *     1.64e6,6.4e6,1.44e7,4.1e7,1.76e8,7.33e8,1.65e9,4.4e9,
     *     1.64e10/
CC
CC**********************************************************************
CC
      TRMVS = 0.0
      t15 = radus * radus
      airden = AIRDN(airprs,airtmp,airhum)
      airvsc = AIRVS(airtmp)
      if (densty .gt. 0.0) then
        den = densty
      else
        den = 1.0
      end if
      t14 = 10453.3333 * den * airden / (airvsc * airvsc) * radus *
     *      t15
      if (t14 .le. 24.0) then
        TRMVS = 217.77777 * den / airvsc * t15
      else
        if (t14 .le. rcst(44)) then
          l = 1
   10     if (l .le. 21.and.t14 .gt. rcst(l+23)) then
            l = l + 1
            go to 10
          end if
          TRMVS = 0.5 * airvsc / airden * (rcst(l) * (t14 /
     *            rcst(l+22))**(ALOG(rcst(l+1) / rcst(l)) /
     *            ALOG(rcst(l+23) / rcst(l+22)))) / radus
        else
          i = KERRS(13,mnpbuf,k,idum,xdum,xdum,xdum,xdum,0)
          TRMVS = -1.0
        end if
      end if
      return
      end
