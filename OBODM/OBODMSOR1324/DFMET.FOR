      Function DFMET(iflgs,radnx,ipasq,refsp,humid)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DFMET Version 01.1                          ---
CC--- Subroutine description - default the meteorological parameter ---
CC---                          specified by iflgs, depending on the ---
CC---                          wind speed and either the Pasquill   ---
CC---                          stability category or the net radia- ---
CC---                          tion category.                       ---
CC---------------------------------------------------------------------
CC
CC        iflgs   - parameter flag.
CC                 1 - pwrlaw - wind speed power law coefficient.
CC                 2 - sigmep - vertical turbulent intensity.
CC                 3 - sigmap - lateral turbulent intensity.
CC                 4 - hmdpth - mixing layer depth.
CC                 5 - dphdz - vertical potential temp. gradient.
CC                 6 - sigmip - longitudinal turbulent intensity.
CC        radnx  - net radiation index (-1 to 4), -2 treated as -1.
CC        ipasq  - Pasquill stability category 1 to 6.
CC        refsp  - reference wind speed (m/s).
CC        humid  - humidity (%).
CC
CC**********************************************************************
CC
      integer*2 j,ispct,netrd
CC
      dimension spcat(4),sgadf(6,5),sgedf(6,5),pwrdf(6,5),
     *          hmhdf(6,5),dphdz(6,5,2)
CC
      data spcat/1.0,3.0,5.0,7.0/,
     *     sgadf/26.0,22.0,21.0,15.0,11.0, 4.0,
     *           22.0,20.0,16.0,11.0,10.0, 4.0,
     *           19.0,16.0,13.0,11.0, 9.0, 7.0,
     *           15.0,13.0,12.0, 9.0, 8.0, 6.7,
     *           13.0,12.0,10.0, 8.0, 7.0, 6.7/,
     *     sgedf/ 8.7, 8.7, 7.0, 5.0, 4.0, 3.0,
     *            7.0, 7.0, 6.0, 4.0, 3.5, 3.0,
     *            7.0, 6.0, 5.5, 5.3, 5.0, 5.0,
     *            6.0, 5.3, 5.0, 4.8, 4.8, 4.7,
     *            5.3, 5.0, 4.5, 4.5, 4.5, 4.5/
      data pwrdf/0.10,0.10,0.20,0.20,0.25,0.40,
     *           0.10,0.10,0.20,0.20,0.25,0.30,
     *           0.10,0.10,0.15,0.15,0.20,0.25,
     *           0.10,0.10,0.10,0.10,0.10,0.20,
     *           0.10,0.10,0.10,0.10,0.10,0.10/
      data dphdz/0.000,0.000,0.000,0.015,0.030,0.035,
     *           0.000,0.000,0.000,0.010,0.020,0.025,
     *           0.000,0.000,0.000,0.005,0.015,0.015,
     *           0.000,0.000,0.000,0.003,0.010,0.010,
     *           0.000,0.000,0.000,0.003,0.003,0.003,
     *           0.000,0.000,0.000,0.020,0.030,0.040,
     *           0.000,0.000,0.000,0.010,0.020,0.030,
     *           0.000,0.000,0.000,0.005,0.010,0.020,
     *           0.000,0.000,0.000,0.000,0.005,0.010,
     *           0.000,0.000,0.000,0.000,0.000,0.005/,
     *     hmhdf/2500.0,2000.0,1000.0, 500.0, 100.0,  30.0,
     *           2200.0,1800.0,1200.0, 600.0, 200.0, 100.0,
     *           1800.0,1500.0,1200.0, 600.0, 300.0, 200.0,
     *           1500.0,1200.0,1000.0, 600.0, 300.0, 200.0,
     *           1200.0,1000.0, 700.0, 500.0, 300.0, 200.0/
CC
CC*********************************************************************
CC
CC      determine wind speed category category ispct.
      j = 0
      ispct = 0
   10 if (j .eq. 0.and.ispct .lt. 4) then
        ispct = ispct + 1
        if (refsp .lt. spcat(ispct)) then
          j = 1
        end if
        go to 10
      end if
      if (j .eq. 0) ispct = 5
CC      net radiation index.
      netrd = radnx
      netrd = MAX0(MIN0(5-netrd,6),1)
CC      if power law coefficient.
      if (iflgs .eq. 1) then
        DFMET = pwrdf(netrd,ispct)
CC      if vertical turbulence.
      else if (iflgs .eq. 2) then
        DFMET = sgedf(netrd,ispct)
CC      if lateral turbulence.
      else if (iflgs .eq. 3) then
        DFMET = sgadf(netrd,ispct)
CC      if mixing layer depth.
      else if (iflgs .eq. 4) then
        DFMET = hmhdf(netrd,ispct)
CC      if vertical gradient of potential temperature.
      else if (iflgs .eq. 5) then
        if (humid .gt. 70.0) then
          ihum = 1
        else
          ihum = 2
        end if
        ipasq = IPSQL(radnx,refsp)
        DFMET = dphdz(ipasq,ispct,ihum)
CC      if longitudinal turbulence.
      else if (iflgs .eq. 6) then
        DFMET = 1.33 * sgadf(netrd,ispct)
      end if
      return
      end
