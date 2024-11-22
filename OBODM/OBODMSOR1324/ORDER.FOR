      Subroutine ORDER(yd,key,npnts,kyfld,iupdn,iflgs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ORDER Version 01.1                          ---
CC--- Subroutine description - sort data in ascending or descending ---
CC---                          order.                               ---
CC---------------------------------------------------------------------
CC
CC               arguments -
CC         yd - array to be sorted. contains one or more sub arrays,
CC              depending on the number of non-zero values in kyfld.
CC              the program sorts the first array in yd and orders
CC              the other arrays accordingly.
CC        key - starting index in the first array. the sort goes from
CC              key to npnts. key is normally = 1.
CC      npnts - number of data ponts to sort.
CC              no sort done if npnts is less than 2.
CC      kyfld - array of starting indices of the respective arrays
CC              in the array yd. up to 6 arrays can be sorted. the
CC              sort ends with the first value of kyfld = 0.
CC      iupdn - direction of the sort
CC              >0 - ascending order
CC              =0 - do not sort
CC              <0 - descending order
CC      iflgs  - flag indicating type of data.
CC               1 - x axis coordinates, xcoord.
CC               2 - y axis coordinates, ycoord.
CC               3 - x,y,z discrete coordinates, xdscrt, ydscrt, zdscrt
CC               4 - particle data drpupr, drplwr, pctmat, vspart,
CC                   gammad.
CC               5 - special plot symbol data, spclxs, spclys, spclht,
CC                   spclan, spcclc, spcpol.
CC
CC**********************************************************************
CC
      integer*2 n,kfld1,lend,key1,key2,jflgs,iupdn
CC
      include 'OCNTRL.INC'
      include 'OCALCS.INC'
CC
      dimension yd(2002),kyfld(6)
CC
CC*********************************************************************
CC
CC        do not need to sort a list with a length less than 2
CC        or if no sort is requested.
      if (npnts .ge. 2.and.iupdn .ne. 0) then
CC
CC        do not need to sort a list with no data or one with data only
CC        in the first location.
        kfld1 = kyfld(1)
        lend = IZGET(yd(kfld1+key-1),npnts,num) - 1
        if (lend .lt. 1) return
CC
CC        begin the sort.
        do 40 key1=key-1,lend-1
          do 30 key2=key1+1,lend
            zsav = yd(kfld1+key1)
            ydk2 = yd(kfld1+key2)
CC            if set missing
            if (INONE(ydk2) .gt. 0) then
              jflgs = 1
              if (iupdn .ge. 0) then
CC                if > and not missing
                if (ydk2 .gt. zsav.and.INONE(zsav) .gt. 0) jflgs = 0
              else
CC                if < and not missing
                if (ydk2 .lt. zsav.and.INONE(zsav) .gt. 0) jflgs = 0
              end if
              if (jflgs .ne. 0) then
CC
CC                swap sort variable
                yd(kfld1+key1) = ydk2
                yd(kfld1+key2) = zsav
CC
CC                swap associated variables
CC
                if (iflgs .le. 2) then
                  do 10 j=1,nmxxyz
                    if (iflgs .eq. 1) then
                      kfld3 = (j - 1) * nmxxyz + kfld1 + key1
                      kfld4 = (j - 1) * nmxxyz + kfld1 + key2
                    else
                      kfld3 = (kfld1 + key1 - 1) * nmxxyz + j
                      kfld4 = (kfld1 + key2 - 1) * nmxxyz + j
                    end if
                    zsav = zcoord(kfld3)
                    zcoord(kfld3) = zcoord(kfld4)
                    zcoord(kfld4) = zsav
   10             continue
                else
                  do 20 n=2,6
                    if (kyfld(n) .ne. 0) then
                      kfld3 = kyfld(n) + key1
                      kfld4 = kyfld(n) + key2
                      zsav = yd(kfld3)
                      yd(kfld3) = yd(kfld4)
                      yd(kfld4) = zsav
                    end if
   20             continue
                end if
              end if
            end if
   30     continue
   40   continue
      end if
      return
      end
