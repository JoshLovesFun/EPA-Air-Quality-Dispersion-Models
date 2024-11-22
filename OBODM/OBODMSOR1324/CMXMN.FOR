      Subroutine CMXMN(ii,jj,nn,ni,nj,array,dmax,dmin,ini,inj)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CMXMN Version 01.0                          ---
CC--- Subroutine description - determine max and min of array.      ---
CC---------------------------------------------------------------------
CC
CC        array - array of real values
CC        ni    - end i index in array.
CC        nj    - end j index in array.
CC        ini   - returned index of i at maximum.
CC        inj   - returned index of j at maximum.
CC        ii    - start i index in array.
CC        jj    - start j index in array.
CC        nn    - i dimension of array.
CC        dmax  - returned maximum.
CC        dmin  - returned minimum.
CC
CC*********************************************************************
CC
      integer*2 i,j
CC
      dimension array(80000)
CC
      include 'OCNTRL.INC'
CC
CC*********************************************************************
CC
      dmax = rmissd
      dmin = -rmissd
      do 20 j=jj,nj
        k = (j - 1) * nn
        do 10 i=ii,ni
          czmax = array(i+k)
          dmin = AMIN1(dmin,czmax)
          if (dmax .lt. czmax) then
            dmax = czmax
            ini = i
            inj = j
          end if
   10   continue
   20 continue
      return
      end
