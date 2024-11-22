      Subroutine LUNIT(mbufr,nchrs,iunit,ifpol,iflgs,numrs,ntrdm,iposs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - LUNIT Version 01.0                          ---
CC--- Subroutine description - used with Subroutine units to convert---
CC---                          units abbreviation to units index or ---
CC---                          units index to abbreviation.         ---
CC---------------------------------------------------------------------
CC
CC    mbufr - units abbreviation character string.
CC    nchrs - number of characters in string mbufr.
CC    iunit - length units index.
CC             -1 - error return, if iflgs = 0 and abbreviation in error.
CC            >=0 - index, see subroutine UNITS.
CC    ifpol - rectangular/polar flag.
CC            0 - rectangular.
CC            1 - polar.
CC    iflgs  - abbreviation/index flag.
CC            0 - convert abbreviation in mbufr to index returned in
CC                iunit.
CC            1 - convert index iunit to abbreviation returned in
CC                mbufr.
CC    numrs - number of different abbreviations possible.
CC    ntrdm - maximum number of characters possible in an abbreviation.
CC    iposs - array containing the character sequence number for each
CC            possible character in an abbreviation, except zero means
CC            no character.
CC
CC**********************************************************************
CC
      integer*2 i,j,k
CC
      character*1 mbufr,mdumy
CC
      dimension iposs(13,4),mbufr(10),mdumy(10)
CC
      include 'ONAMES.INC'
CC
CC********************************************************************
CC
      if (iflgs .eq. 0) then
        call MBLK1(mdumy,10)
CC        move string into temp buffer.
        call MVCH1(mbufr,mdumy,nchrs)
CC        left justify string.                                          090198
        nchr = LRJST(mdumy,nchrs,1)                                     090198
        iunit = 0                                                       090198
        if (nchr .gt. 0) then                                           090198
CC          loop over characters in string.
          do 30 k=1,nchr                                                090198
CC            loop over possible character combinations.
            do 20 j=1,numrs
              i = 0
CC              loop over characters within each combination.
   10         i = i + 1
              if (i .le. ntrdm) then
CC                if another character possible.
                if (iposs(j,i) .gt. 0) then
CC                  if still have string characters left.
                  if (k+i-1 .le. nchr) then                             090198
CC                    if characters compare.
                    if (IFCHR(mdumy(k+i-1),iposs(j,i)) .ne. 0) then
CC                      characters are the same.
                      go to 10
                    else
CC                      characters are not the same.
                      go to 20
                    end if
                  else
CC                    not enough characters in string.
                    go to 20
                  end if
                else
CC                  no more possible characters in combination.
                  if (k+i-1 .le. nchr) then                             090198
CC                    still have string characters left.
                    go to 20
                  else
CC                    no string characters left.
                    go to 50
                  end if
                end if
              end if
              go to 50
   20       continue
            go to 40
   30     continue
        else                                                            090198
          go to 70                                                      090198
        end if                                                          090198
   40   iunit = -1
        go to 70
   50   iunit = j - 1
      else
        call MBLK1(mbufr,nchrs)
        if (iunit .ge. 0.and.iunit .lt. numrs) then
          if (ifpol .ne. 0) then
            mbufr(1) = malpha(30)
            mbufr(2) = malpha(31)
            mbufr(3) = malpha(33)
          else
            do 60 i=1,ntrdm
              if (iposs(iunit+1,i) .gt. 0.and.i .le. nchrs) then
                mbufr(i) = malpha(iposs(iunit+1,i))
              end if
   60       continue
          end if
          i = LRJST(mbufr,nchrs,iflrjs)
        end if
      end if
   70 return
      end
