      Subroutine GLABL(ngrpn,ngrpb,mgrpb,mgrpr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GLABL Version 01.1                          ---
CC--- Subroutine description - builds label for source groups.      ---
CC---                                                               ---
CC---------------------------------------------------------------------
CC
CC    ngrpn - source group number desired.
CC    ngrpb - number of characters in returned group label mgrpb.
CC    mgprf - returned group label.
CC
CC**********************************************************************
CC
      character*1 mrprn,mgrpr
      character*4 mbufr
      character*190 mgrpb
CC
      dimension mgrpr(190)
CC
      include 'OCNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      data mrprn/')'/
CC
CC*******************************************************************
CC
CC       source group labeling
      ngrpb = 0
      mgrpb = mblnk1
      if (ngroup .le. 0) then
        if (nsourc .gt. 1) then
CC          due to all sources.
          WRITE (mgrpb,9000)
          i = 20
        else
CC          no label required for single source.
          i = 0
        end if
      else
CC       (Due to source group ii including sources: ##,##,##,...
        WRITE (mgrpb,9001) ngrpn
        i = 34
        l = 0
        n = 1
   10   if (n .lt. ngrpn) then
          n = n + 1
          l = l + nsogrp(n-1)
          go to 10
        end if
        do 20 k=1,nsogrp(n)
          m = isogrp(l+k)
          if (m .gt. 0) then
            WRITE (mbufr,9002) m
            call MVCH1(mbufr,mgrpr(i+1),3)
            i = i + 3
          else
            j = 0
            if (k .gt. 1) j = isogrp(l+k-1)
            if (k .eq. 1.or.j .lt. 0.or.j .gt. IABS(m)) then
              WRITE (mbufr,9003)
              call MVCH1(mbufr,mgrpr(i+1),3)
              i = i + 3
              WRITE (mbufr,9002) IABS(m)
              call MVCH1(mbufr,mgrpr(i+1),3)
              i = i + 3
            else
              WRITE (mbufr,9004) IABS(m)
              call MVCH1(mbufr,mgrpr(i),4)
              i = i + 3
            end if
          end if
   20   continue
        mgrpr(i) = mrprn
      end if
      if (i .gt. 0) then
        ngrpb = JPACK(mgrpr,i)
        call MVCH1(mgrpr,mgrpb,ngrpb)
      end if
      return
CC
 9000 format ('(Due to all sources)')
 9001 format ('(Due to source group >',i2,', sources:')
 9002 format (i2,',')
 9003 format (' 1-')
 9004 format ('-',i2,',')
      end
