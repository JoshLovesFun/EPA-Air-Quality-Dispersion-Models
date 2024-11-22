      Subroutine LBITQ(i,j)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - LBITQ Version 01.1                          ---
CC--- Subroutine description - load plot points, set bits on.
CC---------------------------------------------------------------------
CC
CC***********************************************************************
CC
      integer*2 ipbyt,ibit
CC
CC      note the array ipbyt is equivalenced to common array calcsa.
CC      if calcsa is reduced in size, check idim and jdim to make
CC      sure plotting capability is not impaired.  see also
CC      Subroutines HD01Q and HD02Q.
      dimension ipbyt(40000)
CC
      include 'OCNTRL.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (ilinem(6),ioff),(ilinem(7),jstrt),(ilinem(8),jendt),
     *            (ilinem(9),jmax),(ilinem(10),idim),(ilinem(11),jflg),
     *            (ilinem(14),lcolr),(calcsa(1),ipbyt(1))
CC
CC***********************************************************************
CC
CC      limit points saved to those that ly within the current plot
CC      segment.
      if (j .ge. jstrt.and.j .le. jendt) then
        if (jflg .eq. 0) then
CC          if no color.
          iwrd = (i + 15) / 16
          ibit = 15 - MOD(i-1,16)
        else
CC          if color.
          iwrd = (i + 1) / 2
          if (ifcolr .eq. 0) then
            ibit = 7
          else
            ibit = lcolr
          end if
          ibit = ibit + MOD(i,2) * 8
        end if
CC        determine which word (byte pair) to add bit to.
        jj = j - jstrt + 1
        jmax = MAX0(jj,jmax)
        jj = (jj - 1) * idim + iwrd + ioff
CC        or bit into correct position.
        ipbyt(jj) = IBSET(ipbyt(jj),ibit)
      end if
      return
      end
