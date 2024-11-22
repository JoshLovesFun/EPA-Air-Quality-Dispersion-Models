      Subroutine FILLQ
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - FILLQ Version 01.1                          ---
CC--- Subroutine description - interpolate plot points, insuring    ---
CC---                          that plot lines that cross plot      ---
CC---                          segment boundaries are correctly     ---
CC---                          aligned.                             ---
CC---------------------------------------------------------------------
CC
CC*********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
CC
      equivalence (ilinem(1),i),(ilinem(2),j),(ilinem(3),ilst),
     *            (ilinem(4),jlst),(ilinem(5),ipen),(ilinem(12),imaxc),
     *            (ilinem(13),imaxr)
CC
CC*********************************************************************
CC
      if (IABS(ipen) .eq. 2) then
        if (i .eq. ilst) then
          if (j .lt. jlst) then
            j1 = j
            j2 = jlst
          else
            j1 = jlst
            j2 = j
          end if
          do 10 jj=j1,j2
   10     call LBITQ(i,jj)
        else if (j .eq. jlst) then
          if (i .lt. ilst) then
            i1 = i
            i2 = ilst
          else
            i1 = ilst
            i2 = i
          end if
          do 20 ii=i1,i2
   20     call LBITQ(ii,j)
        else
          if (i .lt. ilst) then
            i1 = i
            i2 = ilst
          else
            i1 = ilst
            i2 = i
          end if
          a = FLOAT(j - jlst) / float(i - ilst)
          b = FLOAT(j) - i * float(j - jlst) / float(i - ilst)
          do 30 ii=i1,i2
          jj = ii * a + b + 0.5
          if (jj .lt. 1) jj = 1
          if (jj .gt. imaxr) jj = imaxr
   30     call LBITQ(ii,jj)
          if (ABS(a) .gt. 0.0) then
            if (j .lt. jlst) then
              j1 = j
              j2 = jlst
            else
              j1 = jlst
              j2 = j
            end if
            do 40 jj=j1,j2
            ii = (jj - b) / a
            if (ii .lt. 1) ii = 1
            if (ii .gt. imaxc) ii = imaxc
   40       call LBITQ(ii,jj)
          end if
        end if
      end if
      ilst = i
      jlst = j
CC
      return
      end
