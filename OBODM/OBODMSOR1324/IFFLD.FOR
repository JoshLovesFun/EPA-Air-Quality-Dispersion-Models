CCPCITS
      Function IFFLD(ixp,iyp,iline,itabs,ispac,iret,jret)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFFLD Version 01.1                          ---
CC--- Subroutine description - determine if cursor is in data field.---
CC---------------------------------------------------------------------
CC
CC    ixp   - cursor current column number (0-79).
CC    iyp   - cursor current line number (0-24).
CC    iline - array giving the line numbers for lines in mscrn that
CC            contain data fields. if iline(k) > 0, then the line
CC            starting at mscrn(1,iline(k)) contains data fields,
CC            where k = 1 to 20 (maximum number of lines that can
CC            contain data fields = 20).
CC    itabs - array giving the start columns of each data field on the
CC            line given by iline(k). the data field starts at
CC            mscrn(itabs(k,l),iline(k)) for the l'th field on the line,
CC            where l = 1 to 10 (maximum number of data fields per
CC            line = 10).
CC    ispac - array giving the number of columns in each data field.
CC            the l'th data field ends at mscrn(itabs(k,l)+ispac(k,l)-1,
CC            iline(k))
CC    iret  - index of data field cursor is in.
CC    jret  - index of data line cursor is in.
CC
CC    returns IFFLD = 0 , no
CC                    1 , yes
CC
CC**********************************************************************
CC
      integer*2 ixp,iyp
CC
      dimension iline(20),itabs(20,10),ispac(20,10)
CC
CC**********************************************************************
CC
      xp = ixp + 1
      yp = iyp + 1
      IFFLD = 0
      j = 0
      do 10 k=1,20
CC        see if row has data field(s).
        if (iline(k) .eq. iyp+1) then
          j = k
          go to 20
        end if
   10 continue
   20 if (j .ne. 0) then
CC        start row of data field.
        ystrt = iline(j)
CC        end row of data field.
        yend = iline(j)
        do 30 i=1,10
          if (itabs(j,i) .gt. 0) then
CC            column number of start of data field.
            xstrt = itabs(j,i)
CC            column number of end of data field.
            xend = xstrt + ispac(j,i) - 1
CC            see if xp,yp is in box.
            IFFLD = IFBXQ(xp,yp,xstrt,xend,ystrt,yend)
            if(IFFLD .eq. 1) then
              iret = i
              jret = j
              return
            end if
          end if
   30   continue
      end if
      return
      end
CCPCITE
