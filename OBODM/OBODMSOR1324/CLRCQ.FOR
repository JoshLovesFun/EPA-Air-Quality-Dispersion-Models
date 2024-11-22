      Subroutine CLRCQ(jflg,joptn,ioptn)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CLRCQ Version 01.1                          ---
CC--- Subroutine description - check color options entered and      ---
CC---                          default color options and set up the ---
CC---                          selection of color options displayed ---
CC---                          on the terminal.
CC---------------------------------------------------------------------
CC
CC        jflg - index to plot type and color parameter
CC              >0 - set default color and color options
CC              <0 - check entered color code against possible
CC       joptn - entered or default color option
CC       ioptn - array of flags used to indicate which color options
CC               are to be displayed on the terminal.
CC
CC*********************************************************************
CC
      integer*2 i,j,nclr,iclr
CC
      dimension ioptn(16),iclr(3)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
      save nclr,iclr
CC
CC*********************************************************************
CC
      if (jflg .lt. 0) then
        if (IABS(jflg) .eq. 1) then
CC          check all color settings against entered background color.
          iswplt(16) = IFBKQ(iswplt(16),iswplt(9),ipmode)
          iswplt(12) = IFBKQ(iswplt(12),iswplt(9),ipmode)
          iswplt(23) = IFBKQ(iswplt(23),iswplt(9),ipmode)
          if (nspcls .gt. 0) then
            do 10 i=1,nspcls
              ispclr(i) = IFBKQ(ispclr(i),iswplt(9),ipmode)
   10       continue
          end if
          if (IANYI(ipoptn,npoptn,3) .ne. 0.or.IANYI(ipoptn,npoptn,4)
     *        .ne. 0) then
            do 30 icontr=1,2
              if (ncontr(icontr) .gt. 0) then
                do 20 i=1,ncontr(icontr)
                  icnclr(i,icontr) = IFBKQ(icnclr(i,icontr),iswplt(9),
     *                                     ipmode)
   20           continue
              end if
   30       continue
          end if
CC          set number of colors selected to zero.
          nclr = 0
        else
          if (ipmode .lt. 16) then
CC            accumulate different colors selected for PCIT.
            j = 0
            if (nclr .gt. 0) then
CC              see if color already selected.
              i = 0
   40         if (i .lt. nclr.and.j .eq. 0) then
                i = i + 1
                if (iclr(i) .eq. joptn) then
                  j = 1
                end if
                go to 40
              end if
            end if
            if (j .eq. 0) then
CC              new color.
              nclr = nclr + 1
              if (nclr .le. 3) then
CC                a maximum of 3 colors can be selected,
CC                screen 1, 2 and 3.
                iclr(nclr) = joptn
                iswplt(12+nclr) = joptn
              else
CC                too many colors selected, change to color of screen 1.
                nclr = 3
                joptn = iclr(1)
              end if
            end if
          end if
        end if
      else
        if (jflg .ne. 1) then
CC          make sure color option is not equal to background color.
          joptn = IFBKQ(joptn,iswplt(9),ipmode)
CC          if PCIT terminal restrict colors a maximum of 3
CC          other than the background color.
          if (ipmode .lt. 16) then
            if (nclr .ge. 3) then
CC              maximum number of colors, only list these as options.
              i = 0
              j = 0
   50         if (i .lt. nclr.and.j .eq. 0) then
                i = i + 1
                if (joptn .eq. iclr(i)) then
                  j = 1
                end if
                go to 50
              end if
              if (j .eq. 0) then
CC                change default to color of screen 1.
                joptn = iclr(1)
              end if
CC              set options to the 3 screen colors.
              i = JINIT(ioptn,16,0,0)
              do 60 i=1,nclr
                ioptn(iclr(i)+1) = 1
   60         continue
            end if
          end if
          ioptn(iswplt(9)+1) = 0
        end if
      end if
      return
      end
