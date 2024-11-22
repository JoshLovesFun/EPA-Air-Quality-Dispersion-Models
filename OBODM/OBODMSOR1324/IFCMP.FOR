      Function IFCMP(mary1,nchr1,mary2,nchr2,iflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFCMP Version 01.0                          ---
CC--- Subroutine description - compare two character strings to see ---
CC---                          if the second is embedded within the ---
CC---                          first.                               ---
CC---------------------------------------------------------------------
CC
CC        mary1 - 1st character string to compare.
CC        nchr1 - number of characters in mary1.
CC        mary2 - 2nd character string to compare, routine looks
CC                for this string within mary1.
CC        nchr2 - number of characters in maray2.
CC        iflg  - upper/lower case flag.
CC                0 - compare as is, do not look for upper/lower case.
CC                1 - if alphabetic, check both upper and lower case.
CC
CC        returns - IFCMP
CC                  0 - string mary2 not found in mary1.
CC                 >0 - character number of start of string mary2
CC                      in mary1.
CC
CC********************************************************************
CC
      integer*2 i,j,k,n
CC
      character*1 mary1,mary2,m1,m2
CC
      dimension mary1(500),mary2(500)
CC
CC********************************************************************
CC
      IFCMP = 0
      j = 0
CC      set max. no. of characters to check in first array.
      n = nchr1 - nchr2 + 1
CC      loop over first array.
   10 if (j .lt. n.and.IFCMP .eq. 0) then
        j = j + 1
        i = 0
        k = j
CC        loop over second array.
   20   if (i .lt. nchr2) then
          i = i + 1
CC          set characters to compare.
          m1 = mary1(k)
          m2 = mary2(i)
CC          if upper/lower case sensitive.
          if (iflg .ne. 0) then
CC            numeric value of characters. <0=lower case, >0=upper.
            l1 = IFALF(m1)
            l2 = IFALF(m2)
CC            if both characters are alphabetic.
            if (l1 .ne. 0.and.l2 .ne. 0) then
CC              if lower case, change to upper case.
              if (l1 .lt. 0) then
                l1 = IFCHR(m1,l1)
              end if
CC              if lower case, change to upper case.
              if (l2 .lt. 0) then
                l2 = IFCHR(m2,l2)
              end if
            end if
          end if
CC          compare characters.
          if (m1 .ne. m2) then
CC            not the same, end second loop.
            i = nchr2
          else
CC            same, if last of second array, set start character of
CC            found string.
            if (i .eq. nchr2) then
              IFCMP = j
              i = nchr2
            else if (k .lt. nchr1) then
CC              not last character, increment first array index.
              k = k + 1
            else
CC              first array exceeded.
              i = nchr2
            end if
          end if
          go to 20
        end if
        go to 10
      end if
      return
      end
