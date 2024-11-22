      Function IFCHR(mchar,nchr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFCHR Version 01.0                          ---
CC--- Subroutine description - determine if mchar is an upper or    ---
CC---                          lower case character equal to the    ---
CC---                          nchr'th character in the alphabet.   ---
CC---                                         or                    ---
CC---                          return in mchar the alpha character  ---
CC---                          with index equal to the absolute     ---
CC---                          value of nchr.                       ---
CC---------------------------------------------------------------------
CC
CC        mchar  - character*1 character to compare or return.
CC        nchr   - sequence number of character to compare against.
CC                 > 0 & < 27,  then compare mchar to upper and lower
CC                              case characters corresponding to
CC                              sequence number nchr.
CC                 > -53 & < 0, then return the character with sequence
CC                              given by the absolute value of nchr.
CC                              -1 to -26 is a to z,
CC                             -27 to -52 is a to z.
CC
CC        returns - IFCHR -
CC                  if nchr > 0 -
CC                                0 = no compare.
CC                                1 = yes compare upper case.
CC                               -1 = yes compare lower case.
CC                  if nchr < 0 -
CC                                0 = nchr out of range no character
CC                                    returned.
CC                             nchr = character determined by nchr is
CC                                    returned in mchar.
CC
CC********************************************************************
CC
      character*1 malph,mchar
CC
      dimension malph(52)
CC
      data malph/'A','B','C','D','E','F','G','H','I','J','K','L','M',
     *           'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
     *           'a','b','c','d','e','f','g','h','i','j','k','l','m',
     *           'n','o','p','q','r','s','t','u','v','w','x','y','z'/
CC
CC********************************************************************
CC
      if (nchr .gt. 0.and.nchr .lt. 53) then
        if (nchr .lt. 27) then
          i = 26
        else
          i = -26
        end if
        if (mchar .eq. malph(nchr)) then
          i = nchr
          IFCHR = 1
        else if (mchar .eq. malph(nchr+i)) then
          i = nchr + i
          IFCHR = 1
        else
          IFCHR = 0
        end if
        if (i .gt. 26) IFCHR = -IFCHR
      else if (IABS(nchr) .gt. 0.and.IABS(nchr) .lt. 53) then
        IFCHR = nchr
        mchar = malph(IABS(IFCHR))
      else
        IFCHR = 0
      end if
      return
      end
