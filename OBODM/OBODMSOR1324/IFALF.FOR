      Function IFALF(mchar)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFALF Version 01.0                          ---
CC--- Subroutine description - determine if mchar is upper or lower ---
CC---                          case alphabetic character.           ---
CC---------------------------------------------------------------------
CC
CC        mchar  - character*1 character to compare
CC
CC        returns - IFALF -
CC                  0 = not alphabetic
CC            1 to 26 = alphabetic sequence number of upper case
CC                      character.
CC          -1 to -26 = negative of alphabetic sequence number of lower
CC                      case character.
CC
CC********************************************************************
CC
      character*1 mchar
CC
CC********************************************************************
CC
      IFALF = 0
      do 10 i=1,26
        IFALF = IFCHR(mchar,i) * i
        if (IFALF .ne. 0) go to 20
   10 continue
   20 return
      end
