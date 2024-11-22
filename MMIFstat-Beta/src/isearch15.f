      integer function isearch15(list,nlist,string)

      implicit none
      integer nlist,i
      character*15 list(nlist),string

      do i = 1, nlist
        if (string .eq. list(i)) then
          isearch15 = i
          return
        end if
      end do
      isearch15 = 0
      return
      end function isearch15

