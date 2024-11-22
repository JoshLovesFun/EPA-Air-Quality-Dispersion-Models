      Subroutine FNAME(mfnam,num,mdire,mname)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - FNAME Version 01.1                          ---
CC--- Subroutine description - return character array containing    ---
CC---                          directory and file name or file name ---
CC---                          only.                                ---
CC---------------------------------------------------------------------
CC
CC    mfnam  - character array disc, directory, file name returned in.
CC    num    - maximum number of characters in mfnam.
CC    mdire  - character array giving the directory path name.
CC    mname  - character array giving file name. if preceded by an *
CC             mnane is appended to mdire and returned in mfnam.  if
CC             not mname alone is returned in mfnam.
CC
CC**********************************************************************
CC
      character*1 mfnam,mdire,mname,mastk,mslsb,mslsh,mdumy
CC
      dimension mfnam(58),mname(22),mdire(36)
CC
      include 'OCNTRL.INC'
CC
      data mastk/'*'/,mslsb/'\'/,mslsh/'/'/
CC
CC**********************************************************************
CC
      call MBLK1(mfnam,num)
CC      left justify directory name.
      i = LRJST(mdire,36,1)
CC      get number of characters in directory name.
      n = NUMC1(mdire,36)
CC      left justify file name.
      i = LRJST(mname,22,1)
CC      if directory name non-blank.
      if (n .gt. 0) then
CC        directory name non-blank.
        if (mname(1) .eq. mastk) then
CC          append file name to end of directory name.
          call MVCH1(mdire,mfnam,n)
          if (isystm .eq. 1) then
            mdumy = mslsh
          else
            mdumy = mslsb
          end if
          if (mfnam(n) .ne. mdumy) then
            mfnam(n+1) = mdumy
            n = n + 1
          end if
          call MVCH1(mname(2),mfnam(n+1),21)
        else
CC          return file name only.
          call MVCH1(mname,mfnam,22)
        end if
      else
CC        directory name is blank.
        if (mname(1) .eq. mastk) then
CC          file name preceded by an *.
          call MVCH1(mname(2),mfnam,21)
        else
          call MVCH1(mname,mfnam,22)
        end if
      end if
      return
      end
