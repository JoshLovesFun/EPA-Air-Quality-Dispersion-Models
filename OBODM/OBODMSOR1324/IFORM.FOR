      Function IFORM(mform,nchrs,nchfd,nvlls)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFORM Version 01.2                          ---
CC--- Subroutine description - checks input format data for errors  ---
CC---------------------------------------------------------------------
CC
CC    mform - array of FORTRAN format edit codes.
CC    nchrs - length in characters of the array mform.
CC    nchfd - number of characters in data field.  more than one data
CC            field can be used for mform, but all data fields are the
CC            same length.
CC    nvlls - data line number of data field.
CC
CC    returns - IFORM - 0 = no errors or problems.
CC                      1 = no errors, but format modified.
CC                      2 = errors found.
CC
CC**********************************************************************
CC
      character*1 mform,mlprn,mrprn,mcoma,mslsh,mperd,mzero,mnine
CC
      dimension mform(150)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      data mlprn/'('/,mrprn/')'/,mcoma/','/,mslsh/'/'/,mperd/'.'/,
     *     mzero/'0'/,mnine/'9'/
CC
CC********************************************************************
CC
      IFORM = 0
      n = LRJST(mform,nchrs,1)
CC      check for left parenthesis.
      if (mform(1) .ne. mlprn) then
        if (n .ge. nchrs) then
          iferrm(nvlls,1) = 1
          IFORM = 2
          go to 40
        end if
CC        shift to the right.
        IFORM = 1
        n = n + 1
        do 10 j=1,n-1
          mform(n-j+1) = mform(n-j)
   10   continue
        mform(1) = mlprn
      end if
CC      check for right parenthesis.
      if (mform(n) .ne. mrprn) then
        if (n .ge. nchrs) then
          iferrm(nvlls,1) = 1
          IFORM = 1
          go to 40
        end if
        if (IFORM .ne. 2) IFORM = 1
        mform(n+1) = mrprn
        n = n + 1
      end if
      j = 0
CC      check for imbedded blanks.
      do 20 i=1,n
        if (mform(i) .ne. mblnk1) then
          j = j + 1
          mform(j) = mform(i)
        end if
   20 continue
      if (j .ne. n.and.IFORM .ne. 2) IFORM = 1
      n = j
      if (n .lt. nchrs) then
        call MBLK1(mform(n+1),nchrs-n)
      end if
CC      check for strange characters in format.
      j = 0
      ier = 0
CC      loop over characters entered.
   30 if (j .lt. n) then
        j = j + 1
        ier = 0
CC        get character index.
        i = IFALF(mform(j))
        if (i .ne. 0) then
CC          if b.
          if (IABS(i) .eq. 2) then
            if (j .lt. n) then
              j = j + 1
              m = IFALF(mform(j))
CC              if b not followed by n or z, then error.
              if (m .ne. 14.and.m .ne. 26) then
                ier = j
              end if
            else
              ier = j
            end if
CC          if t.
          else if (IABS(i) .eq. 20) then
            if (j .lt. n) then
              j = j + 1
              m = IFALF(mform(j))
CC              if t not followed by l, r or non-alpha then
CC              error.
              if (IABS(m) .ne. 12.and.IABS(m) .ne. 18.and.m .ne. 0)
     *            then
                ier = j
              end if
            else
              ier = j
            end if
CC          if not e, f, g, p or x, then error.
          else if (IABS(i) .ne. 5.and.IABS(i) .ne. 6.and.
     *             IABS(i) .ne. 7.and.IABS(i) .ne. 16.and.
     *             IABS(i) .ne. 24) then
            ier = j
          end if
        else
CC          non-alphabetic. if not comma, left paren or right
CC          paren.
          if (mform(j) .ne. mcoma.and.mform(j) .ne. mlprn.and.
     *        mform(j) .ne. mrprn.and.mform(j) .ne. mslsh.and.
     *        mform(j) .ne. mperd) then
CC            check for number.
            if (mform(j) .lt. mzero.or.mform(j) .gt. mnine) then
CC              not a number.
              ier = j
            else
CC              numeric.
              ier = 0
            end if
          end if
        end if
        if (ier .gt. 0) then
          IFORM = 2
CC          error, offending character is ier'th.
          if (ier .le. nchfd.and.nchrs .ge. 150) then
            iferrm(nvlls-1,1) = 1
          else
            iferrm(nvlls,1) = 1
          end if
        end if
        go to 30
      end if
   40 return
      end
