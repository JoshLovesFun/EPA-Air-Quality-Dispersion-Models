CCPCITS
      interface to integer*2 Function GETFL[C](mpath,mfnam,nfils)
      character*50 mpath[REFERENCE]
      character*22 mfnam(50)
      integer*2 nfils[REFERENCE]
      end
CC
      Function IFPTH(mdirc,mfnam)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFPTH Version 01.1                          ---
CC--- Subroutine description - see if path name is a valid path.    ---
CC---------------------------------------------------------------------
CC
CC    mdirc - name of directory to scan.
CC    mfnam - dumy large buffer.
CC
CC    returns - IFPTH - 0 - if not valid or empty directory.
CC                     >0 - if valid directory with files.
CC
CC*********************************************************************
CC
      integer*2 iret,nfils,GETFL
      character*3 mext
      character*22 mfnam
      character*36 mdirc
      character*50 mpath
CC
      dimension mfnam(50)
CC
      data mext/'*.*'/
CC
CC*********************************************************************
CC
      IFPTH = 0
CC
      n = NUMC1(mdirc,36)
CC
CC      concatenate directory name with *.*
      if (mdirc(n:n) .eq. '\') then
        mpath = mdirc(1:n)//mext
      else
        mpath = mdirc(1:n)//'\'//mext
      end if
CC
CC      get file names.
      iret = GETFL(mpath,mfnam,nfils)
      if (nfils .gt. 0) IFPTH = nfils
      return
      end
CCPCITE
