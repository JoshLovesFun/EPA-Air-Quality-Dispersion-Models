CCPCITS
      interface to integer*2 Function GETFL[C](mpath,mfnam,nfils)
      character*50 mpath[REFERENCE]
      character*22 mfnam(50)
      integer*2 nfils[REFERENCE]
      end
CC
      Subroutine FILES(ioper,mdire,mfnam,numfl,mext)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - FILES Version 01.1                          ---
CC--- Subroutine description - get OBODM data file names, depending ---
CC---                          on extension mext.                   ---
CC---------------------------------------------------------------------
CC
CC    ioper - if = 0, scan current directory for file names.
CC            if = 1, scan directory given by mdire for file names.
CC    mdire - name of directory to scan, if ioper = 1.
CC    mfnam - array of returned file names with extension mext.
CC    numfl - number of file names returned.
CC    mext  - extension for desired file names.
CC
CC*********************************************************************
CC
      integer*2 iret,nfils,GETFL
      character*1 mblnk,mperd
      character*4 mext
      character*22 mfnam
      character*36 mdire
      character*50 mpath
CC
      dimension mfnam(50)
CC
      data mblnk/' '/,mperd/'.'/
CC
CC*********************************************************************
CC
      mpath = mblnk
CC
      if (ioper .eq. 0) then
CC        concatenate * with specified extension name.
        mpath = '*'//mext
      else
CC
        nend = NUMC1(mdire,36)
CC
CC        concatenate directory name with * and extension name.
        if (mdire(nend:nend) .eq. '\') then
          mpath = mdire(1:nend)//'*'//mext
        else
          mpath = mdire(1:nend)//'\*'//mext
        end if
      end if
CC
CC      get file names.
      iret = GETFL(mpath,mfnam,nfils)
      numfl = nfils
      if (numfl .gt. 0) then
        do 10 n=1,numfl
          i = LRJST(mfnam(n),22,1)
          i = IFCMP(mfnam(n),22,mperd,1,0)
          if (i+3 .lt. 22) then
            mfnam(n)(i+4:22) = mblnk
          end if
          i = LRJST(mfnam(n),22,0)
   10   continue
      end if
      return
      end
CCPCITE
