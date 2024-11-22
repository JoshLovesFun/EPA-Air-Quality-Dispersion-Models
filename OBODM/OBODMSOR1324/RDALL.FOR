CCPCITS
      interface to Subroutine GETML[C](ix,iy)
      integer*2 ix[REFERENCE],iy[REFERENCE]
      end
      interface to Subroutine SETML[C](ix,iy)
      integer*2 ix, iy
      end
      interface to integer*2 Function GETCL[C](ibuttn,ix,iy)
      integer*2 ibuttn,ix[REFERENCE],iy[REFERENCE]
      end
      interface to Subroutine WAITM[C](imsecl,imsech)
      integer*2 imsecl,imsech
      end
      interface to Subroutine RDSTA[C](iflag1,iflag2,iflag3)
      integer*2 iflag1[REFERENCE],iflag2[REFERENCE],iflag3[REFERENCE]
      end
      Subroutine RDALL(iext,icha,ix,iy,icols,icole,irow)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RDALL Version 01.00                         ---
CC--- Subroutine description - used to determine which keyboard key ---
CC---                          has been typed.                      ---
CC---------------------------------------------------------------------
CC
CC    iext - index indicating whether type key is normal or from
CC           the extended code.
CC           0 - extended.
CC           1 - normal.
CC    icha - ascii character number.
CC    ix   - mouse horizontal position.
CC    iy   - mouse vertical position.
CC
CC*********************************************************************
CC
      integer*2 iext,icha,ix,iy,GETCL,iflms,iflg1,iflg2,iflg3,icols,
     *          icole,irow
CC
      include 'OCNTRL.INC'
CC
CC*********************************************************************
CC
      if (ifmous .ne. 0) then
        call SETML(locxms,locyms)
        icha = 0
        iext = 0
      end if
CC      check for interupt Ctrl C.
   10 if (icols .ge. 0) then
        if (ifctlc .eq. 1) then
          if (KFCTC(icols,icole,irow) .ne. 0) then
            iext = nkeybd(1,2)
            icha = nkeybd(2,2)
            ifctlc = -1
            go to 30
          end if
        end if
      else
        ifctlc = 0
      end if
CC      check for character in keyboard buffer.
      call RDSTA(iflg1,iflg2,iflg3)
      if (iflg1 .ne. 0.or.iflg2 .ne. nochar) then                       020797
CC>     The values 12870 and 29254 are for a 486 PC DOS and windows     020797
CC>     mode, respectively.                                             020797
CC>     The values 12994 and 29378 are for a 386 PC DOS and Windows     020797
CC>     respectively                                                    020797
CC>   if (iflg1 .ne. 0.or.(iflg2 .ne. 12870.and.iflg2 .ne. 29254.and.   020797
CC>  *    iflg2 .ne. 12994.and.iflg2 .ne. 29378)) then                  020797
CC                                                                      020797
CC        read character from keyboard buffer.
        call QINKY(iext,icha)
        if (ifmous .ne. 0) then
          call GETML(ix,iy)
          locxms = ix
          locyms = iy
        end if
        go to 20
      end if
CC
      if (ifmous .ne. 0) then
        iflms = GETCL(0,ix,iy)
CC
        if (iflms .eq. 1) then
          icha = 257
          call WAITM(imsecl,imsech)
          iflms = GETCL(0,ix,iy)
          if (iflms .ge. 1) icha = 258
          locxms = ix
          locyms = iy
          go to 20
        end if
      end if
CC    
      go to 10
CC
   20 if (ifmous .ne. 0) then
        ix = ix / imxscl
        iy = iy / imyscl
      end if
CC
   30 return
      end
CCPCITE   
