      Function IWTBF(mbfot,nchrs,value,ndecs,iflrj)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IWTBF Version 01.1                          ---
CC--- Subroutine description - write number value into character    ---
CC---                          string mbfot.                        ---
CC---------------------------------------------------------------------
CC
CC    mbfot - character string receiving numeric value.
CC            if value <= rmissc, then 'c' returned, right justified
CC            to indicate value is to be computed.
CC            if value > rmissc and <= rmissd, then blanks returned to
CC            indicate missing or no value.
CC            if an error occurs, 'error' is returned, right justified.
CC            otherwise the character form of the number, right
CC            justified is returned.
CC    nchrs - number of characters in mbfot.  if negative, then force
CC            exponential form with ndec-1 decimal digits.
CC    value - numeric value to place in string.
CC    ndecs - number digits to right of decimal point. for real numbers
CC            the number of digits is changed to maximize the number
CC            of significant digits.  if ndecs is less than or equal to
CC            zero an integer value is assumed.  if less than zero an
CC            integer i.i format is used.
CC    iflrj - left or right justify returned buffer.
CC            0 - right.
CC            1 - left.
CC
CC    returns - IWTBF
CC              0 - no errors.
CC             <0 - write error, 'error' returned in mbfot.
CC             >0 - number of characters in buffer, not including
CC                  trailing blanks if left justified and not including
CC                  leading blanks if right justified.
CC
CC**********************************************************************
CC
      character*1 mperd,mrprn,mfrmt,mbfot,mbf01,mzero,mblnk,mcchr,mfrmi,
     *            mast,mechr,mless,mchr
      character*6 merrs
      character*8 mfmti
      character*11 mfmts
      character*20 mbf20
CC
      dimension mbfot(100),mfrmt(11,3),mbf01(20),mfrmi(8)
CC
      equivalence (mbf20,mbf01(1)),(mfmti,mfrmi(1))
CC
      data mperd/'.'/,mrprn/')'/,mast/'*'/,mechr/'E'/,mless/'<'/,
     *     mfrmt/'(','f',9*' ','(','e',9*' ','(','1','p',',','e',6*' '/
     *     merrs/'error'/,mzero/'0'/,mblnk/' '/,rmissd/-1.0e20/,
     *     rmissc/-1.0e21/,mcchr/'C'/,
     *     mfrmi/'(','i','2','0',4*' '/
CC
CC********************************************************************
CC
      IWTBF = 0
      ichrs = IABS(nchrs)
      call MBLK1(mbfot,ichrs)
      ndec = ndecs
CC      see if field is computed.
   10 if (value .le. rmissc) then
CC        store 'c' in field.
        mbfot(ichrs) = mcchr
CC      see if field is not missing data.
      else if (value .gt. rmissd) then
        idecs = ndec
CC        if a non-zero real number.
        if ((ndec .gt. 0.and.ABS(value) .gt. 0.0).or.nchrs .lt. 0) then
          a = ABS(value)
          if (a .gt. 0.0) then
            b = ALOG10(a)
          else
            b = 0.0
          end if
          ifmts = 1
          if (b .ge. 0.0) then
CC            add for last digit and period.
            n1 =  b + 2
CC            add for minus sign.
            if (value .lt. 0.0) n1 = n1 + 1
            if (n1 .ge. ichrs) ifmts = 2
CC            max number of decimal digits.
            n2 = MAX0(ichrs-n1,0)
            if (n2 .lt. idecs.or.nchrs .lt. 0) then
CC              set exponent form.
              if ((n2 .le. idecs/2.and.ichrs .ge. 9).or. nchrs .lt. 0)
     *            then
                ifmts = 2
                if (nchrs .gt. 0) then
                  idecs = ichrs - 6
                else
                  idecs = ndecs
                end if
              else
                idecs = n2
              end if
            end if
          else
CC            leading character (period).
            n1 = 1
CC            add one for minus sign.
            if (value .lt. 0.0) n1 = n1 + 1
            n2 = ABS(b)
            if (n2 .lt. ABS(b)) n2 = n2 + 1
            if (n1+n2 .ge. ichrs) ifmts = 2
            if (n2 .gt. idecs) then
              if (ichrs .ge. 9) then
CC                set exponent form.
                ifmts = 2
                idecs = ichrs - 6
              else
                if (n1+n2 .le. ichrs) then
                  idecs = ichrs - n1
                end if
              end if
            else
              if (n1+2*n2 .le. ichrs) then
                n2 = 2 * n2
              else
                n2 = ichrs - n1
              end if
              if (n2 .gt. idecs) idecs = n2
            end if
          end if
        else
CC          if an integer or real number equal to zero.
          if (ndec .gt. 0) then
            ifmts = 1
            idecs = 1
          else
            a = ABS(value)
            if (a .gt. 0.0) then
              n = ALOG10(a) + 1.0
              if (value .lt. 0.0) n = n + 1
              if (n .gt. ichrs) then
                ndec = 1
                go to 10
              end if
            end if
            ifmts = 0
            if (ndec .lt. 0) then
              idecs = IABS(ndec)
            else
              idecs = 0
            end if
          end if
        end if
        if (ifmts .eq. 0) then
CC          integer format.
          i = value
          if (idecs .gt. 0) then
            mfrmi(5) = mrprn
            WRITE (mbf20,mfmti,iostat=ier,err=40) idecs
            mfrmi(5) = mperd
            j = MIN0(LRJST(mbf20,20,1),2)
            call mvch1(mbf20,mfrmi(6),j)
            mfrmi(6+j) = mrprn
          else
            mfrmi(5) = mrprn
          end if
          WRITE (mbf20,mfmti,iostat=ier,err=40) i
        else
CC          real format, ifmts = 1 is 'f' type, =2 is 'e' type.
          nchrp = ichrs
          n1 = 0
          i = 3
          if (ifmts .eq. 2) then
            idecs = MAX0(idecs,1)
            nchrp = MAX0(ichrs,9)
            if (idecs-1 .gt. 0.and.idecs-1+7 .le. ichrs) then
              n1 = 1
              idecs = MAX0(idecs - 1,1)
              i = 6
            end if
   20       if (6+n1+idecs .gt. ichrs.and.idecs .gt. 1) then
              idecs = idecs -1
              go to 20
            end if
          end if
          call MBLK1(mfmts,11)
CC          number of characters in field.
          mfrmi(5) = mrprn
          WRITE (mbf20,mfmti,iostat=ier,err=40) nchrp
          n = LRJST(mbf20,20,1)
          call MVCH1(mbf20,mfrmt(i,ifmts+n1),n)
          i = i + n
          mfrmt(i,ifmts+n1) = mperd
CC          number of characters after decimal point.
          WRITE (mbf20,mfmti,iostat=ier,err=40) idecs
          n = LRJST(mbf20,20,1)
          call MVCH1(mbf20,mfrmt(i+1,ifmts+n1),n)
          mfrmt(i+n+1,ifmts+n1) = mrprn
          call MVCH1(mfrmt(1,ifmts+n1),mfmts,i+n+1)
CC          write value to buffer.
          call MBLK1(mbf20,20)
          WRITE (mbf20,mfmts,iostat=ier,err=40) value
        end if
        if (IFCMP(mbf20,20,mast,1,0) .gt. 0) go to 50
        i = LRJST(mbf20,20,0)
CC        remove excess zeros from end of number.
CC        if period in buffer.
        j = IFCMP(mbf20,20,mperd,1,0)
CC        if period not at start or end.
        if (j .gt. 0.and.j .lt. 19) then
CC          see if exponental notation.
          n1 = IFCMP(mbf20,20,mechr,1,1)
          if (n1 .le. 0) n1 = 21
          i = n1
          mchr = mless
          n = 0
   30     if (i .gt. j.and.n .eq. 0) then
            i = i - 1
            if (mbf01(i) .eq. mzero.and.mbf01(i-1) .eq. mzero) then
              mbf01(i) = mchr
              mchr = mblnk
            else
              n = 1
            end if
            go to 30
          end if
        end if
        i = MAX0(JPACK(mbf20,20),1)
        if (j .gt. 0.and.j .lt. 19.and.n1 .eq. 21) then
          j = IFCMP(mbf20,i,mperd,1,0)
          if (j .lt. i-1) then
            if (mbf01(i) .eq. mzero) then
              mbf01(i) = mblnk
              i = i -1
            end if
          end if
        end if
        call MVCH1(mbf20,mbfot,MIN0(i,ichrs))
      end if
      go to 60
   40 IWTBF = -ier
   50 call MVCH1(merrs,mbfot,5)
   60 n = LRJST(mbfot,ichrs,1)
      i = LRJST(mbfot,ichrs,iflrj)
      if (IWTBF .eq. 0) IWTBF = n
      return
      end
