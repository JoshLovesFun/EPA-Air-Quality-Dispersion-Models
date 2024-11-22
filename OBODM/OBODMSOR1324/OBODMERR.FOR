      Program OBODMERR
CC
CC---------------------------------------------------------------------
CC--- AUTHOR ORGANIZATION- H. E. CRAMER CO., INC.                   ---
CC--- CODE - OBODMERR VERSION 01.00                                 ---
CC--- DESCRIPTION - creates OBODM error/warning file OBODMERR.OBD   ---
CC---               from file OBODMERR.DAT                          ---
CC---------------------------------------------------------------------
CC
CC*********************************************************************
CC
      character*1 mdumy,mblnk
CCPCITS
      character*1 mgrt,mequ,mgre,mesi
CCPCITE
      character*22 mnmin,mnmot,mname
      character*40 mess
CC
      dimension ival(8),linen(8),jchrs(8),nchrs(8),ndec(8),ntrec(100)
CCPCITS
      dimension mesi(40)
CC
      equivalence (mess,mesi(1))
CCPCITE
CC
      data mnmin/'OBODMERR.DAT'/,mnmot/'OBODMERR.OBD'/,ival/8*0/
     *     linen/8*0/,jchrs/8*0/,nchrs/8*0/,ndec/8*0/,mblnk/' '/
CCPCITS
      data mgrt/'>'/,mequ/'='/,mgre/'>'/
CCPCITE
CC
CC**********************************************************************
CC
CCPCITS
      mgre = char(242)
CCPCITE
      write (*,9006)
      mdumy = mblnk
      read (*,9007) mdumy
      if (ifchr(mdumy,25) .ne. 0) then
        ifprt = 1
      else
        ifprt = 0
      end if
      if (ifprt .ne. 0) then
        write (*,9008)
        mname = mblnk
        read (*,9009) mname
        if (mname .ne. mblnk) then
          iptfil = 12
          open (unit=iptfil,file=mname,status='unknown')
        else
          ifprt = 0
        end if
      end if
      nmess = 0
      irec = 0
      inpfil = 20
CC      open input error message file.
      OPEN (unit=inpfil,file=mnmin,iostat=ier,err=50,status='old')
      iotfil = 21
CC      open output error message file.
      OPEN (unit=iotfil,file=mnmot,iostat=ier,err=50,status='unknown',
     *     form='unformatted',access='direct',recl=40)
CC
CC
CC      read the number of error messages in the input file.
      irec = irec + 1
      read (inpfil,*,iostat=ier,err=60) num
      if (ifprt .ne. 0) then
        write (iptfil,9010) num
      end if
      n = 0
      iof = 0
   10 n = n + 1
      if (n .le. num) then
CC        read each error message control block.
CC          nrec - number of text lines in error message.
CC          irsp - user response required for the message.
CC                 1 - OK
CC                 2 - N/Y
CC                 3 - value
CC          linen - line number in message in which to place alpha or
CC                  numberic information.  linen(1) is for alpha info.
CC                  linen(2) to linen(6) are for numeric info.
CC          jchrs - starting character in line specified by linen.
CC          nchrs - number of characters provided for info in line
CC                  specified by linen.
CC          ndec  - number of decimal points for numberic info in line
CC                  specified by linen.
CC
        irec = irec + 1
        read (inpfil,*,iostat=ier,err=60) nrec,irsp,(linen(i),
     *                                    jchrs(i),nchrs(i),ndec(i),
     *                                    i=1,7)
        if (ifprt .ne. 0) then
          write (iptfil,9011) n,nrec,irsp,(linen(i),jchrs(i),nchrs(i),
     *                        ndec(i),i=1,7)
          ntrec(n) = nrec
        end if
        jrec = nrec * 100 + irsp
CC        compress values into fewer words.
        do 20 i=1,8
          if (ndec(i) .ge. 0) then
            is = 1
          else
            is = -1
            ndec(i) = IABS(ndec(i))
          end if
          ival(i) = linen(i) * 100000 + jchrs(i) * 1000 + nchrs(i) * 10
     *              + ndec(i)
          ival(i) = ival(i) * is
   20   continue
        ipntr = num + iof + 1
        norec = n
        write (iotfil,rec=norec,iostat=ier,err=70) ipntr,jrec,
     *                                             (ival(i),i=1,8)
        iof = iof + nrec
        nmess = nmess + nrec
        go to 10
      end if
      norec = num
      n = 1
      l = 0
      ll = 0
   30 irec = irec + 1
      ll = ll + 1
      read (inpfil,9000,iostat=ier,err=60,end=80) mess
      if (ll .gt. nmess) then
        if (mess .eq. mblnk) go to 80
      end if
CCPCITS
      do 40 i=2,39
        if (mesi(i) .eq. mgrt) then
          if (mesi(i+1) .eq. mequ) then
            mesi(i) = mgre
            mesi(i+1) = mblnk
          end if
        end if
   40 continue
CCPCITE
      norec = norec + 1
      write (iotfil,rec=norec,iostat=ier,err=70) mess
      if (ifprt .ne. 0) then
        l = l + 1
        if (l .gt. ntrec(n)) then
          n = n + 1
          l = 1
        end if
        if (l .eq. 1) then
          write (iptfil,9012) n,mess
        else
          write (iptfil,9013) mess
        end if
      end if
      go to 30
   50 write (*,9002) ier,mnmin
      go to 90
   60 write (*,9003) ier,irec,mnmin
      go to 90
   70 write (*,9004) ier,norec,mnmot
      go to 90
   80 write (*,9001) irec-2,norec
CC      norec - total number of records written.      
CC      num   - number of control records read.      
CC      nmess - number of message records written.
CC      norec-num should equal nmess.      
      if (norec - num .ne. nmess) then
        write (*,9005) norec-num,nmess
      end if
   90 stop
CC
 9000 format (a40)
 9001 format (' records read    = ',i6/
     *        ' records written = ',i6)
 9002 format (' Open error ',i6,' on file ',a22)
 9003 format (' Read error ',i6,' at block ',i6,' on file ',a22)
 9004 format (' Write error ',i6,' at block ',i6,' on file ',a22)
 9005 format (' Warning, number of message lines written (',i6,
     *') does not match required (',i6,')')
CCOMP1S
c9006 format (/' Program OBODMERR'//' Print list of error/warning messag
c    *es? (N/Y) >',$)
CCOMP1E
CCPCITS
 9006 format (/' Program OBODMERR'//' Print list of error/warning messag
     *es? (N/Y) >',\)
CCPCITE
 9007 format (a1)
CCOMP1S
c9008 format (/' Enter - Print output file name. > ',$)
CCOMP1E
CCPCITS
 9008 format (/' Enter - Print output file name. > ',\)
CCPCITE
 9009 format (a22)
 9010 format (' OBODM error/warning messages (file OBODMERR.OBD)'/
     *' Total number of messages = ',i4/)
 9011 format (1x,i3,'-',1x,i3,',',i1,',',4(1x,4(i3,','))/6x,3(1x,4(1x,i3
     *,',')))
 9012 format (1x,i3,'-',1x,a40)
 9013 format (6x,a40)
      end
      Function IFCHR(mchar,nchr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFCHR Version 01.02                         ---
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
