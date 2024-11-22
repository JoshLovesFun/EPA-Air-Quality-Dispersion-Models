      Subroutine RLABL(indxs,jndxs,nhrrs,iflg)                          120698
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RLABL Version 01.2                          ---
CC--- Subroutine description - load labels into array mscrch for    ---
CC---                          printing or plotting.                ---
CC---------------------------------------------------------------------
CC
CC        mscrch(ilbstr(1)) to mscrch(ilbstr(1)+nlbsch(1)-1) contains
CC        the name of the quantity being calculated, including units and
CC        calculation height if z > 0.0.
CC        mscrch(ilbstr(2)) to mscrch(ilbstr(2)+nlbsch(2)-1) contains
CC        the units of the x receptors.
CC        mscrch(ilbstr(3)) to mscrch(ilbstr(3)+nlbsch(3)-1) contains
CC        the units of the y receptors.
CC        mscrch(ilbstr(4)) to mscrch(ilbstr(4)+nlbsch(4)-1) contains -
CC        'dosage', 'concentration' or 'deposition', depending on
CC        the quantity being printed.
CC        mscrch(ilbstr(5)) to mscrch(ilbstr(5)+nlbsch(5)-1) contains -
CC        calculation height if z > 0.0.
CC
CC        indxs  - type of average or total calculation, where n is
CC                 given by nhrrs.
CC                 0 - non-hourly data.
CC                 1 - n-hour average or total.
CC                 2 - season-n average or total.
CC                 3 - quarter-n average or total.
CC                 4 - annual average or total.
CC                 5 - highest nhrrs-hour average.
CC                 6 - second highest nhrrs-hour average.
CC        jndxs  - index of variable being printed.
CC                 1 - dosage
CC                 2 - concentration
CC                 3 - time-average concentration
CC                 4 - gravitational deposition
CC        nhrrs  - number of hours in average or total for indxs equal
CC                 to 1 or season or quarter number for indxs equal to
CC                 2 or 3.
CC        iflg   - Flag used to indicate whether call is from print
CC                 routines or graphics routines.
CC                 0 - print
CC                 1 - graphics
CC
CC**********************************************************************
CC
      character*1 mlprn,mrprn,mslsh,mless,mgrtr,mbufr,mlbrc,mrbrc
      character*3 munit
      character*4 mpeak
      character*5 mtotl
      character*6 manul
      character*7 mavrg,msean
      character*17 mhigh
      character*44 mbufs                                                020797
CC
      dimension mbufr(44),munit(3),mhigh(2),msean(4)                    020797
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      equivalence (mbufr(1),mbufs)
CC
      data mlprn/'('/,mrprn/')'/,mslsh/'/'/,mless/'<'/,mgrtr/'>'/,
     *     mavrg/'Average'/,mtotl/'Total'/,manul/'Annual'/,
     *     mpeak/'Peak'/,mlbrc/'{'/,mrbrc/'}'/,munit/'sec','min','hr '/,
     *     mhigh/'Highest >        ','Second Highest > '/,
     *     msean/'Winter>','Spring>','Summer>','Fall>'/
CC
CC**********************************************************************
CC
      call MBLK1(mscrch,180)
CC
CC      construct main table heading.
      n = 1
      if (indxs .gt. 0) then
        if (indxs .eq. 5.or.indxs .eq. 6) then
          call mvch1(mhigh(indxs-4),mscrch(n),16)
          n = n + 16
        end if
        if (indxs .eq. 1.or.indxs .eq. 5.or.indxs .eq. 6) then
          if (jndxs .eq. 3) then
            if (iflg .eq. 0) then                                       120698
              tim = timavg                                              120698
            else                                                        120698
              tim = timavg * constt(itmunt+1)                           120698
            end if                                                      120698
            if (ABS(FLOAT(nhrrs) - tim/constt(3)) .lt. 0.1) go to 10    120698
          end if
CC          nhrrs-hourly.
          WRITE (mbufs,9002) nhrrs
          call MVCH1(mbufs,mscrch(n),12)
          n = n + 12
        else if (indxs .eq. 2) then
CC          seasonal.
          WRITE (mbufs,9003) msean(nhrrs)
          call MVCH1(mbufs,mscrch(n),8)
          n = n + 8
        else if (indxs .eq. 3) then
CC          quarterly.
          WRITE (mbufs,9004) nhrrs
          call MVCH1(mbufs,mscrch(n),12)
          n = n + 12
        else if (indxs .eq. 4) then
CC          annual.
          call MVCH1(manul,mscrch(n),6)
          n = n + 6
        end if
        mscrch(n) = mless
        n = n + 1
        mscrch(n) = mblnk1
        n = n + 1
        mscrch(n) = mgrtr
        n = n + 1
        if (jndxs .le. 3) then
          if (jndxs .gt. 2.or.nhrrs .gt. 1) then                        031297
            call MVCH1(mavrg,mscrch(n),7)
            n = n + 7
          end if                                                        031297
        else
          call MVCH1(mtotl,mscrch(n),5)
          n = n + 5
        end if
        if (NUMC1(mspeci,36) .gt. 0) then
          mscrch(n) = mblnk1
          n = n + 1
        end if
        mscrch(n) = mgrtr
        n = n + 1
      end if
   10 i = NUMC1(mspeci,36)
      if (i .gt. 0) then
CC        pollutant/species.
        call MVCH1(mspeci,mscrch(n),i)
        n = n + i
      end if
CC      store name of quantity calc'd, dosage, concentration, etc.
      if (jndxs .eq. 2) then
        mscrch(n) = mless
        n = n + 1
        mscrch(n) = mblnk1
        n = n + 1
        mscrch(n) = mgrtr
        n = n + 1
        call MVCH1(mpeak,mscrch(n),4)
        n = n + 4
        mscrch(n) = mblnk1
        n = n + 1
        mscrch(n) = mgrtr
        n = n + 1
      else if (jndxs .eq. 3) then
        mscrch(n) = mless
        n = n + 1
        mscrch(n) = mblnk1
        n = n + 1
        mscrch(n) = mgrtr
        n = n + 1
        call MVCH1(modelt(jndxs),mscrch(n),12)
        n = n + 12
        mscrch(n) = mblnk1
        n = n + 1
        mscrch(n) = mlbrc
        n = n + 1
        mscrch(n) = mgrtr
        n = n + 1
CC        time label for time-average con.
        if (iflg .eq. 0) then                                           120698
          tim = timavg                                                  120698
        else                                                            120698
          tim = timavg * constt(itmunt+1)                               120698
        end if                                                          120698
        i = tim + 0.5                                                   120698
        if (MOD(i,3600) .eq. 0) then
CC          use hours
          iunit = 2
        else if (MOD(i,60) .eq. 0) then
CC          use minutes
          iunit = 1
        else
CC          use selected output time units itmunt or iswopt(14)
          iunit = itmunt                                                120698
        end if
        a = tim / constt(iunit+1)                                       120698
        i = a
        if (ABS(FLOAT(i)-a) .gt. 0.0) then
          i = 1
        else
          i = 0
        end if
        WRITE (mbufs,9000) munit(iunit+1)
        i = IWTBF(mbufs,12,a,i,0)
        call MVCH1(mbufs,mscrch(n),19)
        n = n + 19                                                      031297
        mscrch(n) = mless
        n = n + 1
        mscrch(n) = mrbrc
        n = n + 1
        mscrch(n) = mless
        n = n + 1
        mscrch(n) = mblnk1
        n = n + 1
      else
        mscrch(n) = mless
        n = n + 1
        mscrch(n) = mblnk1
        n = n + 1
        mscrch(n) = mgrtr
        n = n + 1
      end if
CC        quantity calculated dosage, concentration, etc.
      i = jndxs
      if (i .eq. 3) i = 2
      call MVCH1(modelt(i),mscrch(n),26)
      n = n + 26
      mscrch(n) = mless
      n = n + 1
      mscrch(n) = mblnk1
      n = n + 1
CC        enter left paren '('
      mscrch(n) = mlprn
      n = n + 1
CC        enter mass units.
      if (jndxs .le. 3.and.iswopt(17) .ge. 10) then
CC        ppm, ppb or ppt.
        call MVCH1(munitv(iswopt(17)-7),mscrch(n),18)
        n = n + 18
      else
CC        mass.
        call MVCH1(munitm(iswopt(13)+1),mscrch(n),10)
        n = n + 10
      end if
      if (jndxs .le. 1) then
CC        if dosage.
        mscrch(n) = mless
        n = n + 1
        mscrch(n) = mblnk1
        n = n + 1
CC        enter time units.
        call MVCH1(munitt(iswopt(14)+1),mscrch(n),7)
        n = n + 7
      end if
      if (jndxs .gt. 3.or.iswopt(17) .lt. 10) then
        mscrch(n) = mless
        n = n + 1
CC        enter slash '/'
        mscrch(n) = mslsh
        n = n + 1
        if (jndxs .le. 3) then
CC          enter volume units.
          if (iswopt(17) .le. 8) then
CC            cubic.
            call MVCH1(munitv(1),mscrch(n),18)
            n = n + 18
            mscrch(n) = mless
            n = n + 1
            mscrch(n) = mblnk1
            n = n + 1
CC            meters through inches.
            call MVCH1(munitl(iswopt(17)+1),mscrch(n),11)
            n = n + 11
            i = IFCHS(mscrch,n-11,n-1,19,0,1)
            if (iswopt(17) .eq. 2) i = IFCHS(mscrch,n-11,n-1,5,15,2)
          else
CC            liters.
            call MVCH1(munitv(2),mscrch(n),18)
            n = n + 18
            i = IFCHS(mscrch,n-17,n-1,19,0,1)
          end if
        else
CC          enter area units.
          if (iswopt(16) .le. 8) then
CC            square.
            call MVCH1(munita(1),mscrch(n),7)
            n = n + 7
            mscrch(n) = mless
            n = n + 1
            mscrch(n) = mblnk1
            n = n + 1
CC            meters through inches.
            call MVCH1(munitl(iswopt(16)+1),mscrch(n),11)
            n = n + 11
            i = IFCHS(mscrch,n-11,n-1,19,0,1)
            if (iswopt(16) .eq. 2) i = IFCHS(mscrch,n-11,n-1,5,15,2)
          else
CC            acre or hectare.
            call MVCH1(munita(iswopt(16)-7),mscrch(n),7)
            n = n + 7
          end if
        end if
      end if
      mscrch(n) = mless
      n = n + 1
CC      enter right paren ')'.
      mscrch(n) = mrprn
      ilbstr(1) = 1
CC      pack label down, removing excess blanks.
      n = JPACK(mscrch,n)
CC      save number of characters in main heading.
      nlbsch(1) = n
CC
CC      determine units for x coordinates.
      ilbstr(2) = ilbstr(1) + nlbsch(1)
      n = ilbstr(2)
      mscrch(n) = mlprn
      n = n + 1
      call MVCH1(munitl(iswopt(15)+1),mscrch(n),11)
      n = n + 11
      mscrch(n) = mless
      n = n + 1
      mscrch(n) = mrprn
      n = JPACK(mscrch(ilbstr(2)),n-ilbstr(2)+1)
CC      center in 12 column field.
      if (n .lt. 12) n = ICNTR(mscrch(ilbstr(2)),12)
CC      save number of characters in field.
      nlbsch(2) = 12
CC
CC      determine units for y coordinates.
      ilbstr(3) = ilbstr(2) + nlbsch(2)
      n = ilbstr(3)
      mscrch(n) = mlprn
      n = n + 1
      if (igrect .eq. 0) then
        call MVCH1(munitl(iswopt(15)+1),mscrch(n),11)
        n = n + 11
      else
        call MVCH1(munitg(1),mscrch(n),7)
        n = n + 7
      end if
      mscrch(n) = mless
      n = n + 1
      mscrch(n) = mrprn
      n = JPACK(mscrch(ilbstr(3)),n-ilbstr(3)+1)
CC      center in 12 column field.
      if (n .lt. 12) n = ICNTR(mscrch(ilbstr(3)),12)
CC      save number of characters in field.
      nlbsch(3) = 12
CC
CC        store short form of calculated quantity for center of table.
      ilbstr(4) = ilbstr(3) + nlbsch(3)
      if (jndxs .eq. 1) then
        i = 1
      else if (jndxs .eq. 2) then
        i = 2
      else if (jndxs .eq. 3) then
        i = 5
      else
        i = 6
      end if
      call MVCH1(modelt(i),mscrch(ilbstr(4)),14)
CC      center in 14 column field.
      n = ICNTR(mscrch(ilbstr(4)),14)
      nlbsch(4) = 14
CC
CC      calculation height label.
      nlbsch(5) = 0
      if ((zpsave .gt. 0.0.and.iswopt(6) .le. 1).or.                    120702
     *    (zpsave .gt. 0.0.and.iswopt(6) .eq. 3)) then                  120702
        ilbstr(5) = ilbstr(4) + nlbsch(4)
        if (jndxs .lt.4.or.jndxs .gt. 5) then
          zh = zpsave
        else
          zh = 0.0
        end if
CC        add height of calculation to label.
        if (ifdbug .ne. 26) then                                        020797
          if (iswopt(6) .le. 1) then                                    120702
            WRITE (mbufs,9001) munitl(iswopt(15)+1)
            j = 17
            k = 40
          else                                                          120702
            WRITE (mbufs,9006)                                          120702
            j = 17                                                      120702
            k = 20                                                      120702
          end if                                                        120702
        else                                                            020797
          WRITE (mbufs,9005) munitl(iswopt(15)+1)                       020797
          j = 21
          k = 44
        end if                                                          020797
        if (ifdbug .eq. 26.or.iswopt(6) .le. 1) then                    120702
          i = IWTBF(mbufr(j),12,zh,6,0)                                 020797
        end if                                                          120702
        call MVCH1(mbufs,mscrch(ilbstr(5)),k)                           020797
CC        pack label down, removing excess blanks
        n = JPACK(mscrch(ilbstr(5)),k)                                  020797
CC        center in 35 column field.
        n = ICNTR(mscrch(ilbstr(5)),35)
        nlbsch(5) = n
      end if
      return
 9000 format (13x,'<->',a3)
 9001 format ('at a height of >',13x,a11)
 9002 format ('>',i5,'-hour ')
 9003 format (a8)
 9004 format ('quarter->',i2,' ')
 9005 format ('at a lat. dist. of >',13x,a11)                           020797
 9006 format ('at flag pole heights')
      end
