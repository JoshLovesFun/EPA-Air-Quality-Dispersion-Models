      subroutine wrtfils(note,progver,proglev)

c-----WRTFILS writes statistics to two output files:
c     - hourly stats for plotting of time-series
c     - daily stats table

      USE stat_fields
      USE site_fields
      implicit none

      include 'common.inc'
      
      character*60 note

      integer istrln
      integer ii,i,j,iyr,ihr,idate
      integer n
      integer, allocatable ::  imo(:), idy(:)
      real smobsd_avg,smprdd_avg,sbiasd_avg,serrd_avg,srmsed_avg,
     &     srmsesd_avg,srmseud_avg,sioad_avg
      real dmobsd_avg,dmprdd_avg,dbiasd_avg,derrd_avg
      real tmobsd_avg,tmprdd_avg,tbiasd_avg,terrd_avg,trmsed_avg,
     &     trmsesd_avg,trmseud_avg ,tioad_avg
      real qmobsd_avg,qmprdd_avg,qbiasd_avg,qerrd_avg,qrmsed_avg,
     &     qrmsesd_avg,qrmseud_avg,qioad_avg
      real val

      integer proglev
      real    progver

      allocate( imo(nday) )
      allocate( idy(nday) )

c-----Write headers for hourly-varying  data

      ii = istrln(note)
      write(11,'(a,1x,f3.1,i9)') note(1:ii),progver,proglev
c     write(11,500)' mo/dy,','hr,',
c    &       'ObsWndSpd ,','PrdWndSpd ,','BiasWndSpd,','RMSEWndSpd,', 
c    &       'RMSESWndSp,','RMSEUWndSp,','IOAWndSpd ,','ObsWndDir ,',
c    &       'PrdWndDir ,','BiasWndDir,','ObsTemp   ,','PrdTemp   ,',
c    &       'BiasTemp  ,','RMSETemp  ,','RMSESTemp ,','RMSEUTemp ,',
c    &       'IOATemp   ,','ObsHum    ,','PrdHum    ,','BiasHum   ,',
c    &       'RMSEHum   ,','RMSESHum  ,','RMSEUHum  ,','IOAHum    ,'
      write(11,500)' mo/dy,','hr,',
     &       'Wind Spd  ,','Wind Spd  ,','Wind Spd  ,','Wind Spd  ,',
     &       'Wind Spd  ,','Wind Spd  ,','Wind Spd  ,','Wind Dir  ,',
     &       'Wind Dir  ,','Wind Dir  ,','Temprtr   ,','Temprtr   ,',
     &       'Temprtr   ,','Temprtr   ,','Temprtr   ,','Temprtr   ,',
     &       'Temprtr   ,','Humidity  ,','Humidity  ,','Humidity  ,',
     &       'Humidity  ,','Humidity  ,','Humidity  ,','Humidity  ,'
      write(11,500)'      ,','  ,',
     &       'Mean Obs  ,','Mean Prd  ,','Bias      ,','RMSE      ,',
     &       'Sys RMSE  ,','Unsys RMSE,','IOA       ,','Mean Obs  ,',
     &       'Mean Prd  ,','Bias      ,','Mean Obs  ,','Mean Prd  ,',
     &       'Bias      ,','RMSE      ,','Sys RMSE  ,','Unsys RMSE,',
     &       'IOA       ,','Mean Obs  ,','Mean Prd  ,','Bias      ,',
     &       'RMSE      ,','Sys RMSE  ,','Unsys RMSE,','IOA       ,'
      write(11,500)'      ,','  ,',
     &       '(m/s)     ,','(m/s)     ,','(m/s)     ,','(m/s)     ,',
     &       '(m/s)     ,','(m/s)     ,','          ,','(deg)     ,',
     &       '(deg)     ,','(deg)     ,','(K)       ,','(K)       ,',
     &       '(K)       ,','(K)       ,','(K)       ,','(K)       ,',
     &       '          ,','(pcnt)    ,','(pcnt)    ,','(pcnt)    ,',
     &       '(pcnt)    ,','(pcnt)    ,','(pcnt)    ,','          '

 500  format(26A)

c-----Write hourly-varying data

      do j = 1,nday
        do i = 1,mxhr
          if (idatime(i,j).ne.-999) then
            iyr = idatime(i,j)/100000
            ihr = mod(idatime(i,j),100)
            idate = (idatime(i,j)-iyr*100000)/100
            call caldate(iyr,idate)
            imo(j) = idate/100
            idy(j) = mod(idate,100)
c           if (qmobs(i,j).ne.-999.) qmobs(i,j) = 1000.*qmobs(i,j)
c           if (qmprd(i,j).ne.-999.) qmprd(i,j) = 1000.*qmprd(i,j)
c           if (qbias(i,j).ne.-999.) qbias(i,j) = 1000.*qbias(i,j)
c           if (qrmse(i,j).ne.-999.) qrmse(i,j) = 1000.*qrmse(i,j)
c           if (qrmses(i,j).ne.-999.) qrmses(i,j) = 1000.*qrmses(i,j)
c           if (qrmseu(i,j).ne.-999.) qrmseu(i,j) = 1000.*qrmseu(i,j)
            write(11,1000) imo(j),idy(j),ihr,smobs(i,j),smprd(i,j),
     &         sbias(i,j),srmse(i,j),srmses(i,j),srmseu(i,j),sioa(i,j),
     &         dmobs(i,j),dmprd(i,j),dbias(i,j),tmobs(i,j),tmprd(i,j),
     &         tbias(i,j),trmse(i,j),trmses(i,j),trmseu(i,j),tioa(i,j),
     &         qmobs(i,j),qmprd(i,j),qbias(i,j),qrmse(i,j),qrmses(i,j),
     &         qrmseu(i,j),qioa(i,j)
          endif 
        enddo
c       if (qmobsd(j) .ne.-999.) qmobsd(j)  = 1000.*qmobsd(j)
c       if (qmprdd(j) .ne.-999.) qmprdd(j)  = 1000.*qmprdd(j)
c       if (qbiasd(j) .ne.-999.) qbiasd(j)  = 1000.*qbiasd(j)
c       if (qerrd(j)  .ne.-999.) qerrd(j)   = 1000.*qerrd(j)
c       if (qrmsed(j) .ne.-999.) qrmsed(j)  = 1000.*qrmsed(j)
c       if (qrmsesd(j).ne.-999.) qrmsesd(j) = 1000.*qrmsesd(j)
c       if (qrmseud(j).ne.-999.) qrmseud(j) = 1000.*qrmseud(j)
      enddo
 1000 format(' ',i2.2,'/',i2.2,',',i2.2,24(',',f12.2))

c-----Write daily stats table

      smobsd_avg  = 0.
      smprdd_avg  = 0.
      sbiasd_avg  = 0.
      serrd_avg   = 0.
      srmsed_avg  = 0.
      srmsesd_avg = 0.
      srmseud_avg = 0.
      sioad_avg   = 0.

      dmobsd_avg  = 0.
      dmprdd_avg  = 0.
      dbiasd_avg  = 0.
      derrd_avg   = 0.

      tmobsd_avg  = 0.
      tmprdd_avg  = 0.
      tbiasd_avg  = 0.
      terrd_avg   = 0.
      trmsed_avg  = 0.
      trmsesd_avg = 0.
      trmseud_avg = 0.
      tioad_avg   = 0.

      qmobsd_avg  = 0.
      qmprdd_avg  = 0.
      qbiasd_avg  = 0.
      qerrd_avg   = 0.
      qrmsed_avg  = 0.
      qrmsesd_avg = 0.
      qrmseud_avg = 0.
      qioad_avg   = 0.

      do j = 1, nday
        smobsd_avg  = smobsd_avg  + smobsd(j)
        smprdd_avg  = smprdd_avg  + smprdd(j)
        sbiasd_avg  = sbiasd_avg  + sbiasd(j)
        serrd_avg   = serrd_avg   + serrd(j)
        srmsed_avg  = srmsed_avg  + srmsed(j)
        srmsesd_avg = srmsesd_avg + srmsesd(j)
        srmseud_avg = srmseud_avg + srmseud(j)
        sioad_avg   = sioad_avg   + sioad(j)

        dmobsd_avg  = dmobsd_avg  + dmobsd(j)
        dmprdd_avg  = dmprdd_avg  + dmprdd(j)
        dbiasd_avg  = dbiasd_avg  + dbiasd(j)
        derrd_avg   = derrd_avg   + derrd(j)

        tmobsd_avg  = tmobsd_avg  + tmobsd(j)
        tmprdd_avg  = tmprdd_avg  + tmprdd(j)
        tbiasd_avg  = tbiasd_avg  + tbiasd(j)
        terrd_avg   = terrd_avg   + terrd(j)
        trmsed_avg  = trmsed_avg  + trmsed(j)
        trmsesd_avg = trmsesd_avg + trmsesd(j)
        trmseud_avg = trmseud_avg + trmseud(j)
        tioad_avg   = tioad_avg   + tioad(j)

        qmobsd_avg  = qmobsd_avg  + qmobsd(j)
        qmprdd_avg  = qmprdd_avg  + qmprdd(j)
        qbiasd_avg  = qbiasd_avg  + qbiasd(j)
        qerrd_avg   = qerrd_avg   + qerrd(j)
        qrmsed_avg  = qrmsed_avg  + qrmsed(j)
        qrmsesd_avg = qrmsesd_avg + qrmsesd(j)
        qrmseud_avg = qrmseud_avg + qrmseud(j)
        qioad_avg   = qioad_avg   + qioad(j)
      enddo
      write(12,'(a,1x,f3.1,i9)') note(1:ii),progver,proglev
      write(12,2001) (imo(j),idy(j),j=1,nday)
 2001 format(22x,', , ',32(' ,','    ',i2.2,'/',i2.2))

      write(12,2000) 'Wind Spd,   Mean OBS,(m/s),',(smobsd(j),j=1,nday)
     &               ,smobsd_avg/nday
      write(12,2000) 'Wind Spd,   Mean PRD,(m/s),',(smprdd(j),j=1,nday)
     &               ,smprdd_avg/nday
      write(12,2000) 'Wind Spd,       Bias,(m/s),',(sbiasd(j),j=1,nday)
     &               ,sbiasd_avg/nday
      write(12,2000) 'Wind Spd,Gross Error,(m/s),',(serrd(j),j=1,nday)
     &               ,serrd_avg/nday
      write(12,2000) 'Wind Spd,       RMSE,(m/s),',(srmsed(j),j=1,nday)
     &               ,srmsed_avg/nday
      write(12,2000) 'Wind Spd,   Sys RMSE,(m/s),',(srmsesd(j),j=1,nday)
     &               ,srmsesd_avg/nday
      write(12,2000) 'Wind Spd, Unsys RMSE,(m/s),',(srmseud(j),j=1,nday)
     &               ,srmseud_avg/nday
      write(12,2000) 'Wind Spd,        IOA,     ,',(sioad(j),j=1,nday)
     &               ,sioad_avg/nday

      write(12,2000) 'Wind Dir,   Mean OBS,(deg),',(dmobsd(j),j=1,nday)
     &               ,dmobsd_avg/nday
      write(12,2000) 'Wind Dir,   Mean PRD,(deg),',(dmprdd(j),j=1,nday)
     &               ,dmprdd_avg/nday
      write(12,2000) 'Wind Dir,       Bias,(deg),',(dbiasd(j),j=1,nday)
     &               ,dbiasd_avg/nday
      write(12,2000) 'Wind Dir,Gross Error,(deg),',(derrd(j),j=1,nday)
     &               ,derrd_avg/nday

      write(12,2000) 'Temprtr ,     Mean OBS,(K),',(tmobsd(j),j=1,nday)
     &               ,tmobsd_avg/nday
      write(12,2000) 'Temprtr ,     Mean PRD,(K),',(tmprdd(j),j=1,nday)
     &               ,tmprdd_avg/nday
      write(12,2000) 'Temprtr ,         Bias,(K),',(tbiasd(j),j=1,nday)
     &               ,tbiasd_avg/nday
      write(12,2000) 'Temprtr ,  Gross Error,(K),',(terrd(j),j=1,nday)
     &               ,terrd_avg/nday
      write(12,2000) 'Temprtr ,         RMSE,(K),',(trmsed(j),j=1,nday)
     &               ,trmsed_avg/nday
      write(12,2000) 'Temprtr ,     Sys RMSE,(K),',(trmsesd(j),j=1,nday)
     &               ,trmsesd_avg/nday
      write(12,2000) 'Temprtr ,   Unsys RMSE,(K),',(trmseud(j),j=1,nday)
     &               ,trmseud_avg/nday
      write(12,2000) 'Temprtr ,          IOA,   ,',(tioad(j),j=1,nday)
     &               ,tioad_avg/nday

      write(12,2000) 'Humdity ,  Mean OBS,(pcnt),',(qmobsd(j),j=1,nday)
     &               ,qmobsd_avg/nday
      write(12,2000) 'Humdity ,  Mean PRD,(pcnt),',(qmprdd(j),j=1,nday)
     &               ,qmprdd_avg/nday
      write(12,2000) 'Humdity ,      Bias,(pcnt),',(qbiasd(j),j=1,nday)
     &               ,qbiasd_avg/nday
      write(12,2000) 'Humdity ,Gross Error,(pcnt),',(qerrd(j),j=1,nday)
     &               ,qerrd_avg/nday
      write(12,2000) 'Humdity ,      RMSE,(pcnt),',(qrmsed(j),j=1,nday)
     &               ,qrmsed_avg/nday
      write(12,2000) 'Humdity ,  Sys RMSE,(pcnt),',(qrmsesd(j),j=1,nday)
     &               ,qrmsesd_avg/nday
      write(12,2000) 'Humdity ,Unsys RMSE,(pcnt),',(qrmseud(j),j=1,nday)
     &               ,qrmseud_avg/nday
      write(12,2000) 'Humdity ,       IOA,      ,',(qioad(j),j=1,nday)
     &               ,qioad_avg/nday
 2000 format(a28,32(f10.2,','))

      write(13,'(a,1x,f3.1,i9)') note(1:ii),progver,proglev
      write(13,'(2(a15,i5))')'Num Days = ', nday,'Num Sites = ',nsite
      write(13,'(2a)')' mo/dy,Station        ,lon. (deg),lat. (deg),',
     &                'Variabl,Metric     ,Unit  ,Value '
      do j = 1, nday
        do n = 1, nsite
          val = smobsds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Spd,   Mean OBS,(m/s) ,',val
          end if
          val = smprdds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Spd,   Mean PRD,(m/s) ,',val
          end if
          val = sbiasds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Spd,       Bias,(m/s) ,',val
          end if
          val = serrds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Spd,Gross Error,(m/s) ,',val
          end if
          val = srmseds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Spd,       RMSE,(m/s) ,',val
          end if
          val = srmsesds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Spd,   Sys RMSE,(m/s) ,',val
          end if
          val = srmseuds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Spd, Unsys RMSE,(m/s) ,',val
          end if
          val = sioads(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Spd,        IOA,      ,',val
          end if
          val = dmobsds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Dir,   Mean OBS,(deg) ,',val
          end if
          val = dmprdds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Dir,   Mean PRD,(deg) ,',val
          end if
          val = dbiasds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Dir,       Bias,(deg) ,',val
          end if
          val = derrds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Wind Dir,Gross Error,(deg) ,',val
          end if
          val = tmobsds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Temprtr ,   Mean OBS,(K)   ,',val
          end if
          val = tmprdds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Temprtr ,   Mean PRD,(K)   ,',val
          end if
          val = tbiasds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Temprtr ,       Bias,(K)   ,',val
          end if
          val = terrds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Temprtr ,Gross Error,(K)   ,',val
          end if
          val = trmseds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Temprtr ,       RMSE,(K)   ,',val
          end if
          val = trmsesds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Temprtr ,   Sys RMSE,(K)   ,',val
          end if
          val = trmseuds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Temprtr , Unsys RMSE,(K)   ,',val
          end if
          val = tioads(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Temprtr ,        IOA,      ,',val
          end if
          val = qmobsds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Humdity ,   Mean OBS,(pcnt),',val
          end if
          val = qmprdds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Humdity ,   Mean PRD,(pcnt),',val
          end if
          val = qbiasds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Humdity ,       Bias,(pcnt),',val
          end if
          val = qerrds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Humdity ,Gross Error,(pcnt),',val
          end if
          val = qrmseds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Humdity ,       RMSE,(pcnt),',val
          end if
          val = qrmsesds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Humdity ,   Sys RMSE,(pcnt),',val
          end if
          val = qrmseuds(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Humdity , Unsys RMSE,(pcnt),',val
          end if
          val = qioads(n,j)
          if (val .ne. -999.) then
            write(13,510)imo(j),idy(j),namsit(n),
     &                   ylonlist(n),xlatlist(n),
     &                  'Humdity ,        IOA,      ,',val
          end if
        end do
      end do
  510  format(' ',i2.2,"/",i2.2,',',a15,',',2(f10.4,','),a,f10.4)
          

      deallocate( imo )
      deallocate( idy )
      return
      end
