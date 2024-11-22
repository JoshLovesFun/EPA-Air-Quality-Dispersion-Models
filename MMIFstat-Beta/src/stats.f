      subroutine stats(outvector,note,progver,proglev)

c-----STATS is the driver for the calculation of all MM5 performance statistics.
c     Statistics for two time-scales are calculated:
c     -Hourly
c     -Daily

      USE stat_fields
      USE site_fields
      implicit none

      include 'common.inc'

      logical outvector

      integer j,i,n,nn
      integer istrln,ii
      integer iyr,ihr,idate,imo,idy

      real utmp,vtmp,dum,tmp

      real, allocatable :: sobs(:),sprd(:)
      real, allocatable :: dobs(:),dprd(:)
      real, allocatable :: uo(:),vo(:),to(:),qo(:)
      real, allocatable :: up(:),vp(:),tp(:),qp(:)

      character*60 note
      integer proglev
      real    progver


      allocate( sobs(nsite*mxhr) )
      allocate( sprd(nsite*mxhr) )
      allocate( dobs(nsite*mxhr) )
      allocate( dprd(nsite*mxhr) )
      allocate( uo(nsite*mxhr) )
      allocate( vo(nsite*mxhr) )
      allocate( to(nsite*mxhr) )
      allocate( qo(nsite*mxhr) )
      allocate( up(nsite*mxhr) )
      allocate( vp(nsite*mxhr) )
      allocate( tp(nsite*mxhr) )
      allocate( qp(nsite*mxhr) )

c     write(79,*)'stats.f nday,mxhr,nstats ',nday,mxhr,nsite 
      if (outvector) then
        ii = istrln(note)
        write(79,'(a,1x,f3.1,i9)') note(1:ii),progver,proglev
        write(79,'(5a)')' mo/dy,hr,Station        ,',
     &                 'x(Grid)   ,y(Grid)   ,',
     &                 'U-obs(mps),V-obs(mps),U-prd(mps),',
     &                 'V-prd(mps),T-obs(K)  ,T-prd(K)  ,',
     &                 'RH-obs(%) ,RH-prd(%),'
        do j = 1, nday
          do i = 1, mxhr
            do n = 1, nsite 
              if( (uobs(n,i,j) .ne. -999.) .and.
     &           (vobs(n,i,j) .ne. -999.) .and.
     &           (tobs(n,i,j) .ne. -999.) .and.
     &           (qobs(n,i,j) .ne. -999.) .and.
     &           (idatime(i,j) .ne. -999 ) ) then

                iyr = idatime(i,j)/100000
                ihr = mod(idatime(i,j),100)
                idate = (idatime(i,j)-iyr*100000)/100
                call caldate(iyr,idate)
                imo = idate/100
                idy = mod(idate,100)
                write(79,500)imo,idy,ihr,namsit(n),
     &                  ri(n),rj(n),
     &                  uobs(n,i,j),vobs(n,i,j),
     &                  uprd(n,i,j),vprd(n,i,j),
     &                  tobs(n,i,j),tprd(n,i,j),
     &                  qobs(n,i,j),qprd(n,i,j)
 500             format(' ',i2.2,"/",i2.2,',',i2.2,',',
     &                  a15,11(',',f10.2))
              end if
            end do
          end do
        end do
      end if

      do j = 1,nday

c-----Hourly mean OBS and PRD

        do i = 1,mxhr

c-----Wind speed

          do n = 1,nsite
            call uv2spdr(uobs(n,i,j),vobs(n,i,j),sobs(n),dobs(n))
          enddo
          call averag(sobs,smobs(i,j),nsite)
          do n = 1,nsite
            call uv2spdr(uprd(n,i,j),vprd(n,i,j),sprd(n),dprd(n))
          enddo
          call averag_0(sprd,uobs(1,i,j),smprd(i,j),nsite)

c-----Wind direction

          call averag(uobs(1,i,j),utmp,nsite)
          call averag(vobs(1,i,j),vtmp,nsite)
          call uv2spdr(utmp,vtmp,dum,dmobs(i,j))
          call averag_0(uprd(1,i,j),uobs(1,i,j),utmp,nsite)
          call averag_0(vprd(1,i,j),vobs(1,i,j),vtmp,nsite)
          call uv2spdr(utmp,vtmp,dum,dmprd(i,j))

c-----Temperature and humidity

          call averag(tobs(1,i,j),tmobs(i,j),nsite)
          call averag_0(tprd(1,i,j),tobs(1,i,j),tmprd(i,j),nsite)
          call averag(qobs(1,i,j),qmobs(i,j),nsite)
          call averag_0(qprd(1,i,j),qobs(1,i,j),qmprd(i,j),nsite)

c-----Hourly signed error (bias)

          call biaserrw(sobs,sprd,dobs,dprd,sbias(i,j),dum,
     &                  dbias(i,j),dum,nsite)
          call biaserr(tobs(1,i,j),tprd(1,i,j),tbias(i,j),tmp,nsite) 
          call biaserr(qobs(1,i,j),qprd(1,i,j),qbias(i,j),tmp,nsite) 

c-----Hourly RMSE

          call getrmse(sobs,sprd,srmse(i,j),srmses(i,j),srmseu(i,j),
     &                 nsite)
          call getrmse(tobs(1,i,j),tprd(1,i,j),trmse(i,j),trmses(i,j),
     &                 trmseu(i,j),nsite)
          call getrmse(qobs(1,i,j),qprd(1,i,j),qrmse(i,j),qrmses(i,j),
     &                 qrmseu(i,j),nsite)

c-----Hourly Index of Agreement

          call getioa(sobs,sprd,smobs(i,j),srmse(i,j),sioa(i,j),nsite)
          call getioa(tobs(1,i,j),tprd(1,i,j),tmobs(i,j),trmse(i,j),
     &                tioa(i,j),nsite)
          call getioa(qobs(1,i,j),qprd(1,i,j),qmobs(i,j),qrmse(i,j),
     &                qioa(i,j),nsite)

        enddo

c-----Daily mean OBS and PRD

        do i = 1,mxhr
          do n = 1,nsite
            nn = n + (i - 1)*nsite
            uo(nn) = uobs(n,i,j)
            vo(nn) = vobs(n,i,j)
            to(nn) = tobs(n,i,j)
            qo(nn) = qobs(n,i,j)
            up(nn) = uprd(n,i,j)
            vp(nn) = vprd(n,i,j)
            tp(nn) = tprd(n,i,j)
            qp(nn) = qprd(n,i,j)
          enddo
        enddo


c-----Wind speed

        do n = 1,mxhr*nsite
          call uv2spdr(uo(n),vo(n),sobs(n),dobs(n))
        enddo
        call averag(sobs,smobsd(j),mxhr*nsite)
        do n = 1,mxhr*nsite
          call uv2spdr(up(n),vp(n),sprd(n),dprd(n))
        enddo
        call averag_0(sprd,uo,smprdd(j),mxhr*nsite)

c-----Wind direction

        call averag(uo,utmp,mxhr*nsite)
        call averag(vo,vtmp,mxhr*nsite)
        call uv2spdr(utmp,vtmp,dum,dmobsd(j))
        call averag_0(up,uo,utmp,mxhr*nsite)
        call averag_0(vp,vo,vtmp,mxhr*nsite)
        call uv2spdr(utmp,vtmp,dum,dmprdd(j))

c-----Temperature and humidity

        call averag(to,tmobsd(j),mxhr*nsite)
        call averag(qo,qmobsd(j),mxhr*nsite)
        call averag_0(tp,to,tmprdd(j),mxhr*nsite)
        call averag_0(qp,qo,qmprdd(j),mxhr*nsite)

c-----Daily bias and gross error

        call biaserrw(sobs,sprd,dobs,dprd,sbiasd(j),serrd(j),
     &                dbiasd(j),derrd(j),mxhr*nsite)
        call biaserr(to,tp,tbiasd(j),terrd(j),mxhr*nsite)
        call biaserr(qo,qp,qbiasd(j),qerrd(j),mxhr*nsite)
 
c-----Daily RMSE

        call getrmse(sobs,sprd,srmsed(j),srmsesd(j),srmseud(j),
     &               mxhr*nsite)
        call getrmse(to,tp,trmsed(j),trmsesd(j),trmseud(j),mxhr*nsite)
        call getrmse(qo,qp,qrmsed(j),qrmsesd(j),qrmseud(j),mxhr*nsite)

c-----Daily Index of Agreement

        call getioa(sobs,sprd,smobsd(j),srmsed(j),sioad(j),mxhr*nsite)
        call getioa(to,tp,tmobsd(j),trmsed(j),tioad(j),mxhr*nsite)
        call getioa(qo,qp,qmobsd(j),qrmsed(j),qioad(j),mxhr*nsite)

c-----
c-----Station Daily Summary Statistics
c-----
        do n = 1,nsite
          
          do i = 1,mxhr
            uo(i) = uobs(n,i,j)
            vo(i) = vobs(n,i,j)
            to(i) = tobs(n,i,j)
            qo(i) = qobs(n,i,j)
            up(i) = uprd(n,i,j)
            vp(i) = vprd(n,i,j)
            tp(i) = tprd(n,i,j)
            qp(i) = qprd(n,i,j)
           end do

c-----Wind speed

          do i = 1,mxhr
            call uv2spdr(uo(i),vo(i),sobs(i),dobs(i))
          enddo
          call averag(sobs,smobsds(n,j),mxhr)
          do i = 1,mxhr
            call uv2spdr(up(i),vp(i),sprd(i),dprd(i))
          enddo
          call averag_0(sprd,uo,smprdds(n,j),mxhr)

c-----Wind direction

          call averag(uo,utmp,mxhr)
          call averag(vo,vtmp,mxhr)
          call uv2spdr(utmp,vtmp,dum,dmobsds(n,j))
          call averag_0(up,uo,utmp,mxhr)
          call averag_0(vp,vo,vtmp,mxhr)
          call uv2spdr(utmp,vtmp,dum,dmprdds(n,j))

c-----Temperature and humidity

          call averag(to,tmobsds(n,j),mxhr)
          call averag(qo,qmobsds(n,j),mxhr)
          call averag_0(tp,to,tmprdds(n,j),mxhr)
          call averag_0(qp,qo,qmprdds(n,j),mxhr)

c-----Daily bias and gross error

          call biaserrw(sobs,sprd,dobs,dprd,sbiasds(n,j),serrds(n,j),
     &                  dbiasds(n,j),derrds(n,j),mxhr)
          call biaserr(to,tp,tbiasds(n,j),terrds(n,j),mxhr)
          call biaserr(qo,qp,qbiasds(n,j),qerrds(n,j),mxhr)

c-----Daily RMSE

          call getrmse(sobs,sprd,srmseds(n,j),srmsesds(n,j),
     &                 srmseuds(n,j),mxhr)
          call getrmse(to,tp,trmseds(n,j),trmsesds(n,j),
     &                 trmseuds(n,j),mxhr)
          call getrmse(qo,qp,qrmseds(n,j),qrmsesds(n,j),
     &                 qrmseuds(n,j),mxhr)

c-----Daily Index of Agreement

          call getioa(sobs,sprd,smobsds(n,j),srmseds(n,j),
     &                 sioads(n,j),mxhr)
          call getioa(to,tp,tmobsds(n,j),trmseds(n,j),
     &                 tioads(n,j),mxhr)
          call getioa(qo,qp,qmobsds(n,j),qrmseds(n,j),
     &                 qioads(n,j),mxhr)
        enddo

      enddo

      deallocate( sobs )
      deallocate( sprd )
      deallocate( dobs )
      deallocate( dprd )
      deallocate( uo )
      deallocate( vo )
      deallocate( to )
      deallocate( qo )
      deallocate( up )
      deallocate( vp )
      deallocate( tp )
      deallocate( qp )

      return
      end
