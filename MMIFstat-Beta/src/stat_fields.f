      MODULE stat_fields

      integer, allocatable:: idatime(:,:)  !date/time stamp

      real*4, allocatable::  uprd(:,:,:)   !predicted u wind component 
      real*4, allocatable::  vprd(:,:,:)   !predicted v wind component 
      real*4, allocatable::  smprd(:,:)         !predicted mean wind speed
      real*4, allocatable::  dmprd(:,:)         !predicted mean wind direction
      real*4, allocatable::  tprd(:,:,:)   !predicted temp
      real*4, allocatable::  tmprd(:,:)         !predicted mean temp 
      real*4, allocatable::  qprd(:,:,:)   !predicted humidity 
      real*4, allocatable::  qmprd(:,:)         !predicted mean humidity 
      real*4, allocatable::  smprdd(:)             !predicted daily mean wind speed
      real*4, allocatable::  dmprdd(:)             !predicted daily mean wind direction
      real*4, allocatable::  tmprdd(:)             !predicted daily mean temp
      real*4, allocatable::  qmprdd(:)             !predicted daily mean humidity
      real*4, allocatable::  smprdds(:,:)             !predicted daily mean wind speed
      real*4, allocatable::  dmprdds(:,:)             !predicted daily mean wind direction
      real*4, allocatable::  tmprdds(:,:)             !predicted daily mean temp
      real*4, allocatable::  qmprdds(:,:)             !predicted daily mean humidity

c     common /predicts/ uprd,vprd,smprd,dmprd,tprd,tmprd,qprd,
c    &                  qmprd,smprdd,dmprdd,tmprdd,qmprdd

      real*4, allocatable::  uobs(:,:,:)   !observed u wind component 
      real*4, allocatable::  vobs(:,:,:)   !observed v wind component 
      real*4, allocatable::  smobs(:,:)         !observed mean wind speed 
      real*4, allocatable::  dmobs(:,:)         !observed mean wind direction 
      real*4, allocatable::  tobs(:,:,:)   !observed temp
      real*4, allocatable::  tmobs(:,:)         !observed mean temp 
      real*4, allocatable::  qobs(:,:,:)   !observed humidity 
      real*4, allocatable::  qmobs(:,:)         !observed mean humidity 
      real*4, allocatable::  smobsd(:)             !observed daily mean wind speed
      real*4, allocatable::  dmobsd(:)             !observed daily mean wind direction
      real*4, allocatable::  tmobsd(:)             !observed daily mean temp
      real*4, allocatable::  qmobsd(:)             !observed daily mean humidity
      real*4, allocatable::  smobsds(:,:)             !observed daily mean wind speed
      real*4, allocatable::  dmobsds(:,:)             !observed daily mean wind direction
      real*4, allocatable::  tmobsds(:,:)             !observed daily mean temp
      real*4, allocatable::  qmobsds(:,:)             !observed daily mean humidity

c     common /observe/ uobs,vobs,smobs,dmobs,tobs,tmobs,qobs,qmobs,
c    &                 smobsd,dmobsd,tmobsd,qmobsd

      real*4, allocatable::  sbias(:,:)         !hourly signed bias (bias) wind speed
      real*4, allocatable::  dbias(:,:)         !hourly signed bias (bias) wind direction
      real*4, allocatable::  tbias(:,:)         !hourly signed bias (bias) temp
      real*4, allocatable::  qbias(:,:)         !hourly signed bias (bias) humidity
      real*4, allocatable::  srmse(:,:)         !hourly RMSE in wind speed
      real*4, allocatable::  srmses(:,:)        !hourly systematic RMSE in wind speed
      real*4, allocatable::  srmseu(:,:)        !hourly unsystematic RMSE in wind speed
      real*4, allocatable::  trmse(:,:)         !hourly RMSE in temp
      real*4, allocatable::  trmses(:,:)        !hourly systematic RMSE in temp
      real*4, allocatable::  trmseu(:,:)        !hourly unsystematic RMSE in temp
      real*4, allocatable::  qrmse(:,:)         !hourly RMSE in humidity
      real*4, allocatable::  qrmses(:,:)        !hourly systematic RMSE in humidity
      real*4, allocatable::  qrmseu(:,:)        !hourly unsystematic RMSE in humidity
      real*4, allocatable::  sioa(:,:)          !hourly index of agreement for wind speed
      real*4, allocatable::  tioa(:,:)          !hourly index of agreement for temp
      real*4, allocatable::  qioa(:,:)          !hourly index of agreement for humidity

c     common /hrstat/ sbias,dbias,tbias,qbias,srmse,srmses,srmseu,
c    &                trmse,trmses,trmseu,qrmse,qrmses,qrmseu,sioa,
c    &                tioa,qioa

      real*4, allocatable::  sbiasd(:)	       !daily bias in wind speed
      real*4, allocatable::  serrd(:)	       !daily gross error in wind speed
      real*4, allocatable::  dbiasd(:)	       !daily bias in wind direction
      real*4, allocatable::  derrd(:)	       !daily gross error in wind direction
      real*4, allocatable::  tbiasd(:)	       !daily bias in temp
      real*4, allocatable::  terrd(:)	       !daily gross error in temp
      real*4, allocatable::  qbiasd(:)	       !daily bias in humidity
      real*4, allocatable::  qerrd(:)	       !daily gross error in humidity
      real*4, allocatable::  srmsed(:)	       !daily RMSE in wind speed
      real*4, allocatable::  srmsesd(:)	       !daily systematic RMSE in wind speed
      real*4, allocatable::  srmseud(:)	       !daily unsystematic RMSE in wind speed
      real*4, allocatable::  trmsed(:)	       !daily RMSE in temp
      real*4, allocatable::  trmsesd(:)	       !daily systematic RMSE in temp
      real*4, allocatable::  trmseud(:)	       !daily unsystematic RMSE in temp
      real*4, allocatable::  qrmsed(:)	       !daily RMSE in humidity
      real*4, allocatable::  qrmsesd(:)	       !daily systematic RMSE in humidity
      real*4, allocatable::  qrmseud(:)	       !daily unsystematic RMSE in humidity
      real*4, allocatable::  sioad(:)              !daily index of agreement in wind speed
      real*4, allocatable::  tioad(:)              !daily index of agreement in temp
      real*4, allocatable::  qioad(:)              !daily index of agreement in humidity

c     common /dystat/ sbiasd,serrd,dbiasd,derrd,tbiasd,terrd,qbiasd,
c    &                qerrd,srmsed,srmsesd,srmseud,trmsed,trmsesd,
c    &                trmseud,qrmsed,qrmsesd,qrmseud,sioad,tioad,qioad

      real*4, allocatable::  sbiasds(:,:)	       !daily bias in wind speed
      real*4, allocatable::  serrds(:,:)	       !daily gross error in wind speed
      real*4, allocatable::  dbiasds(:,:)	       !daily bias in wind direction
      real*4, allocatable::  derrds(:,:)	       !daily gross error in wind direction
      real*4, allocatable::  tbiasds(:,:)	       !daily bias in temp
      real*4, allocatable::  terrds(:,:)	       !daily gross error in temp
      real*4, allocatable::  qbiasds(:,:)	       !daily bias in humidity
      real*4, allocatable::  qerrds(:,:)	       !daily gross error in humidity
      real*4, allocatable::  srmseds(:,:)	       !daily RMSE in wind speed
      real*4, allocatable::  srmsesds(:,:)       !daily systematic RMSE in wind speed
      real*4, allocatable::  srmseuds(:,:)       !daily unsystematic RMSE in wind speed
      real*4, allocatable::  trmseds(:,:)	       !daily RMSE in temp
      real*4, allocatable::  trmsesds(:,:)       !daily systematic RMSE in temp
      real*4, allocatable::  trmseuds(:,:)        !daily unsystematic RMSE in temp
      real*4, allocatable::  qrmseds(:,:)	       !daily RMSE in humidity
      real*4, allocatable::  qrmsesds(:,:)       !daily systematic RMSE in humidity
      real*4, allocatable::  qrmseuds(:,:)       !daily unsystematic RMSE in humidity
      real*4, allocatable::  sioads(:,:)         !daily index of agreement in wind speed
      real*4, allocatable::  tioads(:,:)         !daily index of agreement in temp
      real*4, allocatable::  qioads(:,:)         !daily index of agreement in humidity

      CONTAINS

      subroutine alloc_stat(mxsite,mxhr,mxday,missing)
      implicit none

      integer mxsite,mxhr,mxday
      real missing

c     write(*,*)'Alloc_stat mxsite,mxhr,mxday ',mxsite,mxhr,mxday

      allocate( idatime(mxhr,mxday) )
      idatime(:,:) = -999
      allocate( uprd(mxsite,mxhr,mxday) )   !predicted u wind component 
      uprd(:,:,:) = missing
      allocate( vprd(mxsite,mxhr,mxday) )   !predicted v wind component 
      vprd(:,:,:) = missing
      allocate( smprd(mxhr,mxday) )         !predicted mean wind speed
      smprd(:,:) = missing
      allocate( dmprd(mxhr,mxday) )         !predicted mean wind direction
      dmprd(:,:) = missing
      allocate( tprd(mxsite,mxhr,mxday) )   !predicted temp
      tprd(:,:,:) = missing
      allocate( tmprd(mxhr,mxday) )         !predicted mean temp 
      tmprd(:,:) = missing
      allocate( qprd(mxsite,mxhr,mxday) )   !predicted humidity 
      qprd(:,:,:) = missing
      allocate( qmprd(mxhr,mxday) )         !predicted mean humidity 
      qmprd(:,:) = missing
      allocate( smprdd(mxday) )             !predicted daily mean wind speed
      allocate( dmprdd(mxday) )             !predicted daily mean wind direction
      allocate( tmprdd(mxday) )             !predicted daily mean temp
      allocate( qmprdd(mxday) )             !predicted daily mean humidity
      allocate( smprdds(mxsite,mxday) )             !predicted daily mean wind speed
      allocate( dmprdds(mxsite,mxday) )             !predicted daily mean wind direction
      allocate( tmprdds(mxsite,mxday) )             !predicted daily mean temp
      allocate( qmprdds(mxsite,mxday) )             !predicted daily mean humidity

      allocate( uobs(mxsite,mxhr,mxday) )   !observed u wind component 
      uobs(:,:,:) = missing
      allocate( vobs(mxsite,mxhr,mxday) )   !observed v wind component 
      vobs(:,:,:) = missing
      allocate( smobs(mxhr,mxday) )         !observed mean wind speed 
      smobs(:,:) = missing
      allocate( dmobs(mxhr,mxday) )         !observed mean wind direction 
      dmobs(:,:) = missing
      allocate( tobs(mxsite,mxhr,mxday) )   !observed temp
      tobs(:,:,:) = missing
      allocate( tmobs(mxhr,mxday) )         !observed mean temp 
      tmobs(:,:) = missing
      allocate( qobs(mxsite,mxhr,mxday) )   !observed humidity 
      qobs(:,:,:) = missing
      allocate( qmobs(mxhr,mxday) )         !observed mean humidity 
      qmobs(:,:) = missing
      allocate( smobsd(mxday) )             !observed daily mean wind speed
      allocate( dmobsd(mxday) )             !observed daily mean wind direction
      allocate( tmobsd(mxday) )             !observed daily mean temp
      allocate( qmobsd(mxday) )             !observed daily mean humidity
      allocate( smobsds(mxsite,mxday) )             !observed daily mean wind speed
      allocate( dmobsds(mxsite,mxday) )             !observed daily mean wind direction
      allocate( tmobsds(mxsite,mxday) )             !observed daily mean temp
      allocate( qmobsds(mxsite,mxday) )             !observed daily mean humidity

      allocate( sbias(mxhr,mxday) )         !hourly signed bias (bias) wind speed
      allocate( dbias(mxhr,mxday) )         !hourly signed bias (bias) wind direction
      allocate( tbias(mxhr,mxday) )         !hourly signed bias (bias) temp
      allocate( qbias(mxhr,mxday) )         !hourly signed bias (bias) humidity
      allocate( srmse(mxhr,mxday) )         !hourly RMSE in wind speed
      allocate( srmses(mxhr,mxday) )        !hourly systematic RMSE in wind speed
      allocate( srmseu(mxhr,mxday) )        !hourly unsystematic RMSE in wind speed
      allocate( trmse(mxhr,mxday) )         !hourly RMSE in temp
      allocate( trmses(mxhr,mxday) )        !hourly systematic RMSE in temp
      allocate( trmseu(mxhr,mxday) )        !hourly unsystematic RMSE in temp
      allocate( qrmse(mxhr,mxday) )         !hourly RMSE in humidity
      allocate( qrmses(mxhr,mxday) )        !hourly systematic RMSE in humidity
      allocate( qrmseu(mxhr,mxday) )        !hourly unsystematic RMSE in humidity
      allocate( sioa(mxhr,mxday) )          !hourly index of agreement for wind speed
      allocate( tioa(mxhr,mxday) )          !hourly index of agreement for temp
      allocate( qioa(mxhr,mxday) )          !hourly index of agreement for humidity

      allocate( sbiasd(mxday) )	            !daily bias in wind speed
      allocate( serrd(mxday) )	            !daily gross error in wind speed
      allocate( dbiasd(mxday) )	            !daily bias in wind direction
      allocate( derrd(mxday) )	            !daily gross error in wind direction
      allocate( tbiasd(mxday) )	            !daily bias in temp
      allocate( terrd(mxday) )	            !daily gross error in temp
      allocate( qbiasd(mxday) )	            !daily bias in humidity
      allocate( qerrd(mxday) )	            !daily gross error in humidity
      allocate( srmsed(mxday) )	            !daily RMSE in wind speed
      allocate( srmsesd(mxday) )	    !daily systematic RMSE in wind speed
      allocate( srmseud(mxday) )	    !daily unsystematic RMSE in wind speed
      allocate( trmsed(mxday) )	            !daily RMSE in temp
      allocate( trmsesd(mxday) )	    !daily systematic RMSE in temp
      allocate( trmseud(mxday) )	    !daily unsystematic RMSE in temp
      allocate( qrmsed(mxday) )             !daily RMSE in humidity
      allocate( qrmsesd(mxday) )            !daily systematic RMSE in humidity
      allocate( qrmseud(mxday) )            !daily unsystematic RMSE in humidity
      allocate( sioad(mxday) )              !daily index of agreement in wind speed
      allocate( tioad(mxday) )              !daily index of agreement in temp
      allocate( qioad(mxday) )              !daily index of agreement in humidity

      allocate( sbiasds(mxsite,mxday) )	            !daily bias in wind speed
      allocate( serrds(mxsite,mxday) )	            !daily gross error in wind speed
      allocate( dbiasds(mxsite,mxday) )	            !daily bias in wind direction
      allocate( derrds(mxsite,mxday) )	            !daily gross error in wind direction
      allocate( tbiasds(mxsite,mxday) )	            !daily bias in temp
      allocate( terrds(mxsite,mxday) )	            !daily gross error in temp
      allocate( qbiasds(mxsite,mxday) )	            !daily bias in humidity
      allocate( qerrds(mxsite,mxday) )	            !daily gross error in humidity
      allocate( srmseds(mxsite,mxday) )	            !daily RMSE in wind speed
      allocate( srmsesds(mxsite,mxday) )	    !daily systematic RMSE in wind speed
      allocate( srmseuds(mxsite,mxday) )	    !daily unsystematic RMSE in wind speed
      allocate( trmseds(mxsite,mxday) )	            !daily RMSE in temp
      allocate( trmsesds(mxsite,mxday) )	    !daily systematic RMSE in temp
      allocate( trmseuds(mxsite,mxday) )	    !daily unsystematic RMSE in temp
      allocate( qrmseds(mxsite,mxday) )             !daily RMSE in humidity
      allocate( qrmsesds(mxsite,mxday) )            !daily systematic RMSE in humidity
      allocate( qrmseuds(mxsite,mxday) )            !daily unsystematic RMSE in humidity
      allocate( sioads(mxsite,mxday) )              !daily index of agreement in wind speed
      allocate( tioads(mxsite,mxday) )              !daily index of agreement in temp
      allocate( qioads(mxsite,mxday) )              !daily index of agreement in humidity

      end subroutine alloc_stat

      subroutine dealloc_stat
      deallocate( idatime )
      deallocate( uprd )          !predicted u wind component 
      deallocate( vprd )          !predicted v wind component 
      deallocate( smprd )         !predicted mean wind speed
      deallocate( dmprd )         !predicted mean wind direction
      deallocate( tprd )          !predicted temp
      deallocate( tmprd )         !predicted mean temp 
      deallocate( qprd )          !predicted humidity 
      deallocate( qmprd )         !predicted mean humidity 
      deallocate( smprdd )        !predicted daily mean wind speed
      deallocate( dmprdd )        !predicted daily mean wind direction
      deallocate( tmprdd )        !predicted daily mean temp
      deallocate( qmprdd )        !predicted daily mean humidity
      deallocate( smprdds )        !predicted daily mean wind speed
      deallocate( dmprdds )        !predicted daily mean wind direction
      deallocate( tmprdds )        !predicted daily mean temp
      deallocate( qmprdds )        !predicted daily mean humidity

      deallocate( uobs )          !observed u wind component 
      deallocate( vobs )          !observed v wind component 
      deallocate( smobs )         !observed mean wind speed 
      deallocate( dmobs )         !observed mean wind direction 
      deallocate( tobs )          !observed temp
      deallocate( tmobs )         !observed mean temp 
      deallocate( qobs )          !observed humidity 
      deallocate( qmobs )         !observed mean humidity 
      deallocate( smobsd )        !observed daily mean wind speed
      deallocate( dmobsd )        !observed daily mean wind direction
      deallocate( tmobsd )        !observed daily mean temp
      deallocate( qmobsd )        !observed daily mean humidity
      deallocate( smobsds )        !observed daily mean wind speed
      deallocate( dmobsds )        !observed daily mean wind direction
      deallocate( tmobsds )        !observed daily mean temp
      deallocate( qmobsds )        !observed daily mean humidity

      deallocate( sbias )         !hourly signed bias (bias) wind speed
      deallocate( dbias )         !hourly signed bias (bias) wind direction
      deallocate( tbias )         !hourly signed bias (bias) temp
      deallocate( qbias )         !hourly signed bias (bias) humidity
      deallocate( srmse )         !hourly RMSE in wind speed
      deallocate( srmses )        !hourly systematic RMSE in wind speed
      deallocate( srmseu )        !hourly unsystematic RMSE in wind speed
      deallocate( trmse )         !hourly RMSE in temp
      deallocate( trmses )        !hourly systematic RMSE in temp
      deallocate( trmseu )        !hourly unsystematic RMSE in temp
      deallocate( qrmse )         !hourly RMSE in humidity
      deallocate( qrmses )        !hourly systematic RMSE in humidity
      deallocate( qrmseu )        !hourly unsystematic RMSE in humidity
      deallocate( sioa )          !hourly index of agreement for wind speed
      deallocate( tioa )          !hourly index of agreement for temp
      deallocate( qioa )          !hourly index of agreement for humidity

      deallocate( sbiasd )	!daily bias in wind speed
      deallocate( serrd )	        !daily gross error in wind speed
      deallocate( dbiasd )	!daily bias in wind direction
      deallocate( derrd )	        !daily gross error in wind direction
      deallocate( tbiasd )	!daily bias in temp
      deallocate( terrd )	        !daily gross error in temp
      deallocate( qbiasd )	!daily bias in humidity
      deallocate( qerrd )	        !daily gross error in humidity
      deallocate( srmsed )	!daily RMSE in wind speed
      deallocate( srmsesd )	!daily systematic RMSE in wind speed
      deallocate( srmseud )	!daily unsystematic RMSE in wind speed
      deallocate( trmsed )	!daily RMSE in temp
      deallocate( trmsesd )	!daily systematic RMSE in temp
      deallocate( trmseud )	!daily unsystematic RMSE in temp
      deallocate( qrmsed )        !daily RMSE in humidity
      deallocate( qrmsesd )       !daily systematic RMSE in humidity
      deallocate( qrmseud )       !daily unsystematic RMSE in humidity
      deallocate( sioad )         !daily index of agreement in wind speed
      deallocate( tioad )         !daily index of agreement in temp
      deallocate( qioad )         !daily index of agreement in humidity
 
      deallocate( sbiasds )	!daily bias in wind speed
      deallocate( serrds )	        !daily gross error in wind speed
      deallocate( dbiasds )	!daily bias in wind direction
      deallocate( derrds )	        !daily gross error in wind direction
      deallocate( tbiasds )	!daily bias in temp
      deallocate( terrds )	        !daily gross error in temp
      deallocate( qbiasds )	!daily bias in humidity
      deallocate( qerrds )	        !daily gross error in humidity
      deallocate( srmseds )	!daily RMSE in wind speed
      deallocate( srmsesds )	!daily systematic RMSE in wind speed
      deallocate( srmseuds )	!daily unsystematic RMSE in wind speed
      deallocate( trmseds )	!daily RMSE in temp
      deallocate( trmsesds )	!daily systematic RMSE in temp
      deallocate( trmseuds )	!daily unsystematic RMSE in temp
      deallocate( qrmseds )        !daily RMSE in humidity
      deallocate( qrmsesds )       !daily systematic RMSE in humidity
      deallocate( qrmseuds )       !daily unsystematic RMSE in humidity
      deallocate( sioads )         !daily index of agreement in wind speed
      deallocate( tioads )         !daily index of agreement in temp
      deallocate( qioads )         !daily index of agreement in humidity
 
      end subroutine dealloc_stat

      END MODULE stat_fields
