
      logical llcp,lpol
      integer iyear,ibegday,ibegdat,ienddat,itzon,nday,nhr,
     &        ninsite,nsite,ibeg,iend,jbeg,jend
      integer nx,ny,nz,nest,nwrftimes,nproj
      integer mxhr
      real clat,clon,tlat1,tlat2,x0mm5,y0mm5,xfmm5,yfmm5,
     &     deltax,dxcrs,riloc,rjloc,ptop,ps0,ts0,tlp,
     &     cone_factor
      real xbeg,xend,ybeg,yend,zbeg,zend

      common /general/ iyear,ibegday,ibegdat,ienddat,itzon,nday,nhr,
     &                 ninsite,nsite,
     &                 xbeg,xend,ybeg,yend,ibeg,iend,jbeg,
     &                 jend,zbeg,zend,mxhr

      common /domain/  nx,ny,nz,clat,clon,tlat1,tlat2,x0mm5,y0mm5,
     &                 deltax,dxcrs,nest,riloc,rjloc,ptop,ps0,ts0,tlp,
     &                 cone_factor,xfmm5,yfmm5

      common /logicals/  llcp,lpol

      common /wrf/  nwrftimes,nproj
