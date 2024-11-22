MODULE wrf_netcdf
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     This module contains various NetCDF subroutines.
!
!     Development History:
!     2009-05-26  Original Development (ENVIRON International Corp.)
!     2013-02-25  Lengthened file/path strings from 120 to 256 characters, 
!                 based on user feedback.
!     2013-03-26  A few instances of 'file' were hard-coded shorter than 256.
!     2013-06-21  Changed all the nc* (netcdf version 2 C-style) calls to
!                 nf_* (netcdf version 3 Fortran-style) calls.
!
!------------------------------------------------------------------------------
!
CONTAINS
!
!------------------------------------------------------------------------------
!
  subroutine get_max_times_cdf(file,max_times,debug)
!
!-----Finds the number of time periods in the file
!
    implicit none
    include 'netcdf.inc'

    character (len=*), intent(in) :: file
    integer, intent(out)          :: max_times  
    logical, intent(in )          :: debug

    integer cdfid,rcode,id_time
    character (len=80) :: varnam
    integer :: ndims,natts,idims(10),dimids(10)
    integer :: i,ivtype,length
!
!-----Entry point
!
!    cdfid = ncopn(file,NCNOWRIT,rcode)
    rcode = nf_open(file,NF_NOWRITE,cdfid)
    length = max(1,index(file,' ')-1)
    if (rcode == 0) then
       if (debug) write(*,*) 'Open netcdf file ',file(1:length)
    else
       write(*,*) 'Error openiing netcdf file ',file(1:length)
       stop
    end if

!    id_time = ncvid(cdfid,'Times',rcode)
    rcode = nf_inq_varid(cdfid,'Times',id_time)
    rcode = nf_inq_var(cdfid,id_time,varnam,ivtype,ndims,dimids,natts)
    if (debug) write(*,*) 'Number of dims for Time ',ndims
    do i = 1,ndims
       rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
       if (debug) write(*,*) 'Dimension ',i,idims(i)
    enddo
    max_times = idims(2)  ! Get the number of time-stamps in the file

    if (debug) write(*,*) 'Exiting get_max_times_cdf '
!    call ncclos(cdfid,rcode)
    rcode = nf_close(cdfid)

  end subroutine get_max_times_cdf
!
!------------------------------------------------------------------------------
!
  subroutine get_times_cdf(file,times,n_times,debug)
!
!-----Get the time stamps in the file
!
    implicit none
    include 'netcdf.inc'

    integer              :: n_times
    character (len=*)    :: file
    character (len=19)   :: times(n_times)
    logical, intent(in ) :: debug

    character (len=80)   :: varnam
    integer cdfid,rcode,id_time
    integer :: ndims,natts,idims(10),istart(10),iend(10),dimids(10)
    integer :: i,ivtype,length
!
!-----Entry point
!
!    cdfid = ncopn(file,NCNOWRIT,rcode)
    rcode = nf_open(file,NF_NOWRITE,cdfid)
    length = max(1,index(file,' ')-1)
    if (rcode == 0) then
       if (debug) write(*,*) 'Open netcdf file ',file(1:length)
    else
       write(*,*) 'Error openiing netcdf file ',file(1:length)
       stop
    end if

!    id_time = ncvid(cdfid,'Times',rcode)
    rcode = nf_inq_varid(cdfid,'Times',id_time)
    rcode = nf_inq_var(cdfid,id_time,varnam,ivtype,ndims,dimids,natts)
    if (debug) write(*,*) 'Number of dims for Time ',ndims
    do i = 1,ndims
       rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
       if (debug) write(*,*) 'Dimension ',i,idims(i)
    enddo
!
!-----Get the timestamps
!
    do i = 1,idims(2)
       istart(1) = 1
       iend(1) = idims(1)
       istart(2) = i
       iend(2) = 1
       rcode = nf_get_vara_text(cdfid,id_time,istart,iend,times(i))
       length = max(1,index(file,' ')-1)
       if (debug) write(*,*) file(1:length),times(i)(1:19)
    enddo

    if (debug) write(6,*) 'Exiting get_times_cdf '
!    call ncclos(cdfid,rcode)
    rcode = nf_close(cdfid)

  end subroutine get_times_cdf
!
!------------------------------------------------------------------------------
!
  subroutine get_dims_cdf(file,var,dims,ndims,debug)
!
!-----Get the dimensions for a specific variable
!
    implicit none
    include 'netcdf.inc'

    character (len=*), intent(in) :: file
    character (len=*), intent(in) :: var
    logical, intent(in ) :: debug
    integer, intent(out), dimension(4) :: dims
    integer, intent(out) :: ndims

    integer cdfid,rcode,id_time
    character (len=80) :: varnam
    integer :: natts,dimids(10)
    integer :: i,ivtype,length
!
!-----Entry point
!
!    cdfid = ncopn(file,NCNOWRIT,rcode)
    rcode = nf_open(file,NF_NOWRITE,cdfid)
    length = max(1,index(file,' ')-1)
    if (rcode == 0) then
       if (debug) write(*,*) 'Open netcdf file ',file(1:length)
    else
       write(*,*) 'Error openiing netcdf file ',file(1:length)
       stop
    end if

!    id_time = ncvid(cdfid,var,rcode)
    rcode = nf_inq_varid(cdfid,var,id_time)
    rcode = nf_inq_var(cdfid,id_time,varnam,ivtype,ndims,dimids,natts)
    if (debug) then
       write(*,*) 'Number of dims for ',var,' ',ndims
    endif
    do i = 1,ndims
       rcode = nf_inq_dimlen(cdfid,dimids(i),dims(i))
       if (debug) write(*,*) 'Dimension ',i,dims(i)
    enddo

!    call ncclos(cdfid,rcode)
    rcode = nf_close(cdfid)

  end subroutine get_dims_cdf
!
!------------------------------------------------------------------------------
!
  subroutine get_gl_att_int_cdf(file,att_name,value,debug)
!
!-----Get global attributes for integers
!
    implicit none
    include 'netcdf.inc'

    character (len=*), intent(in) :: file
    character (len=*), intent(in) :: att_name
    logical, intent(in ) :: debug
    integer, intent(out) :: value

    integer cdfid,rcode,length
!
!-----Entry point
!
!    cdfid = ncopn(file,NCNOWRIT,rcode)
    rcode = nf_open(file,NF_NOWRITE,cdfid)
    length = max(1,index(file,' ')-1)
    if (rcode == 0) then
       if (debug) write(*,*) 'Open netcdf file ',file(1:length)
    else
       write(*,*) 'Error openiing netcdf file ',file(1:length)
       stop
    end if

    rcode = nf_get_att_int(cdfid,nf_global,att_name,value)

!    call ncclos(cdfid,rcode)
    rcode = nf_close(cdfid)
    if (debug) write(*,*) 'Global attribute ',att_name,' is ',value

  end subroutine get_gl_att_int_cdf
!
!------------------------------------------------------------------------------
!
  subroutine get_gl_att_real_cdf(file,att_name,value,debug)
!
!-----Get global attributes for reals
!
    implicit none
    include 'netcdf.inc'

    character (len=*), intent(in) :: file
    character (len=*), intent(in) :: att_name
    logical, intent(in ) :: debug
    real,    intent(out) :: value

    integer cdfid,rcode,length
!
!-----Entry point
!
!    cdfid = ncopn(file,NCNOWRIT,rcode)
    rcode = nf_open(file,NF_NOWRITE,cdfid)
    length = max(1,index(file,' ')-1)
    if (rcode == 0) then
       if (debug) write(*,*) 'Open netcdf file ',file(1:length)
    else
       write(*,*) 'Error openiing netcdf file ',file(1:length)
       stop
    end if

    rcode = nf_get_att_real(cdfid,nf_global,att_name,value)

!    call ncclos(cdfid,rcode)
    rcode = nf_close(cdfid)
    if (debug) write(*,*) 'Global attribute ',att_name,' is ',value

  end subroutine get_gl_att_real_cdf
!
!------------------------------------------------------------------------------
!
  subroutine get_var_3d_real_cdf(cdfid,var,data,i1,i2,i3,time,debug)
!
!-----Read 3D real variable arrays
!
    implicit none
    include 'netcdf.inc'

    integer, intent(in)  ::  i1,i2,i3,time
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    real, dimension(i1,i2,i3), intent(out) :: data

    integer cdfid,rcode,id_data
    character (len=80) :: varnam
    integer :: ndims,natts,idims(10),istart(10),iend(10),dimids(10)
    integer :: i,ivtype
!
!-----Entry point
!
!    id_data = ncvid(cdfid,var,rcode)
    rcode = nf_inq_varid(cdfid,var,id_data)
    rcode = nf_inq_var(cdfid,id_data,varnam,ivtype,ndims,dimids,natts)
    if (debug) then
       write(*,*) 'Number of dims for ',var,' ',ndims
    endif
    do i = 1,ndims
       rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
       if (debug) write(*,*) 'Dimension ',i,idims(i)
    enddo
!
!-----Check the dimensions
!
    if ( (i1 /= idims(1)) .or.  &
         (i2 /= idims(2)) .or.  &
         (i3 /= idims(3)) .or.  &
         (time > idims(4)) )  then
       write(*,*) 'Error in 3d_var_real read, dimension problem'
       write(*,*) i1,idims(1)
       write(*,*) i2,idims(2)
       write(*,*) i3,idims(3)
       write(*,*) time,idims(4)
       write(*,*) 'Error stop'
       stop
    end if
!
!-----Get the data
!
    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = 1
    iend(3) = i3
    istart(4) = time
    iend(4) = 1

!    call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    rcode = nf_get_vara_real( cdfid,id_data,istart,iend,data)

  end subroutine get_var_3d_real_cdf
!
!------------------------------------------------------------------------------
!
  subroutine get_var_2d_real_cdf(cdfid,var,data,i1,i2,time,debug)
!
!-----Read 2D real variable arrays
!
    implicit none
    include 'netcdf.inc'

    integer, intent(in)  ::  i1,i2,time
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    real, dimension(i1,i2), intent(out) :: data

    integer cdfid,rcode,id_data
    character (len=80) :: varnam
    integer :: ndims,natts,idims(10),istart(10),iend(10),dimids(10)
    integer :: i,ivtype
!
!-----Entry point
!
!    id_data = ncvid(cdfid,var,rcode)
    rcode = nf_inq_varid(cdfid,var,id_data)
    rcode = nf_inq_var(cdfid,id_data,varnam,ivtype,ndims,dimids,natts)
    if (debug) then
       write(*,*) 'Number of dims for ',var,' ',ndims
    endif
    do i = 1,ndims
       rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
       if (debug) write(*,*) 'Dimension ',i,idims(i)
    enddo
!
!-----Check the dimensions
!
    if ( (i1 /= idims(1)) .or.  &
         (i2 /= idims(2)) .or.  &
         (time > idims(3)) )  then
       write(*,*) 'Error in 2d_var_real read, dimension problem'
       write(*,*) i1,idims(1)
       write(*,*) i2,idims(2)
       write(*,*) time,idims(4)
       write(*,*) 'Error stop'
       stop
    end if
!
!-----Get the data
!
    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = time
    iend(3) = 1

!    call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    rcode = nf_get_vara_real( cdfid,id_data,istart,iend,data)

  end subroutine get_var_2d_real_cdf
!
!------------------------------------------------------------------------------
!
  subroutine get_var_2d_int_cdf(cdfid,var,data,i1,i2,time,debug)
!
!-----Read 2D integer variable arrays
!
    implicit none
    include 'netcdf.inc'

    integer, intent(in)  ::  i1,i2,time
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    integer, dimension(i1,i2), intent(out) :: data

    integer cdfid,rcode,id_data
    character (len=80) :: varnam
    integer :: ndims,natts,idims(10),istart(10),iend(10),dimids(10)
    integer :: i,ivtype
!
!-----Entry point
!
!    id_data = ncvid(cdfid,var,rcode)
    rcode = nf_inq_varid(cdfid,var,id_data)
    rcode = nf_inq_var(cdfid,id_data,varnam,ivtype,ndims,dimids,natts)
    if (debug) then
       write(*,*) 'Number of dims for ',var,' ',ndims
    endif
    do i = 1,ndims
       rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
       if (debug) write(*,*) 'Dimension ',i,idims(i)
    enddo
!
!-----Check the dimensions
!
    if( (i1 /= idims(1)) .or.  &
        (i2 /= idims(2)) .or.  &
        (time > idims(3)) )  then
       write(*,*) 'Error in 2d_var_real read, dimension problem'
       write(*,*) i1,idims(1)
       write(*,*) i2,idims(2)
       write(*,*) time,idims(4)
       write(*,*) 'Error stop'
       stop
    end if
!
!-----Get the data
!
    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = time
    iend(3) = 1

!    call ncvgt(cdfid,id_data,istart,iend,data,rcode)
    rcode = nf_get_vara_real( cdfid,id_data,istart,iend,data)

  end subroutine get_var_2d_int_cdf
!
!------------------------------------------------------------------------------
!
  subroutine get_var_1d_real_cdf(cdfid,var,data,i1,time,debug)
!
!-----Read 1D real variable arrays
!
    implicit none
    include 'netcdf.inc'

    integer, intent(in)  ::  i1,time
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    real, dimension(i1), intent(out) :: data

    integer cdfid,rcode,id_data
    character (len=80) :: varnam
    integer :: ndims,natts,idims(10),istart(10),iend(10),dimids(10)
    integer :: i,ivtype
!
!-----Entry point
!
!    id_data = ncvid(cdfid,var,rcode)
    rcode = nf_inq_varid(cdfid,var,id_data)
    rcode = nf_inq_var(cdfid,id_data,varnam,ivtype,ndims,dimids,natts)
    if (debug) then
       write(*,*) 'Number of dims for ',var,' ',ndims
    endif
    do i = 1,ndims
       rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
       if (debug) write(*,*) 'Dimension ',i,idims(i)
    enddo
!
!-----Check the dimensions
!
    if( (i1 /= idims(1)) .or.  &
        (time > idims(2)) )  then
       write(*,*) 'Error in 1d_var_real read, dimension problem'
       write(*,*) i1,idims(1)
       write(*,*) time,idims(4)
       write(*,*) 'Error stop'
       stop
    end if
!
!-----Get the data
!
    istart(1) = 1
    iend(1) = i1
    istart(2) = time
    iend(2) = 1

!    call ncvgt(cdfid,id_data,istart,iend,data,rcode)
    rcode = nf_get_vara_real( cdfid,id_data,istart,iend,data)

  end subroutine get_var_1d_real_cdf
!
!------------------------------------------------------------------------------
!
  subroutine get_var_0d_real_cdf(cdfid,var,data,time,debug)
!
!-----Read 1D real variable arrays
!
    implicit none
    include 'netcdf.inc'

    integer, intent(in)  ::  time
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    real, intent(out)    :: data

    integer cdfid,rcode,id_data
    character (len=80) :: varnam
    integer :: ndims,natts,idims(10),istart(10),iend(10),dimids(10)
    integer :: i,ivtype
!
!-----Entry point
!
!    id_data = ncvid(cdfid,var,rcode)
    rcode = nf_inq_varid(cdfid,var,id_data)
    rcode = nf_inq_var(cdfid,id_data,varnam,ivtype,ndims,dimids,natts)
    if (debug) then
       write(*,*) 'Number of dims for ',var,' ',ndims
    endif
    do i = 1,ndims
       rcode = nf_inq_dimlen(cdfid,dimids(i),idims(i))
       if (debug) write(*,*) 'Dimension ',i,idims(i)
    enddo
!
!-----Check the dimensions
!
    if( (time > idims(1)) )  then
       write(*,*) 'Error in 0d_var_real read, dimension problem:'
       write(*,*) time,idims(1)
       write(*,*) 'Attempting to read ',var
       write(*,*) 'Error stop'
       stop
    end if
!
!-----Get the data
!
    istart(1) = 1
    iend(1) = 1
    istart(2) = time
    iend(2) = 1

!    call ncvgt(cdfid,id_data,istart,iend,data,rcode)
    rcode = nf_get_vara_real( cdfid,id_data,istart,iend,data)

  end subroutine get_var_0d_real_cdf
!
!-------------------------------------------------------------------
!
END MODULE wrf_netcdf
