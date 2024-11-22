      subroutine regress(x,y,z,ndata)

c-----REGRESS determines a linear least-squares fit of predictions Y to
c     observations X (Y = bX + a)

      implicit none

      integer ndata
      real x(ndata), y(ndata), z(ndata)

      integer i
      real sx,sy,st2,b,xx,ss,sxoss,t,a

      sx = 0.
      sy = 0.
      st2 = 0.
      b = 0.

c-----Accumulate sums without weights

      do i = 1,ndata
        sx = sx + x(i)
        sy = sy + y(i)
      enddo
      ss = float(ndata)
      sxoss = sx/ss

      do i = 1,ndata
        t = x(i) - sxoss
        st2 = st2 + t*t
        b = b + t*y(i)
      enddo

c-----Solve for a and b

      b = b/st2
      a = (sy - sx*b)/ss

c-----Calculate regressed predictions Z

      do i = 1,ndata
        z(i) = b*x(i) + a
      enddo

      return
      end
