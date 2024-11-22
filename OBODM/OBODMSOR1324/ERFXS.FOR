      Function ERFXS(arg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ERFXS Version 01.0                          ---
CC--- Subroutine description - calculate the error function of arg. ---
CC---------------------------------------------------------------------
CC
CC********************************************************************
CC
      double precision a,b,c,d,e,f,ERFXS,arg,x
CC
      data a/0.0705230784/,b/0.0422820123/,c/0.0092705272/,
     *     d/0.0001520143/,e/0.0002765672/,f/0.0000430638/
CC
CC********************************************************************
CC
      if (arg .gt. 0.0) then
        n = 1
        x = arg
      else if (arg .lt. 0.0) then
        n = -1
        x = -arg
      else
        n = 0
        ERFXS = 0.0
      end if
      if (n .ne. 0) then
        if (x .gt. 5.0) then
          ERFXS = 1.0
        else if (x .lt. 1.0e-5) then
          ERFXS = 1.12837917 * x
        else
          ERFXS = 1.0 - (1.0 + x * (a + x * (b + x * (c + x * (d + x *
     *           (e + x * f))))))**(-16)
        end if
        ERFXS = n * ERFXS
      end if
      return
      end
