/*
 Subroutine CSCAN - Sets cursor scan lines (0-15, 0 at top) in text mode
    istop - top scan line (0-15)
    isbot - bottom scan line (128-143) (isbot-128 yields bottom scan line)

    note: any numbers may be used. however, for the systems tested the
          indicated range of istop and isbot yield consistent results. the
          bottom scan line is determined from isbot-128.  if the bottom
          scan line < top scan line (e.g, istop=1, isbot=128), the cursor
          is off (this assumes that the indicated ranges are chosen).
          the standard cursor, which occupies scan lines 13 - 14, is
          obtained with istop = 13 and isbot = 142.
*/
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
cscan(istop,isbot)
int istop, isbot;
{
    union REGS setg;
    setg.h.ah=0x1;
    setg.h.ch=istop;
    setg.h.cl=isbot;
    int86(VIDEO,&setg,&setg);
return;
}
