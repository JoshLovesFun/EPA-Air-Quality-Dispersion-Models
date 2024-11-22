/* Subroutine GTRGC - obtains colors in palette registers   */
/*   iprnx - pallette register index (0 - 15)               */
/*   icolr - value of color in register (0 - 63)            */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
gtrgc(iprnx,icolr)
int iprnx, *icolr;
{
      union REGS setg;
      setg.h.ah = 0x10;
      setg.h.al = 0x7;
      setg.h.bl = iprnx;
      int86(VIDEO,&setg,&setg);
      *icolr = setg.h.bh;
return;
}
