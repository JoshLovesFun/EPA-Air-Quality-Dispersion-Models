/* Subroutine CURSP - Sets the cursor position      */
/*    lpage - video page number.                    */
/*    ix    - horizontal position of cursor, column */
/*    iy    - vertical position of cursor, row      */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
cursp(lpage,ix,iy)
int lpage, ix , iy;
{
        union REGS setg;
        setg.h.ah=2;
        setg.h.dh=iy;
        setg.h.dl=ix;
        setg.h.bh=lpage;
        int86(VIDEO,&setg,&setg);
return;
}
