/* Subroutine WRCHR - Write characters to crt screen */
/*   lpage - video page number.                      */
/*   icnum - ASCII character number.                 */
/*   back  - background color (0 - 7).               */
/*   fore  - foreground color (0 - 7).               */
/*   irpet - number of repeat characters.            */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
wrchr(lpage,icnum,back,fore,irpet)
int lpage, icnum, back, fore, irpet;
{
        unsigned char *pntr;
        union REGS setg;
        struct
        {
                fore : 4;
                back : 4;
        } attribute;
        pntr = &attribute;
        attribute.back = back;
        attribute.fore = fore;
        setg.h.ah = 0x9;
        setg.h.al = icnum;
        setg.h.bh = lpage;
        setg.h.bl = *pntr;
        setg.x.cx = irpet;
        int86(VIDEO,&setg,&setg);
return;
}
