/* Subroutine RDCHR - Read a character from the crt screen */
/*   lpage - video page number.                            */
/*   icnum - returnee ASCII character number.              */
/*   back  - returned background color index (0 - 7).      */
/*   fore  - returned foreground color index (0 - 7).      */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
rdchr(lpage,icnum,back,fore)
int lpage, *icnum, *back, *fore;
{
        unsigned char *pntr;
        union REGS setg;
        struct
        {
                fore : 4;
                back : 4;
        } attribute;
        pntr = &attribute;
        setg.h.ah = 0x8;
        setg.h.bh = lpage;
        int86(VIDEO,&setg,&setg);
        *pntr = setg.h.ah;
        *icnum = setg.h.al;
        *back = attribute.back;
        *fore = attribute.fore;
return;
}
