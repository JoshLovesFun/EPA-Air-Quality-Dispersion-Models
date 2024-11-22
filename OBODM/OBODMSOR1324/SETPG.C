/* Subroutine SETPG - Set the display page */
/*     lpage - video page number           */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
setpg(lpage)
int lpage;
{
        union REGS setg;
        setg.h.ah = 0x5;
        setg.h.al = lpage;
        int86(VIDEO,&setg,&setg);
return;
}
