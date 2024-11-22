/* Subroutine SETOV - */
/* Set overscan region color or graphics border               */
/*   icolr - value to be placed in overscan register (0 - 63) */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
setov(icolr)
int icolr;
{
        union REGS setg;
        setg.h.ah = 0x10;
        setg.h.al = 0x1;
        setg.h.bl = 0;
        setg.h.bh = icolr;
        int86(VIDEO,&setg,&setg);
return;
}
