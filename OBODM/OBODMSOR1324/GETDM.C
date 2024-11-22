/* Subroutine GETDM - Get the current video display mode. */
/*      imode - returned mode, see SETDM for values.      */
/*      icol  - returned current number of columns.       */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
getdm(imode,icol)
int *imode, *icol;
{
        union REGS setg;
        setg.h.ah = 0xF;
        int86(VIDEO,&setg,&setg);
        *imode = setg.h.al;
        *icol = setg.h.ah;
return;
}
