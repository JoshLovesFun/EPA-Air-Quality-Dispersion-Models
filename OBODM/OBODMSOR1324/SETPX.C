/* Subroutine SETPX - Sets the foreground color of a pixel           */
/*                    icol  - horizontal pixel number.               */
/*                    irow  - vertical pixel number.                 */
/*                    icolr - color index from the current palette.  */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
setpx(icol,irow,icolr)
int icol, irow, icolr;
{
        union REGS gpix;
        gpix.h.ah = 0x0C;
        gpix.h.bh = 0x00;
        gpix.h.al = icolr;
        gpix.x.cx = icol;
        gpix.x.dx = irow;
        int86(VIDEO,&gpix,&gpix);
return;
}
