/* Subroutine MPVGA - Set foreground and background colors */
/*   iprnx - palette register index (0 - 15)               */
/*   icolr - value to be placed in register (0 - 63)       */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
/*  MPVGA sets color in registers (ega-vga) */
mpvga(iprnx,icolr)
{
        union REGS setg;
        setg.h.ah = 0x10;
        setg.h.al = 0x0;
        setg.h.bl = iprnx;
        setg.h.bh = icolr;
        int86(VIDEO,&setg,&setg);
return;
}
