/* Subroutine CLRSC - Clears video screen                            */
/*    back - background color number (0 - 15)                        */
/*    fore - foreground color number (0 - 15)                        */
/*                                                                   */
/*        color value    color          color value    color         */
/*        -----------   ------------  ------------ ---------------   */
/*             0         black             8         black           */
/*             1         blue              9         light blue      */
/*             2         green            10         light green     */
/*             3         cyan             11         light cyan      */
/*             4         red              12         light red       */
/*             5         magenta          13         light magenta   */
/*             6         yellow-brown     14         yellow          */
/*             7         white            15   high intensity white  */
/*    icolmx - maximum number of screen comlumns.                    */
/*    irowmx - maximum number of screen rows.                        */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
clrsc(back,fore,icolmx,irowmx)
int back, fore, icolmx, irowmx;
{
        unsigned char *pntr;
        union REGS setg;
        struct ATTRIBUTES
        {
        fore  : 4;
        back  : 4;
        }  attr;
        pntr = &attr;
        attr.back = back;
        attr.fore = fore;
        setg.h.ah = 0x6;
        setg.h.al = 0x0;
        setg.h.bh = *pntr;
        setg.h.ch = 0;
        setg.h.cl = 0;
        setg.h.dh = irowmx - 1;
        setg.h.dl = icolmx - 1;
        int86(VIDEO,&setg,&setg);
return;
}
