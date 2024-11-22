/* Subroutine SHOWM - Make the mouse visible */
#include <stdio.h>
#include <dos.h>
#define MOUSE 0x33
showm()
{
        union REGS mouse;
        mouse.x.ax = 0x1;
        int86(MOUSE,&mouse,&mouse);
return;
}
