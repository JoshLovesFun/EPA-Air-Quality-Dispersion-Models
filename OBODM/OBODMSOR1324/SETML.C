/* Subroutine SETML - Specify the mouse location */
/*      x - horizontal position of mouse.        */
/*      y - vertical position of mouse.          */
#include <stdio.h>
#include <dos.h>
#define MOUSE 0x33
setml(x,y)
int x, y;
{
        union REGS mouse;
        mouse.x.ax = 0x4;
        mouse.x.cx = x;
        mouse.x.dx = y;
        int86(MOUSE,&mouse,&mouse);
return;
}
