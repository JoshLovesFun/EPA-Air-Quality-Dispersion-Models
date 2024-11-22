/* Subroutine GETML - Get the current mouse location */
/*     ix - returned horizontal position of mouse.   */
/*     iy - returned vertical position of mouse.     */
#include <stdio.h>
#include <dos.h>
#define MOUSE 0x33
getml(ix,iy)
int *ix, *iy;
{
        union REGS mouse;
        mouse.x.ax = 0x3;
        int86(MOUSE,&mouse,&mouse);
        *ix = mouse.x.cx;
        *iy = mouse.x.dx;
return;
}
