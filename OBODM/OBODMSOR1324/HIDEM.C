/* Subroutine HIDEM - make the mouse transparent */
#include <stdio.h>
#include <dos.h>
#define MOUSE 0x33
hidem()
{
        union REGS mouse;
        mouse.x.ax = 0x2;
        int86(MOUSE,&mouse,&mouse);
return;
}
