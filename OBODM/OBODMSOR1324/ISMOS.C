/* Subroutine ISMOS - See if mouse is present        */
/*     buttons - returned number of buttons on mouse */
#include <stdio.h>
#include <dos.h>
#define MOUSE 0x33
int ismos(buttons)
int *buttons;
{
        union REGS mouse;
        int iret;
        mouse.x.ax = 0;
        int86(MOUSE,&mouse,&mouse);
        *buttons = mouse.x.bx;
        iret = mouse.x.ax;
return iret;
}
