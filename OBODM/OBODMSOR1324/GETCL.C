/* Function GETCL - Get the number of mouse clicks        */
/*      function returns number of clicks 0, 1, 2         */
/*      button - 0 for left, 1 for right button.          */
/*      x      - returned horizontal position of mouse    */
/*      y      - returned vertical position of mouse      */
#include <stdio.h>
#include <dos.h>
#define MOUSE 0x33
int getcl(button,x,y)
int button, *x, *y;
{
        union REGS mouse;
        int status, count;
        mouse.x.ax = 0x5;
        mouse.x.bx = button;
        int86(MOUSE,&mouse,&mouse);
        status = mouse.x.ax;
        count = mouse.x.bx;
        *x = mouse.x.cx;
        *y = mouse.x.dx;
return count;
}
