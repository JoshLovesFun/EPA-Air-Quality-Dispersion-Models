/* Function GETBD - Get the type of video board used */
/*  returns graphics board index number.             */
/*          0 - unknown video card.                  */
/*          1 - installed cga card.                  */
/*          2 - installed ega carD.                  */
/*          3 - installed vga card.                  */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
int getbd()
{
        union REGS setg;
        setg.h.ah = 0x1A;
        setg.h.al = 0;
        int86(VIDEO,&setg,&setg);
return setg.h.bl;
}
