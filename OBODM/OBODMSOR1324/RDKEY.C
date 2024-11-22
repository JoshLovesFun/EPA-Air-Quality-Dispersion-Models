/* Subroutine RDKEY - returns the keyboard depressed key ASCII number */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define KEYBOARD 0x21
int rdkey()
{
        union REGS setg;
        setg.h.ah = 0x8;
        int86(KEYBOARD,&setg,&setg);
return setg.h.al;
}
