/* Subroutine RDKEY - returns the keyboard depressed key ASCII number */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define KEYBOARD 0x16
void wrkey(iscan,ichar)
int iscan, ichar;
{
		union REGS setg;
		setg.h.ah = 0x5;
		setg.h.ch = iscan;
		setg.h.cl = ichar;
		int86(KEYBOARD,&setg,&setg);
return;
}
