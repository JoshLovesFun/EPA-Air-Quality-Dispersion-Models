/* Subroutine WAITM - Time delay between mouse clicks */
#include <stdio.h>
#include <dos.h>
#define TIME 0x15
waitm(microseclo,microsechi)
int microseclo,microsechi;
{
        union REGS time;
        time.h.ah = 0x86;
        time.h.al = 0;
        time.x.cx = microsechi;
        time.x.dx = microseclo;
        int86(TIME,&time,&time);
return;
}
