/* Subroutine RDSTA - get keyboard status */
#include <stdio.h>
#include <dos.h>
#define KEYBOARD 0x16
rdsta(flag1,flag2,flag3)
int *flag1, *flag2, *flag3;
{
        union REGS key;
        key.h.ah = 0x1;
        int86(KEYBOARD,&key,&key);
        *flag1 = key.x.cflag;
        *flag2 = key.x.flags;
        *flag3 = key.h.ah;
return;
}
