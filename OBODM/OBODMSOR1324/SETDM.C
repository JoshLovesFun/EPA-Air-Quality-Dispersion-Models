/* Subroutine SETDM - Set the video display mode                       */
/*   imode - 0 <= mode <= 7                                            */
/*                                                                     */
/*                 resolution         color      text/graphic          */
/*              -----------------    -------    ---------------------  */
/*   imode = 0   40 col X 25 rows      b//w          text              */
/*         = 1   40 col X 25 rows      color         text              */
/*         = 2   80 col X 25 rows      b/w           text              */
/*         = 3   80 col X 25 rows      color         text              */
/*         = 4  320 col X 200 rows     color        graphic            */
/*         = 5  320 col X 200 rows     b/w          graphic            */
/*         = 6  640 col X 200 rows     b/w          graphic            */
/*         = 7   80 col X 25 rows      b/w           text (monochrome) */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
setdm(imode)
int imode;
{
        union REGS setg;
        setg.h.ah = 0;
        setg.h.al = imode;
        int86(VIDEO,&setg,&setg);
return;
}
