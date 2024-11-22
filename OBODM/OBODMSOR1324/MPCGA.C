/* Subroutine MPCGA - selects palette and sets its background       */
/* color. (CGA)                                                     */
/*   ipalt - palette number (0 - 1).                                */
/*           For Palette 1, the foreground colors are green, red    */
/*           and yellow-brown.  These correspond to foreground      */
/*           colors 1, 2, and 3, respectively.  For palette 0, the  */
/*           foreground colors are cyan, magenta, and white.  The   */
/*           corresponding foreground colors are again 1, 2 and 3.  */
/*   iback - background color number (0 - 15), see CLRSC for colors.*/
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
mpcga(ipalt,iback)
int ipalt, iback;
{
        union REGS setg;
        setg.h.ah = 0xB;
        setg.h.bh = ipalt;
        setg.h.bl = iback;
        int86(VIDEO,&setg,&setg);
return;
}
