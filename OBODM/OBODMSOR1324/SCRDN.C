/* Subroutine SCRDN - Scroll the crt screen down                    */
/*   nline - number of lines to blank, starting at bottom of scroll */
/*           window.  if nline = 0, then entire window is blanked.  */
/*   ihrow - text row at top of scrolled window                     */
/*   lwrow - text row at bottom of scrolled window.                 */
/*   lfcol - text column at left hand side of scrolled window.      */
/*   ircol - text column at right hand side of scrolled window.     */
/*   back  - background color in blanked region (0 - 15 (8 - 15 give*/
/*           blinking foreground))                                  */
/*   fore  - foreground color in blanked region.                    */
#include <stdio.h>
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#include <stdlib.h>
#include <alloc.h>
#define VIDEO 0x10
scrdn(nline,ihrow,lwrow,lfcol,ircol,back,fore)
int nline, ihrow, lwrow, lfcol, ircol, back, fore;
{
        unsigned char *pntr;
        union REGS setg;
        struct ATTRIBUTES
        {
        fore  : 4;
        back  : 4;
        }  attr;
        pntr = &attr;
        attr.back = back;
        attr.fore = fore;
        setg.h.ah = 0x7;
        setg.h.al = nline;
        setg.h.ch = ihrow;
        setg.h.dh = lwrow;
        setg.h.cl = lfcol;
        setg.h.dl = ircol;
        setg.h.bh = *pntr;
        int86(VIDEO,&setg,&setg);
return;
}
