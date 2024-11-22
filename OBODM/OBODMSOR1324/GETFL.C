/* Subroutine GETFL - Get the names of specified disk files */
/*    path   - files to search for   path/*.ext where ext is*/
/*             the desired file extension name.             */
/*    fnames - returned file names.                         */
/*    numfil - returned number of names in fnames.          */
#include <dir.h>
#include <dos.h>
#include <stdio.h>
#include <stdlib.h>
int getfl(path,fnames,numfil)
char *path, fnames[50][22];
int *numfil;
{
int i, n, done;
struct ffblk ffblk;
        *numfil = 0;
        done = findfirst(path,&ffblk,0);
        for ( n = 0; n < 50; n++ )
        {
                if (abs(done) && _doserrno == 18) return 0; /*no more files*/
                if ( abs(done) ) return -1;                   /* error */
                for ( i = 0; i < 22; i++ ) fnames[n][i] = ' ';
                for ( i = 0; i < 13; i++ ) fnames[n][i] = ffblk.ff_name[i];
                *numfil = n + 1;
                done = findnext(&ffblk);
        }
return 0;
}
