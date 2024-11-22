#!/bin/bash
#.........................................................................
# Version "@(#)$Header$"
# EDSS/Models-3 I/O API.  Copyright (C) 2002 MCNC
# Distributed under the GNU Lesser PUBLIC LICENSE version 2.1
# See file "LGPL.txt" for conditions of use.
#.........................................................................
#  Script to take I/O API-style Fortran source that is compatible with both 
#  standard F77/F90 fixed source form and F90 free source form, and produce
#  a file compatible with F77/F90 extended-column fixed source form
#.........................................................................
#  USAGE
#       fix_src.csh <input-file> <output-file>
#.........................................................................
#  Modified to bash 
#.........................................................................

case "$#" in
    2)
        echo "processing files $1 $2"
        ;;
    *)
        echo "Usage:  fix_src.csh <input-file> <output-file>" 
        exit 2 
        ;;
esac

sed -e 's/ *&$//' < $1 > $2

foo=$?

echo foo is ${foo}

if [ ${foo} != 0 ] 
  then
    echo "ERROR ${foo} in script fix_src.csh \n\n"
fi
exit ${foo}

