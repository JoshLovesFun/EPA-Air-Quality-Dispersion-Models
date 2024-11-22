#!/bin/csh
if ( [ "$1" == "-h" ] || [ "$1" == "--help" ] ) ; then
    echo "Usage: compile_mmif_windows.sh"
    echo "Used to compile MMIF under MinGW, for Windows"
    exit
fi

make --file=makefile.windows clean >& /dev/null 2>&1
make --file=makefile.windows >& compile_mmif_windows.log 2>&1

rm *.o *.mod # clean up after compilation
