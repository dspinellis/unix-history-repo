#! /bin/sh
# mksyswrite.sh	1.2	87/02/15

mach=`ls -l /usr/include/machine | sed -e 's/.*-> \.\///' -e 's/.*-> //'`
/lib/cpp /usr/src/lib/libc/$mach/sys/write.c |sed 's/_write/_syswrite/g' >syswrite.s
