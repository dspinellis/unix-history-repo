#! /bin/sh
# mksyswrite.sh	1.3	88/05/21

mach=`machine`
/lib/cpp /usr/src/lib/libc/$mach/sys/write.s |sed 's/_write/_syswrite/g' >syswrite.s
