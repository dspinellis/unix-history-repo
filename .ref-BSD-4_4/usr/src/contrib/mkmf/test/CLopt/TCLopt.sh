#!/bin/sh
mkdir proc; cd proc
mv ../main.c main.c
mkdir tmp; touch tmp/main.h tmp/libtmp.a
(mkmf -F ../C.p; cat Makefile) 2>&1 | \
sed -e 's/	\/lib\/libcurses.a/	\/usr\/lib\/libcurses.a/' | \
diff - ../OCLopt
diffstatus=$?
cd ..; /bin/rm -rf proc C.p
exit $diffstatus
