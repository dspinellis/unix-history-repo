#!/bin/sh
mkdir proc; cd proc
mkdir a b
mv ../b.c a/b.c
touch a.h a.c a/a.h a/c.c a/d.c b/d.c b/e.h b/e.c e.c
(mkmf -F ../C.p; cat Makefile) 2>&1 | diff - ../OCvpath
diffstatus=$?
cd ..; /bin/rm -rf proc C.p
exit $diffstatus
