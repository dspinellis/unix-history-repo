#!/bin/sh
mkdir proc; cd proc
mkdir a b c
touch tmp.c b.c a/c.c a/d.c b/d.c b/e.c c/f.c
ln -s tmp.c a.c; rm tmp.c
ln -s b/d.c d.c
ln -s c/f.c f.c
(mkmf -S -F ../C.p; cat Makefile) 2>&1 | diff - ../OCsym
diffstatus=$?
cd ..; /bin/rm -rf proc C.p
exit $diffstatus
