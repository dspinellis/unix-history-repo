#!/bin/sh
mkdir include libhash
touch include/hash.h
mv *.c libhash
(cd libhash; mkmf -F ../C.l CFLAGS="-I../include -O" DEST=../lib LIBRARY=libhash.a; \
cat Makefile) 2>&1 | diff - OClib
diffstatus=$?
/bin/rm -rf include libhash C.l
exit $diffstatus
