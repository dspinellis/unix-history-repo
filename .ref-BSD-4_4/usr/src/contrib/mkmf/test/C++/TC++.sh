#!/bin/sh
mkdir h proc
mv convert.l error.h globs.h parser.y prog.C print.cc scanner.l tab.h proc
touch proc/\#old.c
mv cons.h h
(cd proc; mkmf -F ../C++.p C++FLAGS=-I../h; cat Makefile) 2>&1 | diff - OC++
diffstatus=$?
/bin/rm -rf h proc C++.p
exit $diffstatus
