#!/bin/sh
(mkmf -F Fortran.mkmf LD=f77; cat Makefile) 2>&1 | diff - OFortran
diffstatus=$?
/bin/rm -f Makefile program.f a.f b.h defs Fortran.mkmf
exit $diffstatus
