#!/bin/sh
(mkmf -F Pascal.mkmf LD=pc; cat Makefile) 2>&1 | diff - OPascal
diffstatus=$?
/bin/rm -f Makefile main.p a.p b.h defs imports Pascal.mkmf
exit $diffstatus
