#!/bin/sh -
#
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# This module is believed to contain source code proprietary to AT&T.
# Use and redistribution is subject to the Berkeley Software License
# Agreement and your Software Agreement with AT&T (Western Electric).
#
#	@(#)suggest.sh	8.1 (Berkeley) 6/6/93
#

trap 'rm $$; exit' 1 2 3 15
D=/usr/share/dict/explain.d
while echo "phrase?";read x
do
cat >$$ <<dn
/$x.*	/s/\(.*\)	\(.*\)/use "\2" for "\1"/p
dn
case $x in
[a-z]*)
sed -n -f $$ $D; rm $$;;
*) rm $$;;
esac
done
