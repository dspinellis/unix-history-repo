#!/bin/sh -
#
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.proprietary.sh%
#
#	@(#)suggest.sh	5.3 (Berkeley) %G%
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
