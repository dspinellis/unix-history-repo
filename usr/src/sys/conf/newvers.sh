#!/bin/sh -
#
# Copyright (c) 1984,1986,1990,1993 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)newvers.sh	7.7 (Berkeley) %G%
#

if [ ! -r version ]
then
	echo 0 > version
fi

touch version
v=`cat version` u=${USER-root} d=`pwd` h=`hostname` t=`date`
echo "char ostype[] = \"4.4BSD\";" > vers.c
echo "char osrelease[] = \"4.4BSD\";" >> vers.c
echo "char sccs[4] = { '@', '(', '#', ')' };" >>vers.c
echo "char version[] = \"4.4BSD UNIX #${v}: ${t}\\n    ${u}@${h}:${d}\\n\";" >>vers.c

echo `expr ${v} + 1` > version
