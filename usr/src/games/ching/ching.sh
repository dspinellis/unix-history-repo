#!/bin/sh -
#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)ching.sh	4.3 (Berkeley) %G%
#

cd /usr/games/ching.d
case $1 in
	[6-9]*)	H=$1;shift;;
esac
if test $H; then
	./phx $H | nroff $* macros - | ${PAGER-more}
else
	./cno > "/tmp/#$$"
	echo "  "
	./phx < "/tmp/#$$" | nroff $* macros - | ${PAGER-more}
	rm "/tmp/#$$"
fi
