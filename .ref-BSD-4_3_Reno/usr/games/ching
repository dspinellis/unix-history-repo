#!/bin/sh -
#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)ching.sh	4.4 (Berkeley) 4/30/90
#

cd /usr/games
DIR=/usr/share/games/ching
case $1 in
	[6-9]*)	H=$1;shift;;
esac
if test $H; then
	./ching.phx $H | nroff $* $DIR/macros - | ${PAGER-more}
else
	./ching.cno > "/tmp/#$$"
	echo "  "
	./ching.phx < "/tmp/#$$" | nroff $* $DIR/macros - | ${PAGER-more}
	rm "/tmp/#$$"
fi
