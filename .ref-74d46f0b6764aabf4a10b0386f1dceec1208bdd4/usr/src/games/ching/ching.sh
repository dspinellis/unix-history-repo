#!/bin/sh -
#
# Copyright (c) 1983, 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.proprietary.sh%
#
#	@(#)ching.sh	4.5 (Berkeley) %G%
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
