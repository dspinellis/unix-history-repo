#!/bin/sh -
#
# Copyright (c) 1983, 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# This module is believed to contain source code proprietary to AT&T.
# Use and redistribution is subject to the Berkeley Software License
# Agreement and your Software Agreement with AT&T (Western Electric).
#
#	@(#)ching.sh	8.1 (Berkeley) 5/31/93
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
