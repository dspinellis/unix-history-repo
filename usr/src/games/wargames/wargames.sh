#!/bin/sh -
#
# Copyright (c) 1985 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)wargames.sh	5.4 (Berkeley) %G%
#
echo -n "Would you like to play a game? "
read x

if [ -f /usr/games/$x ] ; then
	tput cl
	exec /usr/games/$x
else
	echo "Funny, the only way to win is not to play at all."
fi
exit 0
