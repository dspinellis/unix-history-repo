#!/bin/sh -
#
# Copyright (c) 1985 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that: (1) source distributions retain this entire copyright
# notice and comment, and (2) distributions including binaries display
# the following acknowledgement:  ``This product includes software
# developed by the University of California, Berkeley and its contributors''
# in the documentation or other materials provided with the distribution
# and in all advertising materials mentioning features or use of this
# software. Neither the name of the University nor the names of its
# contributors may be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)wargames.sh	5.4 (Berkeley) 6/1/90
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
