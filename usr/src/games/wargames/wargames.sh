#!/bin/sh -
#
# Copyright (c) 1980 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)wargames.sh	5.3 (Berkeley) %G%
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
