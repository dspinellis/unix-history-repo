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
#	@(#)wargames.sh	5.2 (Berkeley) %G%
#
echo -n "Would you like to play a game? "
read x

case $x in

adventure)
	exec /usr/games/$x
	;;

backgammon)
	exec /usr/games/$x
	;;

boggle)
	exec /usr/games/$x
	;;

canfield)
	exec /usr/games/$x
	;;

chess)
	exec /usr/games/$x
	;;

cribbage)
	exec /usr/games/$x
	;;

fish)
	exec /usr/games/$x
	;;

fortune)
	exec /usr/games/$x
	;;

hangman)
	exec /usr/games/$x
	;;

mille)
	exec /usr/games/$x
	;;

monop)
	exec /usr/games/$x
	;;

rogue)
	exec /usr/games/$x
	;;

snake)
	exec /usr/games/$x
	;;


trek)
	exec /usr/games/$x
	;;

wump)
	exec /usr/games/$x
	;;

zork)
	exec /usr/games/$x
	;;

*)
	echo "Funny, the only way to win is not to play at all"
	;;
esac
exit 0
