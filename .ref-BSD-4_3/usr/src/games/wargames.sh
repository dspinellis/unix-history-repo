#!/bin/sh -
#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)wargames.sh	5.1 (Berkeley) 6/23/85
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
