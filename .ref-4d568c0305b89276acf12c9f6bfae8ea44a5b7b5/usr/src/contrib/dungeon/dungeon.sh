: sh script to invoke the dungeon game on the pdp
DLIB=/usr/games/lib/dunlib
case $# in
	0) $DLIB/listen| $DLIB/dungpdp| $DLIB/speak;;
	1) $DLIB/listen dungeon.sav| $DLIB/dungpdp| $DLIB/speak;;
	*) $DLIB/listen $2| $DLIB/dungpdp| $DLIB/speak;;
esac
