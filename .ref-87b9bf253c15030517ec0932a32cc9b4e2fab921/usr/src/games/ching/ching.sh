#! /bin/sh
#
#	@(#)ching.sh	4.1	(Berkeley)	82/10/27
#
cd /usr/games/lib/ching.d
PATH=:$PATH
case $1 in
	[6-9]*)	H=$1;shift;;
esac
if	test $H
then	phx $H | nroff $* macros -
else	cno | phx | nroff $* macros -
fi
