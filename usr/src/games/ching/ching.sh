#! /bin/sh
#
#	@(#)ching.sh	4.2	(Berkeley)	83/05/19
#
cd /usr/games/lib/ching.d
PATH=:$PATH
case $1 in
	[6-9]*)	H=$1;shift;;
esac
if	test $H
then	phx $H | nroff $* macros - | more -s
else	cno > "/tmp/#$$" 
	echo "  "
	phx < "/tmp/#$$" | nroff $* macros - | more -s
	rm "/tmp/#$$"
fi
