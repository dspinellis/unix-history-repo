#! /bin/sh
#
#	@(#)explain.sh	4.5	(Berkeley)	83/05/27
#
D=/usr/lib/explain.d
while	echo 'phrase?'
	read x
do
	case $x in
	[a-z]*)	sed -n /"$x"'.*	/s/\(.*\)	\(.*\)/use "\2" for "\1"/p' $D
	esac
done
