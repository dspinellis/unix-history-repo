#! /bin/sh
#
#	@(#)explain.sh	4.6	(Berkeley)	89/05/11
#
D=/usr/share/dict/explain.d
while	echo 'phrase?'
	read x
do
	case $x in
	[a-z]*)	sed -n /"$x"'.*	/s/\(.*\)	\(.*\)/use "\2" for "\1"/p' $D
	esac
done
