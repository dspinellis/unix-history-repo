#! /bin/sh
#	@(#)tmail.sh	1.3 %G%
# Print out mail backwards.
# Author: Jay Lepreau, Univ of Utah
#
PATH=/usr/contrib/bin:/bin:/usr/bin:/usr/ucb
case $# in
	0) file=/var/mail/$USER
	   ;;
	1) if [ -r /var/mail/$1 ]
	   then
		file=/var/mail/$1
	   else
	   	file=$1
	   fi
	   ;;
	*) echo "Usage: `basename $0` [ username ] [ mboxfile ]"
	   exit 1
	   ;;
esac	
exec tac '-
From ' $file
