#! /bin/sh
#	@(#)tmail.sh	1.2 10/17/85
# Print out mail backwards.
# Author: Jay Lepreau, Univ of Utah
#
PATH=/usr/new:/bin:/usr/bin:/usr/ucb
case $# in
	0) file=/usr/spool/mail/$USER
	   ;;
	1) if [ -r /usr/spool/mail/$1 ]
	   then
		file=/usr/spool/mail/$1
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
