#! /bin/sh
#	@(#)tmail.sh	1.1 %G%
# Print out mail backwards.
# Author: Jay Lepreau, Univ of Utah
#
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
	*) echo "Usage: $0 [ username ] [ mboxfile ]"
	   exit 1
	   ;;
esac	
exec tac '-
From ' $file
