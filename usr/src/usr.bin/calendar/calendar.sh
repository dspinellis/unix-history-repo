#!/bin/sh -
#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)calendar.sh	4.7 (Berkeley) 3/27/88
#
PATH=/usr/lib:/bin:/usr/bin:/usr/ucb:/lib	# order important

tmp=/tmp/cal0$$
trap "rm -f $tmp /tmp/cal1$$ /tmp/cal2$$"
trap exit 1 2 13 15
calendar >$tmp

if [ $# = 0 ]; then
	trap "rm -f $tmp ; exit" 0 1 2 13 15
	(cpp calendar | egrep -f $tmp)
	exit 0
fi

if [ $# = 1 ] && [ $1 = "-" ]; then
	trap "rm -f $tmp /tmp/cal1$$ /tmp/cal2$$; exit" 0 1 2 13 15
	echo -n "Subject: Calendar for " > /tmp/cal1$$
	date | sed -e "s/ [0-9]*:.*//" >> /tmp/cal1$$
	sed '
		s/\([^:]*\):.*:\(.*\):[^:]*$/y=\2 z=\1/
	' /etc/passwd \
	| while read x
	do
		eval $x
		if test -r $y/calendar
		then
			(cpp $y/calendar | egrep -f $tmp) 2>/dev/null > /tmp/cal2$$
			if test -s /tmp/cal2$$
			then
				cat /tmp/cal1$$ /tmp/cal2$$ | mail $z
			fi
		fi
	done
	exit 0
fi

echo "usage: calendar [-]"
exit 1
