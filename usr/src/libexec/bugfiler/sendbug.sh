#!/bin/sh -
#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)sendbug.sh	5.7 (Berkeley) %G%
#
# Create a bug report and mail to '4bsd-bugs'.

TEMP=/tmp/bug$$
FORMAT=/usr/lib/bugformat

# uucp sites should use ": ${BUGADDR=ucbvax!4bsd-bugs}" with a suitable path.
: ${BUGADDR=4bsd-bugs@BERKELEY.EDU}
: ${EDITOR=/usr/ucb/vi}

trap '/bin/rm -f $TEMP ; exit 1' 1 2 3 13 15

/bin/cp $FORMAT $TEMP
if $EDITOR $TEMP
then
	if cmp -s $FORMAT $TEMP
	then
		echo "File not changed, no bug report submitted."
		exit
	fi
	case "$#" in
	0) /usr/lib/sendmail -t -oi $BUGADDR  < $TEMP ;;
	*) /usr/lib/sendmail -t -oi "$@" < $TEMP ;;
	esac
fi

/bin/rm -f $TEMP
