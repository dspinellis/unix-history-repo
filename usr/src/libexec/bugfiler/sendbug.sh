#!/bin/sh -
#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)sendbug.sh	5.9 (Berkeley) %G%
#
# Create a bug report and mail to '4bsd-bugs'.

PATH=/bin:/sbin:/usr/sbin:/usr/bin
export PATH

TEMP=/tmp/bug$$
FORMAT=/usr/share/misc/bugformat

# uucp sites should use ": ${BUGADDR=ucbvax!4bsd-bugs}" with a suitable path.
: ${BUGADDR=4bsd-bugs@BERKELEY.EDU}
: ${EDITOR=vi}

trap 'rm -f $TEMP ; exit 1' 1 2 3 13 15

cp $FORMAT $TEMP
if $EDITOR $TEMP
then
	if cmp -s $FORMAT $TEMP
	then
		echo "File not changed, no bug report submitted."
		exit
	fi
	case "$#" in
	0) sendmail -t -oi $BUGADDR  < $TEMP ;;
	*) sendmail -t -oi "$@" < $TEMP ;;
	esac
fi

rm -f $TEMP
