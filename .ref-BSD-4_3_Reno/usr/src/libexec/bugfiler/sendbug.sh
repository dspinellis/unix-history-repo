#!/bin/sh -
#
# Copyright (c) 1983 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted provided
# that: (1) source distributions retain this entire copyright notice and
# comment, and (2) distributions including binaries display the following
# acknowledgement:  ``This product includes software developed by the
# University of California, Berkeley and its contributors'' in the
# documentation or other materials provided with the distribution and in
# all advertising materials mentioning features or use of this software.
# Neither the name of the University nor the names of its contributors may
# be used to endorse or promote products derived from this software without
# specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)sendbug.sh	5.11 (Berkeley) 7/25/90
#

# create a bug report and mail it to '4bsd-bugs'.

PATH=/bin:/sbin:/usr/sbin:/usr/bin
export PATH

TEMP=/tmp/bug$$
FORMAT=/usr/share/misc/bugformat

# uucp sites should use ": ${BUGADDR=ucbvax!4bsd-bugs}" with a suitable path.
: ${BUGADDR=4bsd-bugs@BERKELEY.EDU}
: ${EDITOR=vi}

trap 'rm -f $TEMP ; exit 1' 1 2 3 13 15

cp $FORMAT $TEMP
chmod u+w $TEMP
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
