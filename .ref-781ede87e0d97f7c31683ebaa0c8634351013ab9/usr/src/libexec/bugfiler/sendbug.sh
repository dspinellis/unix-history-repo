#!/bin/sh -
#
# Copyright (c) 1983 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)sendbug.sh	5.11 (Berkeley) %G%
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
