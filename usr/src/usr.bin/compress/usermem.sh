#!/bin/sh -
#
# Copyright (c) 1985 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# James A. Woods, derived from original work by Spencer Thomas
# and Joseph Orost.
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
#	@(#)usermem.sh	5.6 (Berkeley) %G%
#
: This shell script snoops around to find the maximum amount of available
: user memory.  These variables need to be set only if there is no
: /usr/adm/messages.  KMEM, UNIX, and CLICKSIZE can be set on the command
: line, if desired, e.g. UNIX=/unix
KMEM=/dev/kmem		# User needs read access to KMEM
UNIX=
# VAX			CLICKSIZE=512,	UNIX=/vmunix
# PDP-11		CLICKSIZE=64,	UNIX=/unix
# CADLINC 68000		CLICKSIZE=4096,	UNIX=/unix
# Perkin-Elmer 3205	CLICKSIZE=4096,	UNIX=/edition7
# Perkin-Elmer all others, CLICKSIZE=2048, UNIX=/edition7
CLICKSIZE=512
eval $*

if test -n "$UNIX"
then
    : User must have specified it already.
elif test -r /vmunix
then
    UNIX=/vmunix
    CLICKSIZE=512	# Probably VAX
elif test -r /edition7
then
    UNIX=/edition7
    CLICKSIZE=2048	# Perkin-Elmer: change to 4096 on a 3205
elif test -r /unix
then
    UNIX=/unix		# Could be anything
fi

SIZE=0
# messages: probably the most transportable
if test -r /var/log/messages -a -s /var/log/messages
then
    SIZE=`grep avail /var/log/messages | sed -n '$s/.*[ 	]//p'`
fi

if test 0$SIZE -le 0		# no SIZE in /usr/adm/messages
then
    if test -r $KMEM		# Readable KMEM
    then
	if test -n "$UNIX"
	then
	    SIZE=`echo maxmem/D | adb $UNIX $KMEM | sed -n '$s/.*[ 	]//p'`
	    if test 0$SIZE -le 0
	    then
		SIZE=`echo physmem/D | adb $UNIX $KMEM | sed -n '$s/.*[ 	]//p'`
	    fi
	    SIZE=`expr 0$SIZE '*' $CLICKSIZE`
	fi
    fi
fi

case $UNIX in
    /vmunix)		# Assume 4.2bsd: check for resource limits
	MAXSIZE=`csh -c limit | awk 'BEGIN	{ MAXSIZE = 1000000 }
/datasize|memoryuse/ && NF == 3	{ if ($2 < MAXSIZE) MAXSIZE = $2 }
END	{ print MAXSIZE * 1000 }'`
	if test $MAXSIZE -lt $SIZE
	then
	    SIZE=$MAXSIZE
	fi
	;;
esac

if test 0$SIZE -le 0
then
    echo 0;exit 1
else
    echo $SIZE
fi
