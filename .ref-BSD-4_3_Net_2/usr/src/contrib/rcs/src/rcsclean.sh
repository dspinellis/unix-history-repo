#! /bin/sh
#
# RCS cleanup operation.
# $Header: /arthur/src/local/bin/rcs/src/RCS/rcsclean.sh,v 1.5 86/07/03 13:09:55 jdl Exp $
#
# This program removes working files which are copies of the latest
# revision on the default branch of the corresponding RCS files.
# For each file given, rcsclean performs a co operation for the latest
# revision on the default branch, and compares
# the result with the working file. If the two are identical,
# the working file is deleted.
#
# A typical application in a Makefile would be:
# clean:;       rm *.o; rcsclean *.c *.o
#
# Limitation: This program doesn't work if given the name of
# an RCS file rather than the name of the working file.

PATH=/usr/new/bin:/usr/local/bin:/bin:/usr/bin:/usr/ucb
export PATH
progname=$0
if [ $# = 0 ] ; then
    echo        "usage: $progname file ..."
    echo        "removes all working files that are checked in and are unchanged"
    exit  0
fi
TMPFILE=/tmp/rcscl$$.tmp
while test $# -gt 0 ; do
    if test -f $1 ; then
	co -p -q $1 > $TMPFILE
	if [ $? = 0 ] ; then
	    cmp -s $1 $TMPFILE
	    if [ $? = 0 ] ; then
		chmod +w $1; rm -f $1; rcs -u -q $1
	    fi
	fi
    fi
    shift
done
rm -f $TMPFILE
