#!/bin/sh -
#
# Copyright (c) 1991 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)mkdep.gcc.sh	5.6 (Berkeley) %G%
#

PATH=/bin:/usr/bin:/usr/ucb
export PATH

D=.depend			# default dependency file is .depend
append=0
pflag=

while :
	do case "$1" in
		# -a appends to the depend file
		-a)
			append=1
			shift ;;

		# -f allows you to select a makefile name
		-f)
			D=$2
			shift; shift ;;

		# the -p flag produces "program: program.c" style dependencies
		# so .o's don't get produced
		-p)
			pflag=p
			shift ;;
		*)
			break ;;
	esac
done

if [ $# = 0 ] ; then
	echo 'usage: mkdep [-p] [-f depend_file] [cc_flags] file ...'
	exit 1
fi

TMP=/tmp/mkdep$$

trap 'rm -f $TMP ; exit 1' 1 2 3 13 15

if [ x$pflag = x ]; then
	cpp -M $* | sed -e 's; \./; ;g' > $TMP
else
	cpp -M $* | sed -e 's;\.o :; :;' -e 's; \./; ;g' > $TMP
fi

if [ $? != 0 ]; then
	echo 'mkdep: compile failed.'
	rm -f $TMP
	exit 1
fi

if [ $append = 1 ]; then
	cat $TMP >> $D
	rm -f $TMP
else
	mv $TMP $D
fi
exit 0
