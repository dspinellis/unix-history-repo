#!/bin/sh -
#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)mkdep.sh	5.17 (Berkeley) %G%
#
PATH=/bin:/usr/bin:/usr/ucb
export PATH

D=.depend			# default dependency file is .depend
append=0

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
			SED='s;\.o;;'
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

cc -M $* |
sed "
	s; \./; ;g
	/\.c:$/d
	$SED" |
awk '{
	if ($1 != prev) {
		if (rec != "")
			print rec;
		rec = $0;
		prev = $1;
	}
	else {
		if (length(rec $2) > 78) {
			print rec;
			rec = $0;
		}
		else
			rec = rec " " $2
	}
}
END {
	print rec
}' > $TMP

if [ $append = 1 ]; then
	cat $TMP >> $D
	rm -f $TMP
else
	mv $TMP $D
fi
exit 0
