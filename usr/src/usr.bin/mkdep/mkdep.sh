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
#	@(#)mkdep.sh	5.13 (Berkeley) %G%
#
PATH=/bin:/usr/bin:/usr/ucb
export PATH

D=.depend			# default dependency file is .depend

while :
	do case "$1" in
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

# If your compiler doesn't have -M, add it.  If you can't, the next two
# lines will try and replace the "cc -M".  The real problem is that this
# hack can't deal with anything that requires a search path, and doesn't
# even try for anything using bracket (<>) syntax.
#
# egrep '^#include[ 	]*".*"' /dev/null $* |
# sed -e 's/:[^"]*"\([^"]*\)".*/: \1/' -e 's/\.c/.o/' |

cc -M $* |
sed "
	s; \./; ;g
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

# copy to preserve permissions
cp $TMP $D
rm -f $TMP
exit 0
