#!/bin/sh -
#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)mkdep.sh	5.8 (Berkeley) %G%
#

PATH=/bin:/usr/bin:/usr/ucb
export PATH

if [ $# = 0 ] ; then
	echo 'usage: mkdep [-p] [-f makefile] [flags] file ...'
	exit 1
fi

MAKE=Makefile			# default makefile name is "Makefile"
case $1 in
	# -f allows you to select a makefile name
	-f)
		MAKE=$2
		shift; shift ;;

	# the -p flag produces "program: program.c" style dependencies
	# so .o's don't get produced
	-p)
		SED='-e s;\.o;;'
		shift ;;
esac

if [ ! -w $MAKE ]; then
	echo "mkdep: no writeable file \"$MAKE\""
	exit 1
fi

TMP=/tmp/mkdep$$

trap 'rm -f $TMP ; exit 1' 1 2 3 13 15

cp $MAKE ${MAKE}.bak

sed -e '/DO NOT DELETE THIS LINE/,$d' < $MAKE > $TMP

cat << _EOF_ >> $TMP
# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

_EOF_

cc -M $* | /bin/sed -e "s; \./; ;g" $SED | \
	awk ' { \
		if ($1 != prev) { \
			if (rec != "") \
				print rec; rec = $0; prev = $1; \
		} \
		else { \
			if (length(rec $2) > 78) { \
				print rec; rec = $0; \
			} else \
				rec = rec " " $2 \
		} \
	} \
	END { \
		print rec \
	} ' >> $TMP

cat << _EOF_ >> $TMP

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
_EOF_

# copy to preserve permissions
cp $TMP $MAKE
rm -f ${MAKE}.bak $TMP
exit 0
