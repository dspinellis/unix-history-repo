#!/bin/sh -
#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)lorder.sh	5.3 (Berkeley) %G%
#

PATH=/bin:/usr/bin
export PATH

# only one argument is a special case, just output the name twice
case $# in
	0)
		echo "usage: lorder file ...";
		exit ;;
	1)
		echo $1 $1;
		exit ;;
esac

# temporary files
R=/tmp/_reference_$$
S=/tmp/_symbol_$$

# remove temporary files on HUP, INT, QUIT, PIPE, TERM
trap "rm -f $R $S; exit 1" 1 2 3 13 15

# if the line ends in a colon, assume it's the first occurrence of a new
# object file.  Echo it twice, just to make sure it gets into the output.
#
# if the line has " T " or " D " it's a globally defined symbol, put it
# into the symbol file.
#
# if the line has " U " it's a globally undefined symbol, put it into
# the reference file.
nm -go $* | sed "
	/:$/ {
		s/://
		s/.*/& &/
		p
		d
	}
	/ [TD] / {
		s/:.* [TD]//
		w $S
		d
	}
	/ U / {
		s/:.* U//
		w $R
	}
	d
"

# sort symbols and references on the first field (the symbol)
# join on that field, and print out the file names.
sort +1 $R -o $R
sort +1 $S -o $S
join -j 2 -o 1.1 2.1 $R $S
rm -f $R $S
