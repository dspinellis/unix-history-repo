#!/bin/awk -f
#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Van Jacobson.
#
# %sccs.include.redist.sh%
#
#	@(#)mean.awk	5.2 (Berkeley) %G%
#
/^ *[0-9]/	{
	# print out the average time to each hop along a route.
	tottime = 0; n = 0;
	for (f = 5; f <= NF; ++f) {
		if ($f == "ms") {
			tottime += $(f - 1)
			++n
		}
	}
	if (n > 0)
		print $1, tottime/n, median
}
