#!/bin/awk -f
#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Van Jacobson.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)median.awk	5.2 (Berkeley) 4/28/90
#
/^ *[0-9]/	{
	# print out the median time to each hop along a route.
	tottime = 0; n = 0;
	for (f = 5; f <= NF; ++f) {
		if ($f == "ms") {
			++n
			time[n] = $(f - 1)
		}
	}
	if (n > 0) {
		# insertion sort the times to find the median
		for (i = 2; i <= n; ++i) {
			v = time[i]; j = i - 1;
			while (time[j] > v) {
				time[j+1] = time[j];
				j = j - 1;
				if (j < 0)
					break;
			}
			time[j+1] = v;
		}
		if (n > 1 && (n % 2) == 0)
			median = (time[n/2] + time[(n/2) + 1]) / 2
		else
			median = time[(n+1)/2]

		print $1, median
	}
}
