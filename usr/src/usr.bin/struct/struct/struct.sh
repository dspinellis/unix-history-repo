#!/bin/sh -
#
# This module is believed to contain source code proprietary to AT&T.
# Use and redistribution is subject to the Berkeley Software License
# Agreement and your Software Agreement with AT&T (Western Electric).
#
#	@(#)struct.sh	8.1 (Berkeley) 6/6/93
#

trap "rm -f /tmp/struct*$$" 0 1 2 3 13 15
files=no
for i
do
	case $i in
	-*)	;;
	*)	files=yes
	esac
done

case $files in
yes)
	/usr/libexec/structure $* >/tmp/struct$$
	;;
no)
	cat >/tmp/structin$$
	/usr/libexec/structure /tmp/structin$$ $* >/tmp/struct$$
esac &&
	/usr/libexec/beautify</tmp/struct$$
