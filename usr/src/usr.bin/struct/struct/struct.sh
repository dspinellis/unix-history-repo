#!/bin/sh -
#
# %sccs.include.proprietary.sh%
#
#	@(#)struct.sh	4.4 (Berkeley) %G%
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
