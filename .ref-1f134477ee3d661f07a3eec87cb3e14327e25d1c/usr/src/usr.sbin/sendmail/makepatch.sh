#!/bin/sh

#
# create a patch file by looking for .OLD files and doing the diffs
#
#	@(#)makepatch.sh	8.1 (Berkeley) %G%
#

ext=${1-.OLD}

for i in `find . -name \*${ext} | sed -e "s/..//" -e "s/${ext}$//" | sort`
do
	diff -c $i${ext} $i
done
