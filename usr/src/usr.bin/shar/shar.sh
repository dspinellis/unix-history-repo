#!/bin/sh -
#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)shar.sh	5.3 (Berkeley) %G%
#

if [ $# -eq 0 ]; then
	echo 'usage: shar file ...'
	exit 1
fi

cat << EOF
# This is a shell archive.  Save it in a file, remove anything before
# this line, and then unpack it by entering "sh file".  Note, it may
# create directories; files and directories will be owned by you and
# have default permissions.
#
# This archive contains:
#
EOF

for i
do
	echo "#	$i"
done

echo "#"

for i
do
	if [ -d $i ]; then
		echo "echo c - $i"
		echo "mkdir -p $i > /dev/null 2>&1"
	else
		echo "echo x - $i"
		echo "sed 's/^X//' >$i << 'END-of-$i'"
		sed 's/^/X/' $i
		echo "END-of-$i"
	fi
done
echo exit
echo ""

exit 0
