#!/bin/sh -
#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that: (1) source distributions retain this entire copyright
# notice and comment, and (2) distributions including binaries display
# the following acknowledgement:  ``This product includes software
# developed by the University of California, Berkeley and its contributors''
# in the documentation or other materials provided with the distribution
# and in all advertising materials mentioning features or use of this
# software. Neither the name of the University nor the names of its
# contributors may be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)shar.sh	5.2 (Berkeley) 5/23/90
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
		echo "mkdir $i > /dev/null 2>&1"
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
