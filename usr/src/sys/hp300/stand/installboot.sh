#!/bin/sh -
#
# Copyright (c) 1994
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)installboot.sh	8.2 (Berkeley) %G%
#
# compatibility with old installboot program
#
if [ $# != 2 ]
then
	echo "Usage: installboot bootprog device"
	exit 1
fi
if [ ! -f $1 ]
then
	echo "Usage: installboot bootprog device"
	echo "${1}: bootprog must be a regular file"
	exit 1
fi
if [ ! -c $2 ]
then
	echo "Usage: installboot bootprog device"
	echo "${2}: device must be a char special file"
	exit 1
fi
/sbin/disklabel -B -b $1 $2
exit $?
