#!/bin/sh -
#
# Copyright (c) 1994
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)pagesize.sh	8.1 (Berkeley) %G%
#

PATH=/bin:/usr/bin:/usr/sbin
export PATH

sysctl -n hw.pagesize
