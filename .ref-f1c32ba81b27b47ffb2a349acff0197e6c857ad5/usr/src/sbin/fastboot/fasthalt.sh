#!/bin/sh -
#
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)fasthalt.sh	5.4 (Berkeley) %G%
#

cp /dev/null /fastboot
/sbin/halt $*
