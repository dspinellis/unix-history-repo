#!/bin/sh -
#
# Copyright (c) 1985 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)fastboot.sh	5.6 (Berkeley) %G%
#

cp /dev/null /fastboot
/sbin/reboot $*
