#!/bin/sh -
#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)fastboot.sh	5.2 (Berkeley) 6/6/85
#
cp /dev/null /fastboot
/etc/reboot $*
