#
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Ozan Yigit.
#
# %sccs.include.redist.sh%
#
#	@(#)ack.m4	5.2 (Berkeley) %G%
#

define(ack, `ifelse($1,0,incr($2),$2,0,`ack(DECR($1),1)',
`ack(DECR($1), ack($1,DECR($2)))')')
