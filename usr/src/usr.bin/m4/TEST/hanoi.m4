#
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Ozan Yigit.
#
# %sccs.include.redist.sh%
#
#	@(#)hanoi.m4	5.2 (Berkeley) %G%
#

define(hanoi, `trans(A, B, C, $1)')

define(moved,`move disk from $1 to $2
')

define(trans, `ifelse($4,1,`moved($1,$2)',
	`trans($1,$3,$2,DECR($4))moved($1,$2)trans($3,$2,$1,DECR($4))')')
