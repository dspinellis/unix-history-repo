#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# the Systems Programming Group of the University of Utah Computer
# Science Department.
#
# %sccs.include.redist.sh%
#
#	@(#)mcount.sed	5.1 (Berkeley) %G%
#

s/^\(_[a-z_].*\):$/&\
	.data\
X\1:\
	.long	0\
	.text\
	movel	#X\1,a0\
	jsr	mcount/
