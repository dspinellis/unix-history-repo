#
# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)mcount.sed	5.3 (Berkeley) %G%
#

s/.word	0x0.*$/&\
	.data\
1:\
	.long	0\
	.text\
	moval	1b,r0\
	jsb	mcount/
