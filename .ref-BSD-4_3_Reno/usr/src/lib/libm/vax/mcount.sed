#
# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
# All recipients should regard themselves as participants in an ongoing
# research project and hence should feel obligated to report their
# experiences (good or bad) with these elementary function codes, using
# the sendbug(8) program, to the authors.
#
#	@(#)mcount.sed	5.2 (Berkeley) 2/14/89
#
s/.word	0x0.*$/&\
	.data\
1:\
	.long	0\
	.text\
	moval	1b,r0\
	jsb	mcount/
