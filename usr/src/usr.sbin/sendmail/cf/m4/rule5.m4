divert(10)
#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)rule5.m4	1.4 (Berkeley) 11/4/87
#
divert(0)
############################################################
############################################################
#####
#####		Provide Backward Compatibility
#####
############################################################
############################################################

#####################################################
#  General code to convert back to old style names  #
#####################################################
S5

R$+<@$w>		$1				strip host
R$+<@$-.UUCP>		$2!$1				u@host.UUCP => host!u
