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
#	@(#)fudge.m4	5.2 (Berkeley) 8/2/85
#
divert(0)
############################################################
#
#	Fudge to make things look old style
#
############################################################

ifdef(`m4COMPAT',, `include(compat.m4)')

############################################################
###	Convert names to old style for local delivery
############################################################
S10

R$+			$:$>5$1				convert to old style
