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
#	@(#)compat.m4	5.3 (Berkeley) 8/2/85
#
divert(0)
############################################################
############################################################
#####
#####		Provide Backward Compatibility
#####
############################################################
############################################################

define(m4COMPAT, 5.3)

include(berkhosts.m4)

#####################################################
#  General code to convert back to old style names  #
#####################################################
S5

R$+<@$-.LOCAL>		$2:$1				u@h.LOCAL => h:u
R$+<@$=Z>		$@$2:$1				u@bhost => h:u
R$+<@$=C>		$@$2:$1				u@cchost => h:u
R$+<@$-.Bitnet>		$C!$2:$1			u@h.Bitnet => cfo-relay!h:u
R$+<@$-.CC>		$C!$2:$1			u@h.CC => cfo-relay!h:u
R$+<@$-.UUCP>		$2!$1				u@host.UUCP => host!u
R$+@$+.ARPA		$1@$2				u@host.ARPA => u@host
