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
#	@(#)prewriterule.m4	1.1 (Berkeley) 8/8/85
#
divert(0)
################################
#  Sender Field Pre-rewriting  #
################################
S1
#R$*<$*>$*		$1$2$3				defocus

###################################
#  Recipient Field Pre-rewriting  #
###################################
S2
#R$*<$*>$*		$1$2$3				defocus

