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
#	@(#)mmdfm.m4	5.2 (Berkeley) 8/2/85
#
divert(0)
############################################################
############################################################
#####
#####		MMDF Phonenet Channel Mailer specification
#####
############################################################
############################################################

Mmmdf,	P=/usr/lib/mmdf/sendmmdf, F=sDFMu, S=16, R=16, A=sendmmdf $f $h $u

S16
R$+<@$-.LOCAL>		$@$1<@$2.$D.ARPA>		externalize local names
R$+<@$+>		$@$1<@$2>			already ok
R$+			$@$1<@Berk-Test>		tack on our hostname
