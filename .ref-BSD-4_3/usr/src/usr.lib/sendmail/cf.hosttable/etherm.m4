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
#	@(#)etherm.m4	5.2 (Berkeley) 8/2/85
#
divert(0)
############################################################
############################################################
#####
#####		Ethernet Mailer specification
#####
#####	Messages processed by this configuration are assumed to remain
#####	in the same domain.  Hence, they may not necessarily correspond
#####	to RFC822 in all details.
#####
############################################################
############################################################

Mether,	P=[IPC], F=mDFMueCX, S=11, R=21, A=IPC $h

S11
R$*<@$+>$*		$@$1<@$2>$3			already ok
R$+			$@$1<@$w>			tack on our hostname

S21
