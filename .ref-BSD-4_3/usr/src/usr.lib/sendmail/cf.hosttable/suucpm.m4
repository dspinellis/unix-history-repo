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
#	@(#)suucpm.m4	5.3 (Berkeley) 11/1/85
#
divert(0)
############################################################
############################################################
#####
#####		Smart UUCP Mailer specification
#####
#####	The other end must speak domain-based  addresses for
#####	this to work.  Someday this should become the "suucp"
#####	mailer, and we should be able to select by host name.
#####
############################################################
############################################################

Muucp,	P=/usr/bin/uux, F=sDFMhu, S=15, M=100000,
	A=uux - -r $h!rmail ($u)

S15
R$*<@$+>$*		$@$1<@$2>$3			accept usual domain name
R$+			$:$1<@$U.UUCP>			stick on our host name
