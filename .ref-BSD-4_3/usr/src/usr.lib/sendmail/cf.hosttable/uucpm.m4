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
#	@(#)uucpm.m4	5.4 (Berkeley) 8/2/85
#
divert(0)
############################################################
############################################################
#####
#####		UUCP Mailer specification
#####
############################################################
############################################################

ifdef(`m4COMPAT',, `include(compat.m4)')

Muucp,	P=/usr/bin/uux, F=DFMhuU, S=13, R=23, M=100000,
	A=uux - -r -z -a$f -gC $h!rmail ($u)

S13
R$+			$:$>5$1				convert to old style
R$=w!$+			$2				strip local name
R$*<@$=S>$*		$1<@$2.$A>$3			resolve abbreviations
R$*<@$=Z>$*		$1<@$2.$A>$3			resolve abbreviations
R$*<@$->$*		$1<@$2.ARPA>$3			resolve abbreviations
R$+<@$+>		$2!$1				uucpize (no @'s in addr)
R$+			$:$U!$1				stick on our host name
R$=w!$=R:$+		$:$1!$3				ucbvax!ucbvax:xxx

S23
R$+			$:$>5$1				convert to old style
R$*<@$=S>$*		$1<@$2.$A>$3			resolve abbreviations
R$*<@$=Z>$*		$1<@$2.$A>$3			resolve abbreviations
R$*<@$=w>$*		$1<@$2.$A>$3			resolve abbreviations
R$*<@$->$*		$1<@$2.ARPA>$3			resolve abbreviations
R$+<@$*$=w.EDU>		$2$3.EDU!$1			uucp!u@local -> local!uucp!u
