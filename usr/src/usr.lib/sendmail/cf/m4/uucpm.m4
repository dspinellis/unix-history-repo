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
#	@(#)uucpm.m4	1.10 (Berkeley) 11/11/87
#
divert(0)
############################################################
############################################################
#####
#####		UUCP Mailer specification
#####
############################################################
############################################################


Muucp,	P=/usr/bin/uux, F=DFMhuU, S=13, R=23, M=100000,
	A=uux - -r -z -a$f -gC $h!rmail ($u)

S13
R$+			$:$>5$1				convert to old style
R$*<@$=w>$*		$1<@$w>$2			resolve abbreviations
R$*<@$->$*		$1<@$2.$D>$3			resolve abbreviations
R$+<@$+>		$2!$1				uucpize (no @'s in addr)
R$w!$+			$1				strip local name
R$+			$:$U!$1				stick on our host name
R$=w!$=R:$+		$:$1!$3				us!us:user -> us!user
R$=U!$-%$-		$:$1!$2@$3.$D			ucbvax!user@host.domain

S23
R$+			$:$>5$1				convert to old style
R$*<@$=w>$*		$1<@$w>$2			resolve abbreviations
R$*<@$->$*		$1<@$2.$D>$3			resolve abbreviations
R$+<@$w>		$U!$1				a!b@here -> here!a!b
R$=U!$+			$2				here!a!b -> a!b
# sanity ... should not happen.
R$=U.$D!$+		$2				strip local name.domain
