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
#	@(#)suucpm.m4	1.3 (Berkeley) 1/3/89
#
divert(0)
############################################################
############################################################
#####
#####		UUCP Smart Mailer specification
#####		(handles multiple recipients)
#####
############################################################
############################################################



Msuucp,	P=/usr/bin/uux, F=mDFMhuU, S=13, R=23, M=100000,
	A=uux - -r $h!rmail ($u)

ifdef(`m4UUCP',`divert(10)',)

S13
R$+			$:$>5$1				convert to old style
R$=w!$+			$2				strip local name
R$*<@$->$*		$1<@$2.$D>$3			resolve abbreviations
R$+<@$+>		$2!$1				uucpize (no @'s in addr)
R$+			$:$U!$1				stick on our host name

S23
R$+			$:$>5$1				convert to old style
R$*<@$=w>$*		$1<@$2.$D>$3			resolve abbreviations
R$*<@$->$*		$1<@$2.$D>$3			resolve abbreviations
R$+<@$w>		$w!$1				uucp!u@local -> local!uucp!u

ifdef(`m4UUCP',`divert(0)',)
