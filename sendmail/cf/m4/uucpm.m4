divert(10)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)uucpm.m4	1.12 (Berkeley) 2/15/89
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
R$=U!$-%$-		$:$1!$2@$3.$D			ucbvax!user@host.domain

S23
R$+			$:$>5$1				convert to old style
R$*<@$=w>$*		$1<@$w>$2			resolve abbreviations
R$*<@$->$*		$1<@$2.$D>$3			resolve abbreviations
R$+<@$w>		$U!$1				a!b@here -> here!a!b
R$=U!$+			$2				here!a!b -> a!b
# sanity ... should not happen.
R$=U.$D!$+		$2				strip local name.domain
