PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
POPDIVERT
#####################################
###   UUCP Mailer specification   ###
#####################################

VERSIONID(@(#)uucp.m4	2.1 (Berkeley) %G%)

Msuucp,	P=ifdef(`UUCP_MAILER', `UUCP_MAILER', /usr/bin/uux), F=mDFMhuU, S=12, R=12, M=100000
	A=uux - -r -z -a$f -gC $h!rmail ($u)

Muucp,	P=ifdef(`UUCP_MAILER', `UUCP_MAILER', /usr/bin/uux), F=DFMhuU, S=12, R=12, M=100000
	A=uux - -r -z -a$f -gC $h!rmail ($u)


S12

`R$* < @ $w >			$1			strip local name'
`R$* < @ $- . UUCP >		$2 ! $1			convert to UUCP format'
`R$* < @ $+ >			$2 ! $1			convert to UUCP format'
`R$+				$: $U ! $1		prepend our name'
