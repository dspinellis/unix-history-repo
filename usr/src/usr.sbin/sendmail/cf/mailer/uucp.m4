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

VERSIONID(`@(#)uucp.m4	6.2 (Berkeley) %G%')

Msuucp,	P=ifdef(`UUCP_MAILER', `UUCP_MAILER', /usr/bin/uux), F=mDFMhuU, S=12, R=12, M=100000
	A=uux - -r -z -a$f -gC $h!rmail ($u)

Muucp,	P=ifdef(`UUCP_MAILER', `UUCP_MAILER', /usr/bin/uux), F=DFMhuU, S=12, R=12, M=100000
	A=uux - -r -z -a$f -gC $h!rmail ($u)

S12

# don't qualify list:; syntax
R$* :;				$@ $1 :;

R$* < @ $j >			$1			strip local name
R$* < @ $- . UUCP >		$2 ! $1			convert to UUCP format
R$* < @ $+ >			$2 ! $1			convert to UUCP format
R$+				$: $k ! $1		prepend our name

PUSHDIVERT(4)
# resolve locally connected UUCP links
R< @ $=Y . UUCP > : $+		$#suucp $@ $1 $: $1:$2	@host.UUCP: ...
R< @ $=U . UUCP > : $+		$#uucp $@ $1 $: $1:$2	@host.UUCP: ...
R$+ < @ $=Y . UUCP >		$#suucp $@ $2 $: $1	user@host.UUCP
R$+ < @ $=U . UUCP >		$#uucp $@ $2 $: $1	user@host.UUCP
POPDIVERT
