PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#

ifdef(`UUCP_MAILER_PATH',, `define(`UUCP_MAILER_PATH', /usr/bin/uux)')
ifdef(`UUCP_MAILER_FLAGS',, `define(`UUCP_MAILER_FLAGS', `')')
POPDIVERT
#####################################
###   UUCP Mailer specification   ###
#####################################

VERSIONID(`@(#)uucp.m4	6.12 (Berkeley) %G%')

Msuucp,		P=UUCP_MAILER_PATH, F=CONCAT(mDFMhuU, UUCP_MAILER_FLAGS), S=12, R=22, M=100000,
		A=uux - -r -z -a$f -gC $h!rmail ($u)

Muucp,		P=UUCP_MAILER_PATH, F=CONCAT(DFMhuU, UUCP_MAILER_FLAGS), S=12, R=22, M=100000,
		A=uux - -r -z -a$f -gC $h!rmail ($u)

# sender rewriting
S12

# handle error address as a special case
R<@>				$n			errors to mailer-daemon

# don't qualify list:; syntax
R$* :; <@>			$@ $1 :;

R$* < @ $* . >			$1 < @ $2 >		strip trailing dots
R$* < @ $j >			$1			strip local name
R$* < @ $- . UUCP >		$2 ! $1			convert to UUCP format
R$* < @ $+ >			$2 ! $1			convert to UUCP format
R$+				$: $k ! $1		prepend our name

# recipient rewriting
S22

# don't touch list:; syntax
R$* :; <@>			$@ $1 ;:

R$* < @ $* . >			$1 < @ $2 >		strip trailing dots
R$* < @ $j >			$1			strip local name
R$* < @ $- . UUCP >		$2 ! $1			convert to UUCP format
R$* < @ $+ >			$2 ! $1			convert to UUCP format

PUSHDIVERT(4)
# resolve locally connected UUCP links
R< @ $=Y . UUCP > : $+		$#suucp $@ $1 $: $2	@host.UUCP: ...
R< @ $=U . UUCP > : $+		$#uucp $@ $1 $: $2	@host.UUCP: ...
R$+ < @ $=Y . UUCP >		$#suucp $@ $2 $: $1	user@host.UUCP
R$+ < @ $=U . UUCP >		$#uucp $@ $2 $: $1	user@host.UUCP
POPDIVERT
