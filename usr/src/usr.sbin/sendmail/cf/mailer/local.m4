PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
ifdef(`LOCAL_MAILER_FLAGS',, `define(`LOCAL_MAILER_FLAGS', `rn')')
ifdef(`LOCAL_MAILER_PATH',, `define(`LOCAL_MAILER_PATH', /bin/mail)')
ifdef(`LOCAL_SHELL_PATH',, `define(`LOCAL_SHELL_PATH', /bin/sh)')
POPDIVERT

##################################################
###   Local and Program Mailer specification   ###
##################################################

VERSIONID(`@(#)local.m4	6.10 (Berkeley) %G%')

Mlocal,		P=LOCAL_MAILER_PATH, F=CONCAT(`lsDFMm', LOCAL_MAILER_FLAGS), S=10, R=20,
		A=mail -d $u
Mprog,		P=LOCAL_SHELL_PATH, F=lsDFMeu, S=10, R=20, D=$z:/,
		A=sh -c $u

S10
R<@>			$n			errors to mailer-daemon
ifdef(`_ALWAYS_ADD_DOMAIN_',
`R$* < @ $* > $*		$@ $1 < @ $2 > $3	already fully qualified
R$*			$: $1 @ $M		add local qualification
R$* @			$: $1 @ $j		if $M not defined',
`dnl')
