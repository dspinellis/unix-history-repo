PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
ifdef(`LOCAL_MAILER_FLAGS',, `define(`LOCAL_MAILER_FLAGS', `rmn')')
ifdef(`LOCAL_MAILER_PATH',, `define(`LOCAL_MAILER_PATH', /bin/mail)')
ifdef(`LOCAL_MAILER_ARGS',, `define(`LOCAL_MAILER_ARGS', `mail -d $u')')
ifdef(`LOCAL_SHELL_FLAGS',, `define(`LOCAL_SHELL_FLAGS', `eu')')
ifdef(`LOCAL_SHELL_PATH',, `define(`LOCAL_SHELL_PATH', /bin/sh)')
ifdef(`LOCAL_SHELL_ARGS',, `define(`LOCAL_SHELL_ARGS', `sh -c $u')')
ifdef(`LOCAL_SHELL_DIR',, `define(`LOCAL_SHELL_DIR', `$z:/')')
POPDIVERT

##################################################
###   Local and Program Mailer specification   ###
##################################################

VERSIONID(`@(#)local.m4	8.12 (Berkeley) %G%')

Mlocal,		P=LOCAL_MAILER_PATH, F=CONCAT(`lsDFMAw5:/|@', LOCAL_MAILER_FLAGS), S=10, R=20/40, T=X-Unix,
		A=LOCAL_MAILER_ARGS
Mprog,		P=LOCAL_SHELL_PATH, F=CONCAT(`lsDFMo', LOCAL_SHELL_FLAGS), S=10, R=20/40, D=LOCAL_SHELL_DIR, T=X-Unix,
		A=LOCAL_SHELL_ARGS

S10
R<@>			$n			errors to mailer-daemon
R$+			$: $>40 $1

S20
R$+ < @ $* >		$: $1			strip host part

S40
ifdef(`_ALWAYS_ADD_DOMAIN_',
`R$* < @ $* > $*		$@ $1 < @ $2 > $3	already fully qualified
R$*			$: $1 @ $M		add local qualification
R$* @			$: $1 @ $j		if $M not defined',
`dnl')
