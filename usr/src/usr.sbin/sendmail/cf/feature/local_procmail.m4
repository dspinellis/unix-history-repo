divert(-1)
#
# Copyright (c) 1994 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)local_procmail.m4	8.2 (Berkeley) %G%')
divert(-1)

define(`PROCMAIL_PATH',
	ifelse(_ARG_, `', `/usr/local/bin/procmail', `_ARG_'))
define(`LOCAL_MAILER_FLAGS', `SPfhn')
define(`LOCAL_MAILER_PATH', PROCMAIL_PATH)
define(`LOCAL_MAILER_ARGS', `procmail -a $h -d $u')
