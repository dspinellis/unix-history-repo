divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)nextstep.m4	8.4 (Berkeley) %G%')
define(`ALIAS_FILE', /etc/sendmail/aliases)dnl
define(`HELP_FILE', /usr/lib/sendmail.hf)dnl
define(`STATUS_FILE', /etc/sendmail/sendmail.st)dnl
define(`UUCP_MAILER_PATH', /usr/bin/uux)dnl
define(`QUEUE_DIR', /usr/spool/mqueue)dnl
define(`LOCAL_MAILER_FLAGS', `rmnP')dnl
define(`LOCAL_SHELL_FLAGS', `euP')dnl
