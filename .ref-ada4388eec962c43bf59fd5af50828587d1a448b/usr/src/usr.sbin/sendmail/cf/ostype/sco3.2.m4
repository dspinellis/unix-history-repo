divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)sco3.2.m4	8.1 (Berkeley) %G%')
define(`ALIAS_FILE', /usr/lib/mail/aliases)dnl
define(`QUEUE_DIR', /usr/spool/mqueue)dnl
define(`STATUS_FILE', /usr/lib/sendmail.st)dnl
define(`UUCP_MAILER_PATH', /usr/bin/uux)dnl
define(`LOCAL_MAILER_PATH', /usr/bin/lmail)dnl
define(`LOCAL_MAILER_FLAGS', PuhCE)dnl
define(`LOCAL_MAILER_ARGS', `lmail $u')dnl
define(`LOCAL_SHELL_FLAGS', Peu)dnl
