divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
#

divert(0)
VERSIONID(`@(#)bsd4.4.m4	8.2 (Berkeley) %G%')
define(`HELP_FILE', /usr/share/misc/sendmail.hf)dnl
define(`STATUS_FILE', /var/log/sendmail.st)dnl
define(`LOCAL_MAILER_PATH', /usr/libexec/mail.local)dnl
define(`UUCP_MAILER_ARGS', `uux - -r -z -a$f $h!rmail ($u)')dnl
