divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)hpux10.m4	8.3 (Berkeley) %G%')

define(`QUEUE_DIR', /var/spool/mqueue)dnl
define(`ALIAS_FILE', /etc/mail/aliases)dnl
define(`STATUS_FILE', /etc/mail/sendmail.st)dnl
define(`LOCAL_MAILER_PATH', /usr/bin/rmail)dnl
define(`LOCAL_MAILER_FLAGS', `m')dnl
define(`LOCAL_MAILER_ARGS', `rmail -d $u')dnl
define(`LOCAL_SHELL_PATH', /usr/bin/sh)dnl
define(`UUCP_MAILER_ARGS', `uux - -r -a$g -gC $h!rmail ($u)')dnl
define(`confTIME_ZONE', `USE_TZ')dnl
