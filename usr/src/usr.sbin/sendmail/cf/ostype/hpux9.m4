divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)hpux9.m4	8.5 (Berkeley) %G%')

define(`QUEUE_DIR', /usr/spool/mqueue)dnl
define(`ALIAS_FILE', /usr/lib/aliases)dnl
define(`STATUS_FILE', /usr/lib/sendmail.st)dnl
define(`LOCAL_MAILER_FLAGS', `m')dnl
define(`UUCP_MAILER_ARGS', `uux - -r -a$f -gC $h!rmail ($u)')dnl
define(`confTIME_ZONE', `USE_TZ')dnl
