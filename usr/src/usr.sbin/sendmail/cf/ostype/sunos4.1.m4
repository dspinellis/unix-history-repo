divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
VERSIONID(@(#)sunos4.1.m4	2.2 (Berkeley) %G%)
#

define(`ALIAS_FILE', /etc/aliases)dnl
define(`HELP_FILE', /usr/lib/sendmail.hf)dnl
define(`QUEUE_DIR', /var/spool/mqueue)dnl
define(`STATUS_FILE', /etc/sendmail.st)dnl
define(`LOCAL_MAILER', /bin/mail)dnl

divert(0)
