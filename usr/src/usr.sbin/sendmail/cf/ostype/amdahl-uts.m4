divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)amdahl-uts.m4	8.1 (Berkeley) %G%')
divert(-1)

define(`ALIAS_FILE', /etc/mail/aliases)
define(`HELP_FILE', /etc/mail/sendmail.hf)
define(`STATUS_FILE', /usr/lib/sendmail.st)
define(`LOCAL_MAILER_FLAGS', `fSn')
define(`confCW_FILE', /etc/mail/sendmail.cw)
