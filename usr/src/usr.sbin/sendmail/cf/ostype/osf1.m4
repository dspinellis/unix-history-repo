divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)osf1.m4	2.1 (Berkeley) %G%')
ifdef(`_OLD_SENDMAIL_', `define(`NEED_DOMAIN', `')')dnl
define(`ALIAS_FILE', /usr/adm/sendmail/aliases)dnl
define(`STATUS_FILE', /usr/adm/sendmail/sendmail.st)dnl
define(`HELP_FILE', /usr/share/lib/sendmail.hf)dnl
