divert(-1)
#
# Copyright (c) 1994 Eric P. Allman
# Copyright (c) 1994
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

# Support for DYNIX/ptx 2.x.

divert(0)
VERSIONID(`@(#)ptx2.m4	8.1 (Berkeley) %G%')
ifdef(`ALIAS_FILE',,`define(`ALIAS_FILE', /usr/lib/aliases)')dnl
ifdef(`HELP_FILE',,`define(`HELP_FILE', /usr/lib/sendmail.hf)')dnl
ifdef(`STATUS_FILE',,`define(`STATUS_FILE', /usr/lib/sendmail.st)')dnl
define(`LOCAL_MAILER_PATH', `/bin/mail')dnl
define(`LOCAL_MAILER_FLAGS', `rmn')dnl
define(`LOCAL_SHELL_FLAGS', `e')dnl
