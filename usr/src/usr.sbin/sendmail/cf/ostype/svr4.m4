divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)svr4.m4	8.2 (Berkeley) %G%')

ifdef(`ALIAS_FILE',,`define(`ALIAS_FILE', /usr/ucblib/aliases)')dnl
ifdef(`HELP_FILE',,`define(`HELP_FILE', /usr/ucblib/sendmail.hf)')dnl
ifdef(`STATUS_FILE',,`define(`STATUS_FILE', /usr/ucblib/sendmail.st)')dnl
define(`LOCAL_MAILER_PATH', `/usr/ucblib/binmail')dnl
define(`LOCAL_MAILER_FLAGS', `rmn')dnl
define(`LOCAL_SHELL_FLAGS', `ehuP')dnl
define(`UUCP_MAILER_ARGS', `uux - -r -a$g -gmedium $h!rmail ($u)')dnl
