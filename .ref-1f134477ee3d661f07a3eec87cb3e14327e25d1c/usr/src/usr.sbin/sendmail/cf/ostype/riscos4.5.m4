divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)riscos4.5.m4	8.2 (Berkeley) %G%')

define(`LOCAL_MAILER_ARGS', `rmail -d $u')dnl
define(`ALIAS_FILE', `/usr/lib/aliases')dnl
define(`QUEUE_DIR', `/usr/spool/mqueue')dnl
define(`HELP_FILE', `/usr/lib/sendmail.hf')dnl
