divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)mailertable.m4	8.1 (Berkeley) %G%')
divert(-1)

define(`MAILER_TABLE', ifelse(_ARG_, `', `hash /etc/mailertable -o', `_ARG_'))dnl
