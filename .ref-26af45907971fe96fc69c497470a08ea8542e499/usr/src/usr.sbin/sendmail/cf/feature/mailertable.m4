divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)mailertable.m4	6.2 (Berkeley) %G%')
divert(-1)

define(`MAILER_TABLE', ifelse(_ARG_, `', `hash /etc/mailertable -o', `_ARG_'))dnl
