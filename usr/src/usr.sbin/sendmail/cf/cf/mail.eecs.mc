divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

include(`../m4/cf.m4')
VERSIONID(`@(#)mail.eecs.mc	8.5 (Berkeley) %G%')
OSTYPE(ultrix4.1)dnl
DOMAIN(eecs.hidden)dnl
FEATURE(use_cw_file)dnl
MAILER(local)dnl
MAILER(smtp)dnl
define(`confUSERDB_SPEC', `/usr/local/lib/users.eecs.db,/usr/local/lib/users.cs.db,/usr/local/lib/users.coe.db')dnl
