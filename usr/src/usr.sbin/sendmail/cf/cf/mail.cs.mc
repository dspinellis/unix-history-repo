divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

#
#  This is a Berkeley-specific configuration file for a specific
#  machine in the Computer Science Division at Berkeley, and should
#  not be used elsewhere.   It is provided on the sendmail distribution
#  as a sample only.
#
#  This file is for the primary CS Division mail server.
#

include(`../m4/cf.m4')
VERSIONID(`@(#)mail.cs.mc	8.6 (Berkeley) %G%')
OSTYPE(ultrix4)dnl
DOMAIN(Berkeley)dnl
MASQUERADE_AS(CS.Berkeley.EDU)dnl
FEATURE(use_cw_file)dnl
MAILER(local)dnl
MAILER(smtp)dnl
define(`confUSERDB_SPEC', ``/usr/local/lib/users.cs.db,/usr/local/lib/users.eecs.db'')dnl
