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
#  machine in Electrical Engineering and Computer Sciences at Berkeley,
#  and should not be used elsewhere.   It is provided on the sendmail
#  distribution as a sample only.
#
#  This file is for the primary EECS mail server.
#

include(`../m4/cf.m4')
VERSIONID(`@(#)mail.eecs.mc	8.6 (Berkeley) %G%')
OSTYPE(ultrix4.1)dnl
DOMAIN(eecs.hidden)dnl
FEATURE(use_cw_file)dnl
MAILER(local)dnl
MAILER(smtp)dnl
define(`confUSERDB_SPEC', `/usr/local/lib/users.eecs.db,/usr/local/lib/users.cs.db,/usr/local/lib/users.coe.db')dnl
