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

include(`../m4/cf.m4')
VERSIONID(`@(#)knecht.cs.mc	8.2 (Berkeley) %G%')
OSTYPE(hpux9)dnl
DOMAIN(CS.Berkeley.EDU)dnl
define(`LOCAL_RELAY', CS.Berkeley.EDU)dnl
MAILER(smtp)dnl

# our local domain
DDCS.Berkeley.EDU
