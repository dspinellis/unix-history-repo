divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

#
#  This is a generic configuration file for HP-UX 9.x.
#  It has support for local and SMTP mail only.  If you want to
#  customize it, copy it to a name appropriate for your environment
#  and do the modifications there.
#

include(`../m4/cf.m4')
VERSIONID(`@(#)generic-hpux9.mc	8.1 (Berkeley) %G%')
OSTYPE(hpux9)dnl
DOMAIN(generic)dnl
MAILER(local)dnl
MAILER(smtp)dnl
