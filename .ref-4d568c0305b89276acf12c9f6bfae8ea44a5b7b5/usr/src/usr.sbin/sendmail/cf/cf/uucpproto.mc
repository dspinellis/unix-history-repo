divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

#
#  This is the prototype for a configuration that only supports UUCP.
#
#  You may want to add an OSTYPE macro to get the location of various
#  support files for your operating system environment.
#

include(`../m4/cf.m4')
VERSIONID(`@(#)uucpproto.mc	8.3 (Berkeley) %G%')

FEATURE(nodns)dnl

MAILER(local)dnl
MAILER(uucp)dnl
