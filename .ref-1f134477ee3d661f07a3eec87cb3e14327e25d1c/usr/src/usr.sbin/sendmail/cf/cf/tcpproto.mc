divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

#
#  This is the prototype file for a configuration that supports nothing
#  but basic SMTP connections via TCP.
#
#  You may want to add an OSTYPE macro to get the location of various
#  support files for your operating system environment.
#

include(`../m4/cf.m4')
VERSIONID(`@(#)tcpproto.mc	8.2 (Berkeley) %G%')

FEATURE(nouucp)

MAILER(local)
MAILER(smtp)
