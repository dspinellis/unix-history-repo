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
#  You MUST change the `OSTYPE' macro to specify the operating system
#  on which this will run; this will set the location of various
#  support files for your operating system environment.  You MAY
#  create a domain file in ../domain and reference it by adding a
#  `DOMAIN' macro after the `OSTYPE' macro.  I recommend that you
#  first copy this to another file name so that new sendmail releases
#  will not trash your changes.
#

include(`../m4/cf.m4')
VERSIONID(`@(#)tcpproto.mc	8.3 (Berkeley) %G%')
OSTYPE(unknown)
FEATURE(nouucp)
MAILER(local)
MAILER(smtp)
