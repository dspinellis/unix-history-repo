divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

#
#  This the prototype for a "null client" -- that is, a client that
#  does nothing except forward all mail to a mail hub.
#
#  To use this, you MUST use the nullclient feature with the name of
#  the mail hub as its argument.  You MAY also define an OSTYPE to
#  define the location of the queue directories and the like.
#  Other than these, it should never contain any other lines.
#

include(`../m4/cf.m4')
VERSIONID(`@(#)clientproto.mc	8.2 (Berkeley) %G%')

FEATURE(nullclient, mailhost.$m)
