divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

#
#  This machine has been decommissioned at Berkeley, and hence should
#  not be considered to be tested.  This file is provided as an example
#  only, of how you might set up a joint SMTP/UUCP configuration.  At
#  this point I recommend using `FEATURE(mailertable)' instead of
#  `SITECONFIG'.  See also ucbvax.mc.
#

include(`../m4/cf.m4')
VERSIONID(`@(#)ucbarpa.mc	8.2 (Berkeley) %G%')
DOMAIN(CS.Berkeley.EDU)dnl
OSTYPE(bsd4.4)dnl
MAILER(local)dnl
MAILER(smtp)dnl
MAILER(uucp)dnl
SITECONFIG(uucp.ucbarpa, ucbarpa, U)
