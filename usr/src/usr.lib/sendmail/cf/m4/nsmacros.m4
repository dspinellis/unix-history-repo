divert(10)
#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)nsmacros.m4	1.7 (Berkeley) 4/8/88
#
divert(0)
######################
#   General Macros   #
######################

# our arpanet gateway
ifdef(`INTERNET_RELAY',
INTERNET_RELAY,
DAucbvax.Berkeley.EDU)

# local domain names
DDBerkeley.EDU

# major relay host
ifdef(`UUCP_RELAY',
UUCP_RELAY,
DRucbvax.Berkeley.EDU)

# my official hostname
Dj$w

