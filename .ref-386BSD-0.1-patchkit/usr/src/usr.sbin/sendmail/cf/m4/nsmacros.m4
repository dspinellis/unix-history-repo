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
#	@(#)nsmacros.m4	1.9 (Berkeley) 1/3/89
#
divert(0)
######################
#   General Macros   #
######################

# local domain name
ifdef(`DOMAIN',
DOMAIN,
`#' YOUR DOMAIN NAME GOES HERE!
DDYOUR_DOMAIN_NAME)

ifdef(`INTERNET_RELAY',
`#' Internet relay host -- machines in our domain that are not
`#' registered with the NIC will be "hidden" behind this relay machine
`#' with the % kludge`,' although SMTP delivery will still be performed
`#' by the sending machine.  Someday this will go away.
INTERNET_RELAY)

ifdef(`UUCP_RELAY',
`#' UUCP relay host
UUCP_RELAY)

ifdef(`CSNET_RELAY',
`#' csnet relay host
CSNET_RELAY)

ifdef(`BITNET_RELAY',
`#' bitnet relay host
BITNET_RELAY)

# my official hostname
Dj$w

