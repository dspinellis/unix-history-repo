divert(10)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)nsmacros.m4	1.10 (Berkeley) 2/15/89
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

