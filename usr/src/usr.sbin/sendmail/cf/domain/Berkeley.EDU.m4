divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
divert(0)
VERSIONID(`@(#)Berkeley.EDU.m4	2.5 (Berkeley) %G%')
define(`UUCP_RELAY', `ucbvax.Berkeley.EDU')dnl
define(`BITNET_RELAY', `jade.Berkeley.EDU')dnl
define(`CSNET_RELAY', `Relay.Prime.COM')dnl
ifdef(`_OLD_SENDMAIL_', `', `FEATURE(no_wildcard_MX)')dnl
