divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
divert(0)
VERSIONID(@(#)CS.Berkeley.EDU.m4	2.4 (Berkeley) %G%)
define(`UUCP_RELAY', `ucbvax.Berkeley.EDU')dnl
define(`BITNET_RELAY', `jade.Berkeley.EDU')dnl
define(`CSNET_RELAY', `Relay.Prime.COM')dnl
FEATURE(no_wildcard_MX)dnl
LOCAL_RULE_0
# hacks to allow local hostnames as host.Berkeley.EDU or host.CS.Berkeley.EDU
R$* < @ $=w .CS.Berkeley.EDU > $*	$: $1 < @ $w > $3
R$* < @ $=w .Berkeley.EDU > $*		$: $1 < @ $w > $3
divert(0)
