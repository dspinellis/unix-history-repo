divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
divert(0)
VERSIONID(@(#)CS.Berkeley.EDU.m4	2.7 (Berkeley) %G%)
DOMAIN(Berkeley)dnl
#FEATURE(no_wildcard_MX)dnl
LOCAL_RULE_0
# hacks to allow local hostnames as host.Berkeley.EDU or host.CS.Berkeley.EDU
R$* < @ $=w .CS.Berkeley.EDU > $*	$: $1 < @ $j > $3
R$* < @ $=w .Berkeley.EDU > $*		$: $1 < @ $j > $3
divert(0)
