divert(10)dnl
#
# Handle "fake" top level domains, by sending to a smarter
# host.  Currently, we support:
#
#	user@host.bitnet	->	jade.berkeley.edu	user@host.bitnet
#	user@host.csnet		->	relay.cs.net	user@host.csnet
#
# Eventually, all these should Go Away.
#
# @(#)fake_domains.m4	1.4 (Berkeley) 4/6/88
#
divert(0)dnl
R$*<@$+.BITNET>$*	$#tcpld$@jade.berkeley.edu$:$1<@$2.BITNET>$3	user@host.BITNET
R$*<@$+.CSNET>$*	$#tcp$@relay.cs.net$:$1<@$2.CSNET>$3	user@host.CSNET
