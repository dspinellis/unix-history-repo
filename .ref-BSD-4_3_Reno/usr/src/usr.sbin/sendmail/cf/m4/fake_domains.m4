divert(10)dnl
#
# Handle "fake" top level domains, by sending to a smarter
# host.  Currently, we support:
#
#	user@host.bitnet	->	$B		user@host.bitnet
#	user@host.csnet		->	$C		user@host.csnet
#
# Eventually, all these should Go Away.
#
# @(#)fake_domains.m4	1.5 (Berkeley) 1/3/89
#
divert(0)dnl
ifdef(`BITNET_RELAY',
R$*<@$+.BITNET>$*	$`#'tcp$@$B$:$1<@$2.BITNET>$3		user@host.BITNET)
ifdef(`CSNET_RELAY',
R$*<@$+.CSNET>$*	$`#'tcp$@$C$:$1<@$2.CSNET>$3		user@host.CSNET)
