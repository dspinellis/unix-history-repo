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
#	@(#)rule0.m4	1.6 (Berkeley) 2/26/86
#
divert(0)
############################################################
############################################################
#####
#####		RULESET ZERO PREAMBLE
#####
#####	The beginning of ruleset zero is constant through all
#####	configurations.
#####
############################################################
############################################################

S0

# first make canonical
R$*<$*>$*		$1$2$3				defocus
R$+			$:$>3$1				make canonical

# handle special cases.....
R$*<@[$+]>$*		$:$1<@$[[$2]$]>$3		lookup numeric internet addr
R$*<@[$+]>$*		$#tcp$@[$2]$:$1@[$2]$3		numeric internet spec
R$+			$:$>6$1
R$-<@LOCAL>		$#local$:$1
R@			$#local$:$n			handle <> form

# canonicalize using the nameserver if not internal domain
R$*<@$*.$~I>$*		$:$1<@$[$2.$3$]>$4
R$*<@$->$*		$:$1<@$[$2$]>$3

# arrange for local names to be fully qualified
R$*<$+.ARPA.LOCAL>$*	$1<$2.ARPA>$3			because ARPA is a host

# now delete the local info
R$*<$*$=w.$=T>$*	$1<$2>$5			thishost.LOCAL
R$*<$*$=w>$*		$1<$2>$4			thishost
R$*<$*.>$*		$1<$2>$3			drop trailing dot
R<@>:$*			$@$>0$1				retry after route strip
R$*<@>			$@$>0$1				strip null trash & retry

# forward around hosts with communication problems
R$*<@$=F.LOCAL>$*	$#tcp$@$F$:$1<@$2.LOCAL>$3	reroute message

##################################
#  End of ruleset zero preamble  #
##################################
