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
#	@(#)zerobase.m4	5.3 (Berkeley) 8/2/85
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
R@			$#local$:$n			handle <> form
R$*<@[$+]>$*		$#tcp$@[$2]$:$1@[$2]$3		numeric internet spec

# arrange for local names to be fully qualified
R$*<$*$=S>$*		$1<$2$3.LOCAL>$4		user@etherhost
R$*<$*$=Z>$*		$1<$2$3.LOCAL>$4		user@berkhost
R$*<$+.ARPA.LOCAL>$*	$1<$2.ARPA>$3			because ARPA is a host

# now delete the local info
R$*<$*$=w.$=T>$*	$1<$2>$5			thishost.LOCAL
R$*<$*$=w>$*		$1<$2>$4			thishost
R$*<$*.>$*		$1<$2>$3			drop trailing dot
R<@>:$*			$@$>0$1				retry after route strip
R$*<@>			$@$>0$1				strip null trash & retry

# forward around hosts with communication problems
R$*<@$=F.LOCAL>$*	$#ether$@$F$:$1<@$2.LOCAL>$3	reroute message

##################################
#  End of ruleset zero preamble  #
##################################
