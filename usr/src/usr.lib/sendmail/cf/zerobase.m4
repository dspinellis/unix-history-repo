############################################################
############################################################
#####
#####		RULESET ZERO PREAMBLE
#####
#####	The beginning of ruleset zero is constant through all
#####	configurations.
#####
#####		@(#)zerobase.m4	4.1		7/25/83
#####
############################################################
############################################################

S0

# first make canonical
R$*<$*>$*		$1$2$3				defocus
R$+			$:$>3$1				make canonical

# handle special cases.....
R@			$#local$:MAILER-DAEMON		handle <> form
R$*<@[$+]>$*		$#tcp$@[$2]$:$1@[$2]$3		numeric internet spec

# arrange for local names to be fully qualified
R$*<$*$=S>$*		$1<$2$3.LOCAL>$4		user@etherhost
R$*<$*$=Z>$*		$1<$2$3.LOCAL>$4		user@berkhost
R$*<$+.ARPA.LOCAL>$*	$1<$2.ARPA>$3			because ARPA is a host

# now delete the local info
R$*<$*$=w.LOCAL>$*	$1<$2>$4			thishost.LOCAL
R$*<$*$=w.ARPA>$*	$1<$2>$4			thishost.ARPA
R$*<$*$=w.UUCP>$*	$1<$2>$4			thishost.UUCP
R$*<$*$=w>$*		$1<$2>$4			thishost
R$*<$*.>$*		$1<$2>$3			drop trailing dot
R<@>:$*			$@$>0$1				retry after route strip
R$*<@>			$@$>0$1				strip null trash & retry

# forward around hosts with communication problems
R$*<@$=F.LOCAL>$*	$#ether$@$F$:$1<@$2.LOCAL>$3	reroute message

##################################
#  End of ruleset zero preamble  #
##################################
