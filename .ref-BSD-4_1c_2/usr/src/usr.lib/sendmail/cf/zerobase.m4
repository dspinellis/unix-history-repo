############################################################
############################################################
#####
#####		RULESET ZERO PREAMBLE
#####
#####	The beginning of ruleset zero is constant through all
#####	configurations.
#####
#####		@(#)zerobase.m4	3.3		2/24/83
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

# now delete the local info
R$*<$*$=w.LOCAL>$*	$1<$2>$4			thishost.LOCAL
R$*<$*LOCAL>$*		$1<$2$R>$3			domain gateway
R$*<$*$=w.ARPA>$*	$1<$2>$4			thishost.ARPA
R$*<$*$=w.UUCP>$*	$1<$2>$4			thishost.UUCP
R$*<$*$=w>$*		$1<$2>$4			thishost
R$*<$*.>$*		$1<$2>$3			drop trailing dot
R<@>:$*			$@$>0$1				retry after route strip
R$*<@>			$@$>0$1				strip null trash & retry
