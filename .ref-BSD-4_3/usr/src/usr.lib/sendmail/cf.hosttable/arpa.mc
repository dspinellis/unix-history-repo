#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)arpa.mc	5.4 (Berkeley) 10/16/85
#
############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# internet hostname
Cwucbarpa arpa arpavax ucbarpavax r UCB-ARPA

# uucp hostnames
DUucbarpa
CUucbarpa

# local UUCP connections
CWmetron

# we have full sendmail support here
Oa

include(csbase.m4)

#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)zerobase.m4	5.1 (Berkeley) 6/10/85
#
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
R@			$#local$:MAILER-DAEMON		handle <> form
R$*<@[$+]>$*		$#tcp$@[$2]$:$1@[$2]$3		numeric internet spec

# arrange for local names to be fully qualified
R$*<$*$=S>$*		$1<$2$3.LOCAL>$4		user@etherhost
R$*<$*$=Z>$*		$1<$2$3.LOCAL>$4		user@berkhost
R$*<$+.ARPA.LOCAL>$*	$1<$2.ARPA>$3			because ARPA is a host

# now delete the local info
R$*<$*$=w.$=T>$*	$1<$2>$5			thishost.LOCAL
# remove next line as it screws up arpa connections
#R$*<$*$=w>$*		$1<$2>$4			thishost
R$*<$*.>$*		$1<$2>$3			drop trailing dot
R<@>:$*			$@$>0$1				retry after route strip
R$*<@>			$@$>0$1				strip null trash & retry

# forward around hosts with communication problems
R$*<@$=F.LOCAL>$*	$#ether$@$F$:$1<@$2.LOCAL>$3	reroute message

##################################
#  End of ruleset zero preamble  #
##################################
################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve names we can handle locally
R<@$=W.UUCP>:$+		$#uucp$@$1$:$2			@host.UUCP:...
R$+<@$=W.UUCP>		$#uucp$@$2$:$1			user@host.UUCP

# optimize names of known ethernet hosts
R$*<@$*$=S.LOCAL>$*	$#ether$@$3$:$1<@$2$3>$4	user@etherhost.Berkeley

# pass test addresses off to MMDF
R$+@UDel-Test		$#mmdf$@UDel-Test$:$1		user.host@udel-relay

# other non-local names will be kicked upstairs
R$+@$+			$#ether$@$F$:$1@$2		user@some.where

# everything else is a local name
R$+			$#local$:$1			local names

########################################
###  Host dependent address cleanup  ###
########################################

S8
R$*$=U!$+@$+		$3@$4				drop uucp forward


include(tcpm.m4)
include(mmdfm.m4)
include(suucpm.m4)
