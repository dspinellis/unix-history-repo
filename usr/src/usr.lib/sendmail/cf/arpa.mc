############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####		@(#)arpa.mc	4.2		8/6/83
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
#CVsun

# we have full sendmail support here
Oa

include(csbase.m4)

include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve names we can handle locally
R<@$=V.UUCP>:$+		$#uucp$@$1$:$2			@host.UUCP:...
R$+<@$=V.UUCP>		$#uucp$@$2$:$1			user@host.UUCP

# optimize names of known ethernet hosts
R$*<@$*$=S.LOCAL>$*	$#ether$@$3$:$1<@$2$3.$D>$4	user@etherhost.Berkeley

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
