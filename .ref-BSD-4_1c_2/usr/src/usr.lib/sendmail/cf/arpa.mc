############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####		@(#)arpa.mc	3.25		2/24/83
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
CVsun

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

# catch names with ".ARPA" now so that they won't look like a Berkeley host name
R$*<@$+.ARPA>$*		$#ether$@$R$:$1<@$2.ARPA>$3	user@host.ARPA

# optimize names of known ethernet hosts
R$*<@$*$=S>$*		$#ether$@$3$:$1<@$2$3>$4	user@etherhost

# pass test addresses off to MMDF
R$+@UDel-Test		$#mmdf$@UDel-Test$:$1		user.host@udel-relay

# other non-local names will be kicked upstairs
R$+@$+			$#ether$@$R$:$1@$2		user@some.where

# everything else is a local name
R$+			$#local$:$1			local names

########################################
###  Host dependent address cleanup  ###
########################################

S8
R$*$=U!$+@$+		$3@$4				drop uucp forward


include(tcpm.m4)
include(mmdfm.m4)
include(uucpm.m4)
