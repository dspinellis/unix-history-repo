############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	This one is the big daddy.  There is no "upstairs"
#####	to bounce a message to -- except perhaps the arpanet.
#####
#####		@(#)ucbvax.mc	3.30		3/5/83
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# internet hostname
Cwucbvax vax k UCB-VAX Berkeley UCB-C70 UCB LOCAL

# uucp hostnames
DUucbvax
CUucbvax ernie

# berknet hostname
DBUCBVAX

# UUCP connections on arpavax
DWucbarpa
CWsun

# known computer center hosts and gateway
include(cchosts.m4)

# we have full sendmail support here
Oa

include(csbase.m4)

# names of NCP hosts
include(ncphosts.m4)

include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# forward NCP mail through udel-relay
R$+<@$=N>		$#tcp$@$N$:$1.$2<@$N>		user.host@UDEL-RELAY
R$+<@$=N.ARPA>		$#tcp$@$N$:$1.$2<@$N>		user.host@UDEL-RELAY

# resolve explicitly arpanet names (to avoid with machine name "arpa" below)
R$*<@$*$-.ARPA>$*	$#tcp$@$3$:$1<@$2$3.ARPA>$4	user@domain.ARPA

# resolve names that can go via the ethernet
R$*<@$*$=S>$*		$#ether$@$3$:$1<@$2$3>$4	user@etherhost

# resolve berknet names
R<@$=Z>:$+		$#berk$@$1$:$2			@berkhost: ...
R$+<@$=Z>		$#berk$@$2$:$1			user@berknethost

# resolve names destined for the computer center
R$*<@$+.CC>$*		$#ether$@$C$:$1<@$2.CC>$3	user@host.CC
R$*<@$+.BITNET>$*	$#ether$@$C$:$1<@$2.BITNET>$3	user@host.BITNET

# resolve nonlocal UUCP links
R$*<@$*$=W.UUCP>$*	$#ether$@$W$:$1<@$2$3.UUCP>$4	user@host.UUCP

# resolve local UUCP links (all others)
R<@$-.UUCP>:$+		$#uucp$@$1$:$2			@host.UUCP: ...
R$+<@$-.UUCP>		$#uucp$@$2$:$1			user@host.UUCP

# resolve mmdf hack
R$+<@UDel-Test>		$#ether$@ucbarpa$:$1@Udel-Test	user.host@UDel-Relay

# other non-local names will be kicked upstairs
R$*<@$*$->$*		$#tcp$@$3$:$1<@$2$3>$4		user@host

# remaining names must be local
R$+			$#local$:$1			everything else

########################################
###  Host dependent address cleanup  ###
########################################

S8
R$*$=U!$+@$+		$3@$4				drop uucp forward


include(berkm.m4)
include(uucpm.m4)
include(tcpm.m4)
