############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	Prototype for Arpanet (TCP) only sites.  Modify this
#####	file as appropriate for your configuration.
#####
#####		@(#)arpaproto.mc	4.1		7/25/83
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# internet hostname
DA$w

# domain
DDARPA
CDARPA

# official hostname
Dj$w.$D

include(base.m4)

include(zerobase.m4)

###############################################
###  Machine dependent part of rulset zero  ###
###############################################

# resolve names we can handle locally
R$*<@$*$-.ARPA>$*	$#tcp$@$3$:$1<@$2$3.ARPA>$4	user@tcphost.ARPA
R$*<@$*$->$*		$#tcp$@$3$:$1<@$2$3>$4		user@tcphost.ARPA

# everything else must be a local name
R$+			$#local$:$1			local names

include(localm.m4)
include(tcpm.m4)
