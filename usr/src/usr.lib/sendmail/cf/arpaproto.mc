############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	Prototype for Arpanet (TCP) only sites.  Modify this
#####	file as appropriate for your configuration.
#####
#####		@(#)arpaproto.mc	3.3		2/24/83
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# internet hostname
DA$w

# domain
DD$w
CD

# official hostname
Dj$w.ARPA

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
