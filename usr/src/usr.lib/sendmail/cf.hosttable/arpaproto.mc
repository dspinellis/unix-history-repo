#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)arpaproto.mc	5.2 (Berkeley) 9/17/85
#
############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	Prototype for Arpanet (TCP) only sites.  Modify this
#####	file as appropriate for your configuration.
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
