#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)lanleaf.mc	5.1 (Berkeley) 9/17/85
#
############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	Prototype for sites having a local area ethernet only.
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# domain
DDXXX
CDLOCAL XXX

# host on LAN with UUCP (or other) connection
DRXXX

# official hostname
Dj$w.$D

include(base.m4)

include(zerobase.m4)

###############################################
###  Machine dependent part of rulset zero  ###
###############################################

# resolve local ethernet names
R$*<@$*$-.$=D>$*	$#ether$@$3$:$1<@$2$3.$4>$5	user@host.LOCAL
R$*<@$->$*		$#ether$@$2$:$1<@$2>$3		user@host

# forward other domains to a relay host
R$*<@$+.$+>$*		$#ether$@$R$:$1<@$2.$3>$4	user@host.DOMAIN

# everything else must be a local name
R$+			$#local$:$1			local names

include(etherm.m4)
