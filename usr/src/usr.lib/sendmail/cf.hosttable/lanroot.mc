#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)lanroot.mc	5.1 (Berkeley) 9/17/85
#
############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	Prototype for sites having a local area ethernet running
#####	TCP plus one site with UUCP connections.
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# domain
DDXXX
CDLOCAL XXX

# official hostname
Dj$w.$D

# UUCP name
DU$w

include(base.m4)

include(zerobase.m4)

###############################################
###  Machine dependent part of rulset zero  ###
###############################################

# resolve UUCP names
R<@$+.UUCP>:$+		$1!$2				to old format
R$+<@$+.UUCP>		$2!$1				to old format
R$-!$+			$#uucp$@$1$:$2			host!user

# resolve local ethernet names
R$*<@$*$-.$=D>$*	$#ether$@$3$:$1<@$2$3.$4>$5	user@host.LOCAL
R$*<@$->$*		$#ether$@$2$:$1<@$2>$3		user@host

# could forward other domains to a relay host here....
#R$*<@$+.$+>$*		$#uucp$@$R$:$1<@$2.$3>$4	user@host.DOMAIN

# everything else must be a local name
R$+			$#local$:$1			local names

include(etherm.m4)
include(uucpm.m4)
