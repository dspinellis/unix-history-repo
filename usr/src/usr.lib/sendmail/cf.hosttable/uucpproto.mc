#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)uucpproto.mc	5.2 (Berkeley) 9/17/85
#
############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	Prototype for UUCP only sites.  Modify this
#####	file as appropriate for your configuration.
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# domain
DDUUCP
CDUUCP

# official hostname
Dj$w.$D

# UUCP name
DU$w

include(base.m4)

include(zerobase.m4)

###############################################
###  Machine dependent part of rulset zero  ###
###############################################

# resolve names we can handle locally
R<@$+.UUCP>:$+		$1!$2				to old format
R$+<@$+.UUCP>		$2!$1				to old format
R$-!$+			$#uucp$@$1$:$2			host!user

# everything else must be a local name
R$+			$#local$:$1			local names

include(uucpm.m4)
