############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	Prototype for UUCP only sites.  Modify this
#####	file as appropriate for your configuration.
#####
#####		@(#)uucpproto.mc	4.1		7/25/83
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

include(localm.m4)
include(uucpm.m4)
