############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	Prototype for UUCP only sites.  Modify this
#####	file as appropriate for your configuration.
#####
#####		@(#)uucpproto.mc	3.3		2/24/83
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# domain
DD$w
CD

# official hostname
Dj$w.UUCP

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
