############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	Customized for dbvax at University of Wisconsin.
#####	This configuration is highly experimental.
#####
#####		@(#)dbvax.mc	3.7		2/24/83
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# internet hostname
Cwdbvax

######################
#   General Macros   #
######################

# arpanet gateway
DAUWisc
CAUWisc UWVax

# local domain names
DDUWISC
CDUWISC

# major relay host
DRUWVAX
CRUWVAX

include(base.m4)

#######################
#   Rewriting rules   #
#######################

##### special local conversions
S6
R$*<@$+.$=D>$*		$1<@$2.LOCAL>$4			convert local domain
R$*<@$+.$=D.ARPA>$*	$1<@$2.LOCAL>$4
R$*<@$+.UUCP.LOCAL>$*	$1<@$2.UUCP>$3			make UUCP top level

include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve names that can go via the ethernet
R$*<@$*$=S>$*		$#ether$@$3$:$1<@$2$3>$4	user@etherhost

# resolve berknet names
R<@$=Z>:$+		$#berk$@$1$:$2			@berkhost: ...
R$+<@$=Z>		$#berk$@$2$:$1			user@berknethost

# resolve local UUCP links (all others)
R<@$-.UUCP>$+		$1!$2@$R.ARPA			@host.UUCP: ...
R$+<@$-.UUCP>		$2!$1@$R.ARPA			user@host.UUCP

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#tcp$@$3$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else

include(localm.m4)
include(ncpm.m4)
