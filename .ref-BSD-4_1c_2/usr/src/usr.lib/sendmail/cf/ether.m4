############################################################
############################################################
#####
#####		BASIC ETHERNET RULES
#####
#####		@(#)ether.m4	3.10		3/5/83
#####
############################################################
############################################################



include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# handle explicit ".ARPA" names to avoid conflict with local host "arpa"
R$*<@$+.ARPA>$*		$#ether$@$R$:$1<@$2.ARPA>$3	user@host.ARPA

# resolve names that can go via the ethernet
R$*<@$*$=S>$*		$#ether$@$3$:$1<@$2$3>$4	user@etherhost

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#ether$@$R$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else
