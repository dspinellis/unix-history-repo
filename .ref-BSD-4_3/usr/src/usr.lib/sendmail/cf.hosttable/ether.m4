divert(10)
#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)ether.m4	5.3 (Berkeley) 10/9/85
#
divert(0)
############################################################
############################################################
#####
#####		BASIC ETHERNET RULES
#####
############################################################
############################################################



include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve names that can go via the ethernet
R$*<@$*$=S.LOCAL>$*	$#ether$@$3$:$1<@$2$3>$4	user@etherhost

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#ether$@$F$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else
