#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)degas.mc	5.1 (Berkeley) 6/10/85
#
############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# internet hostname
Cwucbdegas degas

# UUCP name
DUucbdegas degas
CUucbdegas degas

#local UUCP connections
CWiris

include(csbase.m4)
include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve names we can handle locally
R<@$=W.UUCP>:$+		$#uucp$@$1$:$2			@host.UUCP: ...
R$+<@$=W.UUCP>		$#uucp$@$2$:$1			user@host.UUCP

# resolve names that can go via the ethernet
R$*<@$*$=S.LOCAL>$*	$#ether$@$3$:$1<@$2$3.$D>$4	user@etherhost

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#ether$@$R$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else

include(uucpm.m4)
