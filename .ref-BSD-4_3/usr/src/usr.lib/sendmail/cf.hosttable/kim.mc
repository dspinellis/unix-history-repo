#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)kim.mc	5.3 (Berkeley) 1/3/86
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
Cwkim ucbkim n

# UUCP name
DUucbkim
CUucbkim

# UUCP hosts that we talk to
CWfranz
CWfisi
CWfateman
CWendotsew
CWfimus

include(csbase.m4)

include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve names that we can handle locally
R<@$=W.UUCP>$+		$#uucp$@$1$:$2			@host.UUCP: ...
R$+<@$=W.UUCP>		$#uucp$@$2$:$1			user@host.UUCP

# resolve names that can go via the ethernet
R$*<@$*$=S.LOCAL>$*	$#ether$@$3$:$1<@$2$3.$D>$4	user@etherhost

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#ether$@$R$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else

include(uucpm.m4)
