#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)monet.mc	1.8 (Berkeley) 2/26/86
#
############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
############################################################
############################################################



##################
#   local info   #
##################

# internet hostname
Cwucbmonet monet monet-cs ucbmonet-cs LOCAL

#############################
###   Setup Information   ###
#############################

include(nsmacros.m4)
include(nsclasses.m4)
include(cchosts.m4)
include(nicregistered.m4)
include(berkhosts.m4)
include(version.m4)
include(boilerplate.m4)

###########################
###   Rewriting Rules   ###
###########################

include(prewriterule.m4)
include(postwriterule.m4)
include(rule3.m4)

###################
###   Mailers   ###
###################

include(localm.m4)
include(nstcpm.m4)
include(nstcpldm.m4)

#####################
###   Rule Zero   ###
#####################

include(rule0.m4)

###############################################
###   Machine dependent part of Rule Zero   ###
###############################################

# resolve names destined for the computer center
R$*<@$=C.berkeley.edu>$*	$#tcpld$@$C$:$1<@$2>$3
R$*<@$=C>$*		$#tcpld$@$C$:$1<@$2>$3
R$*<@$+.CC>$*		$#tcpld$@$C$:$1<@$2>$3	user@host.CC

# resolve names destined for berknet sites
R<@$=Z>:$+		$#tcpld$@$B$:$2@$1		@berkhost: ...
R$+<@$=Z>		$#tcpld$@$B$:$1@$2		user@berknethost

# resolve explicit known foreign networks
R$*<@$+.Oz.au>$*	$#tcp$@seismo.css.gov$:$1<@$2.OZ.AU>$3	down-under
R$*<@$+.BITNET>$*	$#tcpld$@$C$:$1<@$2.BITNET>$3	user@host.BITNET
R$*<@$+.CSNET>$*	$#tcp$@CSNET-RELAY.ARPA$:$1<@$2.CSNET>$3	user@host.CSNET
R$*<@$+.DEC>$*		$#tcp$@decwrl.dec.com$:$1<@$2.DEC>$3	user@host.DEC
R$*<@$+.MailNET>$*	$#tcp$@MIT-Multics.ARPA$:$1<@$2.MailNET>$3	user@host.MailNET
R$*<@$*.UUCP>$*		$#tcpld$@$F$:$1<@$2.UUCP>		uucp mail

# local domain sites
R$*<@$*.berkeley.edu>$*	$#tcpld$@$2$:$1<@$2.$A>$3	user@host.berkeley.edu

# other non-local names 
R$*<@$+>$*		$#tcp$@$2$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else
