############################################################
#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)ucbvax.mc	1.19 (Berkeley) 5/1/86
#
############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	This one is the big daddy.  There is no "upstairs"
#####	to bounce a message to -- except perhaps the arpanet.
#####
#####
############################################################
############################################################



######################
###   local info   ###
######################

# internet hostname
Cwucbvax vax k UCB-VAX Berkeley UCB-C70 UCB LOCAL

# override hostname to use ARPANET name
#DwUCB-VAX		remove override if using named

# uucp hostnames
DUucbvax
CUucbvax 

# berknet hostname
DBUCBVAX

# UUCP connections on ucbcad
DWcad
CWjupiter
CWharris
CWhpda
CWmasscomp masscom
CWsda
CWtektronix

# second alternate UUCP connection and hosts
DXkim
CXfranz
CXfateman
CXfisi
CXendotsew
CXfimus

# third alternate UUCP connection
DYucboz
CYucbwodan wodan
CYCTSB
CYaloft
CYaltos86

# known uucp connections with a smart uucp
CMdecvax

# we have full sendmail support here
Oa

#############################
###   Setup Information   ###
#############################

include(nsmacros.m4)
include(nsclasses.m4)
include(cchosts.m4)
include(nicregistered.m4)
include(uucphosts.m4)
include(berkhosts.m4)
include(version.m4)
include(boilerplate.m4)

###########################
###   Rewriting Rules   ###
###########################

include(prewriterule.m4)
include(postwriterule.m4)

# addition to Post-rewrite Rule
R$+%$=w@$=w.EDU		$1@$w			u%UCB@UCB.edu => u@UCB.berk.edu
R$+%$=w@$=w.$=w.EDU	$1@$w			u%UCB@UCB.berk.edu => u@UCB

include(rule3.m4)
include(rule5.m4)

###################
###   Mailers   ###
###################

include(localm.m4)
include(berkm.m4)
define(`m4UUCP',TRUE)
include(suucpm.m4)
include(uucpm.m4)
include(nstcpm.m4)
include(nstcpldm.m4)

#####################
###   Rule Zero   ###
#####################

include(rule0.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# if host is not in the class Q (class of valid uucp hosts), then assume
# it is to be sent via tcp.
R$*<@$*$~Q.UUCP>$*	$1<@$2$[$3$]>$4

# resolve explicitly arpanet names (to avoid with machine name "arpa" below)
R$*<@$*$-.ARPA>$*	$#tcp$@$3.ARPA$:$1<@$2$3.ARPA>$4	user@domain.ARPA

# resolve names destined for the computer center
R$*<@$=C.berkeley.edu>$*	$#tcpld$@$C$:$1<@$2>$3
R$*<@$=C>$*		$#tcpld$@$C$:$1<@$2>$3
R$*<@$+.CC>$*		$#tcpld$@$C$:$1<@$2>$3			user@host

# resolve berknet names
R<@$=Z>:$+		$#berk$@$1$:$2				@berkhost: ...
R$+<@$=Z>		$#berk$@$2$:$1				user@berknethost

# resolve explicit known foreign networks
R$*<@$+.Oz.au>$*	$#tcp$@seismo.css.gov$:$1<@$2.OZ.AU>$3	user@host.oz.au
R$*<@$+.BITNET>$*	$#tcpld$@$C$:$1<@$2.BITNET>$3		user@host.BITNET
R$*<@$+.CSNET>$*	$#tcp$@CSNET-RELAY.ARPA$:$1<@$2.CSNET>$3	user@host.CSNET
R$*<@$+.DEC>$*		$#tcp$@decwrl.dec.com$:$1<@$2.DEC>$3	user@host.DEC
R$*<@$+.MailNET>$*	$#tcp$@MIT-Multics.ARPA$:$1<@$2.MailNET>$3	user@host.MailNET

# resolve nonlocal UUCP links
R$*<@$*$=W.UUCP>$*	$#tcpld$@$W$:$1<@$2$3.UUCP>$4	user@host.UUCP
R$*<@$*$=X.UUCP>$*	$#tcpld$@$X$:$1<@$2$3.UUCP>$4	user@host.UUCP
R$*<@$*$=Y.UUCP>$*	$#tcpld$@$Y$:$1<@$2$3.UUCP>$4	user@host.UUCP
R$*<@$*$=Y>$*		$#tcpld$@$Y$:$1<@$2$3.UUCP>$4	user@host.UUCP

# this uucp stuff is wrong for domain uucp addresses
# - we should pass the whole "host.domain" to uucp so it can
#   find the best route.  But that depends on a uucp router
#   which doesn't exist here yet, so for now, we'll settle for
#   trying to route to the domain (pretending its a host).
#   Suitable L.sys entries can make this work.  If it doesn't
#   then returned mail will just say "dom unknown", which is true ..

# resolve smart UUCP links
R<@$=M.$-.UUCP>:$+	$#suucp$@$2$:@$1.$2.UUCP:$3	@host.domain.UUCP: ...
R<@$=M.UUCP>:$+		$#suucp$@$1$:$2			@host.UUCP: ...
R$+<@$=M.$-.UUCP>	$#suucp$@$3$:$1@$2.$3.UUCP	user@host.domain.UUCP
R$+<@$=M.UUCP>		$#suucp$@$2$:$1			user@host.UUCP

# resolve local UUCP links (all others)
R<@$+.$-.UUCP>:$+	$#uucp$@$2$:@$1.$2.UUCP:$3	@host.domain.UUCP: ...
R<@$-.UUCP>:$+		$#uucp$@$1$:$2			@host.UUCP: ...
R$+<@$+.$-.UUCP>	$#uucp$@$3$:$1@$2.$3.UUCP	user@host.domain.UUCP
R$+<@$-.UUCP>		$#uucp$@$2$:$1			user@host.UUCP

# resolve mmdf hack
R$+<@UDel-Test>		$#tcpld$@ucbarpa$:$1@Udel-Test	user.host@UDel-Relay

# check for local that has had .berkeley.edu stripped by previous rule
R$*<@$->$*		$1<@$2.$D>$3			user@host-within-this-domain

# local domain sites
R$*<@$*.berkeley.edu>$*	$#tcpld$@$2$:$1<@$2.$D>$3	user@host.berkeley.edu

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#tcp$@$2$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else

########################################
###  Host dependent address cleanup  ###
########################################

S8
R$*$=U!$+@$+		$3@$4				drop uucp forward
