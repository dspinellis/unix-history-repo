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
#	@(#)ucbvax.mc	5.10 (Berkeley) 1/3/86
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



############################################################
###	local info
############################################################

# internet hostname
Cwucbvax vax k UCB-VAX Berkeley UCB-C70 UCB LOCAL

# override hostname to use ARPANET name
DwUCB-VAX

# uucp hostnames
DUucbvax
CUucbvax 

# berknet hostname
DBUCBVAX

# UUCP connections on cad
DWcad
CWharris
CWhpda
CWjupiter
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

# third alternate UUCP connection and hosts
# (currently unused)
#DYmiro
#CYucbbugs ug bugs ucbugs

# known computer center hosts and gateway
include(cchosts.m4)

# known uucp connections with a smart uucp
CMdecvax

# we have full sendmail support here
Oa

include(csbase.m4)

include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve explicitly arpanet names (to avoid with machine name "arpa" below)
R$*<@$*$-.ARPA>$*	$#tcp$@$3.ARPA$:$1<@$2$3.ARPA>$4	user@domain.ARPA

# resolve names that can go via the ethernet
R$*<@$*$=S>$*		$#ether$@$3$:$1<@$2$3>$4	user@etherhost

# resolve berknet names
R<@$=Z>:$+		$#berk$@$1$:$2			@berkhost: ...
R$+<@$=Z>		$#berk$@$2$:$1			user@berknethost

# resolve names destined for the computer center
R$*<@$+.CC>$*		$#ether$@$C$:$1<@$2>$3		user@host

# resolve explicit known foreign networks
R$*<@$+.BITNET>$*	$#ether$@$C$:$1<@$2.BITNET>$3	user@host.BITNET
R$*<@$+.CSNET>$*	$#tcp$@CSNET-RELAY.ARPA$:$1<@$2.CSNET>$3	user@host.CSNET
R$*<@$+.DEC>$*		$#tcp$@decwrl.ARPA$:$1<@$2.DEC>$3	user@host.DEC
R$*<@$+.MailNET>$*	$#tcp$@MIT-Multics.ARPA$:$1<@$2.MailNET>$3	user@host.MailNET

# resolve nonlocal UUCP links
R$*<@$*$=W.UUCP>$*	$#ether$@$W$:$1<@$2$3.UUCP>$4	user@host.UUCP
R$*<@$*$=X.UUCP>$*	$#ether$@$X$:$1<@$2$3.UUCP>$4	user@host.UUCP
R$*<@$*$=Y.UUCP>$*	$#ether$@$Y$:$1<@$2$3.UUCP>$4	user@host.UUCP
R$*<@$*$=Y>$*		$#ether$@$Y$:$1<@$2$3.UUCP>$4	user@host.UUCP

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
R$+<@UDel-Test>		$#ether$@ucbarpa$:$1@Udel-Test	user.host@UDel-Relay

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#tcp$@$2$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else

########################################
###  Host dependent address cleanup  ###
########################################

S8
R$*$=U!$+@$+		$3@$4				drop uucp forward


include(berkm.m4)
include(uucpm.m4)

############################################################
############################################################
#####
#####		UUCP Smart Mailer specification
#####		(handles multiple recipients)
#####
#####		@(#)suucpm.m4	1.0		4/1/85
#####
############################################################
############################################################



Msuucp,	P=/usr/bin/uux, F=mDFMhuU, S=13, R=23, M=100000,
	A=uux - -r $h!rmail ($u)
