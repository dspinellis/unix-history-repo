#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#  DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER
#  DANGER							  DANGER
#  DANGER	This is an EXPERIMENTAL configuration file	  DANGER
#  DANGER	Do not use this without talk to Phil Lapsley	  DANGER
#  DANGER							  DANGER
#  DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER
#
ifdef(`EXTERNAL_VERSION', EXTERNAL_VERSION, `#')
#	@(#)ucbproto.mc	1.15 (Berkeley) 3/31/88
#
sinclude(buildinfo)dnl
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

ifdef(`INTERNET_ALIASES',
INTERNET_ALIASES,
`#' internet aliases	REPLACE HOSTNAME WITH YOUR MACHINE NAME HERE
Cwucbmonet monet)

ifdef(`UUCP_NAME',
`#' uucp hostnames
UUCP_NAME
UUCP_ALIASES

`#' local UUCP connections
`include(UUCP_HOSTS_FILE)')

#############################
###   Setup Information   ###
#############################

include(../m4/nsmacros.m4)
include(../m4/nsclasses.m4)
include(../m4/nicregistered.m4)
include(../m4/version.m4)
include(../m4/boilerplate.m4)

###########################
###   Rewriting Rules   ###
###########################

include(../m4/prewriterule.m4)
include(../m4/postwriterule.m4)
include(../m4/rule3.m4)

###################
###   Mailers   ###
###################

include(../m4/localm.m4)
include(../m4/nstcpm.m4)
include(../m4/x-nssmtpm.m4)
ifdef(`UUCP_NAME',
`include(../m4/uucpm.m4)'
`include(../m4/rule5.m4)')
ifdef(`SMTPUUCP',
`include(../m4/smtpuucpm.m4)')

#####################
###   Rule Zero   ###
#####################

include(../m4/rule0.m4)

###############################################
###   Machine dependent part of Rule Zero   ###
###############################################

ifdef(`SMTPUUCP',
`# resolve SMTP UUCP connections'
`include(SMTPUUCP)')

ifdef(`UUCP_NAME',
`# resolve local UUCP connections'
`R<@$=V.UUCP>:$+		$#uucp$@$1$:$2				@host.UUCP:...'
`R$+<@$=V.UUCP>		$#uucp$@$2$:$1				user@host.UUCP'
)

# resolve fake top level domains by forwarding to other hosts
include(../m4/fake_domains.m4)

# forward UUCP traffic to our UUCP gateway
R$*<@$*.UUCP>$*		$#tcpld$@$F$:$1<@$2.UUCP>	uucp mail

# local domain sites
R$*<@$*.$D>$*		$#tcpld$@$2.$D$:$1<@$2.$D>$3	user@host.our.domain

# other non-local names 
R$*<@$+>$*		$#tcp$@$2$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else
