#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
ifdef(`EXTERNAL_VERSION', EXTERNAL_VERSION, `#')
#	@(#)proto.mc	1.20 (Berkeley) 1/25/89
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

`#' file containing our internet aliases
Fw/etc/sendmail.cw

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
ifdef(`INTERNET_RELAY',
`include(../sitedep/nicregistered.m4)')
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
ifdef(`UUCP_ONLY',,
`include(../m4/nstcpldm.m4)')
include(../m4/nstcpm.m4)
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

ifdef(`UUCP_ONLY',,
`#' resolve fake top level domains by forwarding to other hosts
`include(../m4/fake_domains.m4)'

`ifdef(`UUCP_RELAY',
`#' forward non-local UUCP traffic to our UUCP relay
R$*<@$*.UUCP>$*		$`#'tcpld$@$R$:$1<@$2.UUCP>	uucp mail)'

`ifdef(`ARPAKLUDGE',
`#' hide behind our internet relay when talking to people in the arpa domain
R$*<@$*.arpa>$*		$`#'tcp$@$2.arpa$:$1<@$2.arpa>$3	user@host.arpa

`#' but speak domains to them if they speak domains too
R$*<@$*>$*		$`#'tcpld$@$2$:$1<@$2>$3		user@host.domain,
`#' resolve SMTP traffic
`ifdef(`INTERNET_RELAY',
R$*<@$*.$D>$*		$`#'tcpld$@$2.$D$:$1<@$2.$D>$3	user@host.ourdomain
R$*<@$+>$*		$`#'tcp$@$2$:$1<@$2>$3		user@host.ourdomain,
R$*<@$+>$*		$`#'tcpld$@$2$:$1<@$2>$3	user@host.domain)')')

# remaining names must be local
R$+			$#local$:$1			everything else
