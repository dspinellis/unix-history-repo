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
#	@(#)ucbvax.mc	1.38 (Berkeley) 4/9/88
#
sinclude(buildinfo)dnl
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

# internet hostnames
Cwucbvax vax k UCB-VAX Berkeley UCB-C70 UCB

# UUCP hostnames
DUucbvax
CUucbvax 

# local UUCP connections
include(../machdep/uucp.ucbvax.m4)dnl

# UUCP connections on ucbarpa
DWucbarpa.Berkeley.EDU
define(`CV', CW)dnl
include(../machdep/uucp.ucbarpa.m4)dnl
undefine(`CV')dnl

# UUCP connections on ucbcad
DXcad.Berkeley.EDU
define(`CV', CX)dnl
include(../machdep/uucp.cad.m4)dnl
undefine(`CV')dnl

# UUCP connections on cogsci
DYcogsci.Berkeley.EDU
define(`CV', CY)dnl
include(../machdep/uucp.cogsci.m4)dnl
undefine(`CV')dnl

# known uucp connections with a smart uucp
CMdecvax

# we have full sendmail support here
Oa

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

# addition to Post-rewrite Rule
R$+%$=w@$=w.EDU		$1@$w			u%UCB@UCB.edu => u@UCB.berk.edu
R$+%$=w@$=w.$=w.EDU	$1@$w			u%UCB@UCB.berk.edu => u@UCB

include(../m4/rule3.m4)
include(../m4/rule5.m4)

###################
###   Mailers   ###
###################

include(../m4/localm.m4)
define(`m4UUCP',TRUE)
include(../m4/suucpm.m4)
include(../m4/uucpm.m4)
include(../m4/smtpuucpm.m4)
include(../m4/nstcpm.m4)
include(../m4/nstcpldm.m4)

#####################
###   Rule Zero   ###
#####################

include(../m4/rule0.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve SMTP UUCP connections
include(../machdep/smtpuucp.ucbvax.m4)

# resolve local UUCP links
R<@$=V.UUCP>:$+		$#uucp$@$1$:$1:$2		@host.UUCP: ...
R$+<@$=V.UUCP>		$#uucp$@$2$:$1			user@host.UUCP

# resolve explicit arpanet names (to avoid with machine name "arpa" below)
R$*<@$*$-.ARPA>$*	$#tcp$@$3.ARPA$:$1<@$2$3.ARPA>$4	user@domain.ARPA

# resolve fake top level domains by forwarding to other hosts
include(../m4/fake_domains.m4)

# resolve non-local UUCP links
R$*<@$=W.UUCP>$*	$#tcpld$@$W$:$1<@$2.UUCP>$3	user@host.UUCP
R$*<@$=X.UUCP>$*	$#tcpld$@$X$:$1<@$2.UUCP>$3	user@host.UUCP
R$*<@$=Y.UUCP>$*	$#tcpld$@$Y$:$1<@$2.UUCP>$3	user@host.UUCP

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

# local domain sites
R$*<@$*.$D>$*		$#tcpld$@$2.$D$:$1<@$2.$D>$3	user@host.our.domain
R$*<@$->$*		$#tcpld$@$2.$D$:$1<@$2.$D>$3	user@host
R$*<@$-.UUCP>$*		$#tcpld$@$2.$D$:$1<@$2.$D>$3	user@host.UUCP

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#tcp$@$2$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else

########################################
###  Host dependent address cleanup  ###
########################################

S8
R$*$=U!$+@$+		$3@$4				drop uucp forward
