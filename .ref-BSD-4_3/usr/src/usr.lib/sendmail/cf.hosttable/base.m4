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
#	@(#)base.m4	5.7 (Berkeley) 10/21/85
#
divert(0)
############################################################
#
#	General configuration information
#
#	This information is basically just "boiler-plate"; it must be
#	there, but is essentially constant.
#
#	Information in this file should be independent of location --
#	i.e., although there are some policy decisions made, they are
#	not specific to Berkeley per se.
#
############################################################

include(version.m4)

##########################
###   Special macros   ###
##########################

# my name
DnMAILER-DAEMON
# UNIX header format
DlFrom $g  $d
# delimiter (operator) characters
Do.:%@!^=/[]
# format of a total name
Dq$g$?x ($x)$.
# SMTP login message
De$j Sendmail $v/$Z ready at $b

# forwarding host -- redefine this if you can't talk to the relay directly
DF$R

###################
###   Options   ###
###################

# location of alias file
OA/usr/lib/aliases
# wait up to ten minutes for alias file rebuild
Oa10
# substitution for space (blank) characters
OB.
# (don't) connect to "expensive" mailers
#Oc
# default delivery mode (deliver in background)
Odbackground
# temporary file mode
OF0600
# default GID
Og1
# location of help file
OH/usr/lib/sendmail.hf
# log level
OL9
# default network name
ONARPA
# default messages to old style
Oo
# queue directory
OQ/usr/spool/mqueue
# read timeout -- violates protocols
Or2h
# status file
OS/usr/lib/sendmail.st
# queue up everything before starting transmission
Os
# default timeout interval
OT3d
# time zone names (V6 only)
OtPST,PDT
# default UID
Ou1
# wizard's password
OW*
# load average at which we just queue messages
Ox8
# load average at which we refuse connections
OX12

###############################
###   Message precedences   ###
###############################

Pfirst-class=0
Pspecial-delivery=100
Pbulk=-60
Pjunk=-100

#########################
###   Trusted users   ###
#########################

Troot
Tdaemon
Tuucp
Teric
Tnetwork

#############################
###   Format of headers   ###
#############################

H?P?Return-Path: <$g>
HReceived: $?sfrom $s $.by $j ($v/$Z)
	id $i; $b
H?D?Resent-Date: $a
H?D?Date: $a
H?F?Resent-From: $q
H?F?From: $q
H?x?Full-Name: $x
HSubject:
# HPosted-Date: $a
# H?l?Received-Date: $b
H?M?Resent-Message-Id: <$t.$i@$j>
H?M?Message-Id: <$t.$i@$j>

###########################
###   Rewriting rules   ###
###########################


################################
#  Sender Field Pre-rewriting  #
################################
S1
#R$*<$*>$*		$1$2$3				defocus

###################################
#  Recipient Field Pre-rewriting  #
###################################
S2
#R$*<$*>$*		$1$2$3				defocus

#################################
#  Final Output Post-rewriting  #
#################################
S4

R@			$@				handle <> error addr

# externalize local domain info
R$*<$*LOCAL>$*		$1<$2$D>$3			change local info
R$*<$*LOCAL.ARPA>$*	$1<$2$D>$3			change local info
R$*<$*.CC>$*		$1$2$3				strip .CC
R$*<$+>$*		$1$2$3				defocus
R@$+:@$+:$+		@$1,@$2:$3			<route-addr> canonical

# UUCP must always be presented in old form
R$+@$-.UUCP		$2!$1				u@h.UUCP => h!u

# delete duplicate local names -- mostly for arpaproto.mc
R$+%$=w@$=w		$1@$3				u%UCB@UCB => u@UCB
R$+%$=w@$=w.ARPA	$1@$3.ARPA			u%UCB@UCB => u@UCB

###########################
#  Name Canonicalization  #
###########################
S3

# handle "from:<>" special case
R<>			$@@				turn into magic token

# basic textual canonicalization -- note RFC733 heuristic here
R$*<$*<$*<$+>$*>$*>$*	$4				3-level <> nesting
R$*<$*<$+>$*>$*		$3				2-level <> nesting
R$*<$+>$*		$2				basic RFC821/822 parsing
R$+ at $+		$1@$2				"at" -> "@" for RFC 822

# make sure <@a,@b,@c:user@d> syntax is easy to parse -- undone later
R@$+,$+			@$1:$2				change all "," to ":"

# localize and dispose of route-based addresses
R@$+:$+			$@$>6<@$1>:$2			handle <route-addr>

# more miscellaneous cleanup
R$+			$:$>8$1				host dependent cleanup
R$+:$*;@$+		$@$1:$2;@$3			list syntax
R$+:$*;			$@$1:$2;			list syntax
R$+@$+			$:$1<@$2>			focus on domain
R$+<$+@$+>		$1$2<@$3>			move gaze right
R$+<@$+>		$@$>6$1<@$2>			already canonical

# convert old-style addresses to a domain-based address
R$-:$+			$@$>6$2<@$1>			host:user
R$+^$+			$1!$2				convert ^ to !
R$-.$+!$+		$@$>6$3<@$1.$2>			host.domain!user
R$-!$+			$@$>6$2<@$1.UUCP>		host!user (uucp)
R$+%$+			$@$>6$1<@$2>			user%host
R$-=$+			$@$>6$2<@$1.BITNET>		host=user (bitnet)
R$-.$+			$@$>6$2<@$1>			host.user

# always include local and tcp mailers (tcp for [w.x.y.z]-style addrs)
include(localm.m4)
include(tcpm.m4)
