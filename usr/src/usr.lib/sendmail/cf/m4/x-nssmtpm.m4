divert(10)
#
#
#	DANGER	DANGER	DANGER	DANGER	DANGER	DANGER	DANGER
#	DANGER	This configuration file is EXPERIMENTAL	DANGER
#	DANGER	If you don't talk to Phil Laplsey, 	DANGER
#	DANGER	don't use it!!!!!!			DANGER
#	DANGER	DANGER	DANGER	DANGER	DANGER	DANGER	DANGER
#
#
#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	%W%	(Berkeley) %G%
#
divert(0)
############################################################
############################################################
#####
#####		Local Domain TCP Mailer specification
#####
#####	Messages processed by this specification are assumed to remain
#####	the local domain -- hence, they must be canonical according to
#####	RFC822 etc.
#####
#####	This mailer is to be used with the Berkeley Name Server.
#####
############################################################
############################################################

Mtcpld,	P=[IPC], F=mDFMueXLC, S=17, R=27, A=IPC $h, E=\r\n

S17

# output local host as user@host.domain
R$-			$:$1<@$w>			user w/o host
R$*<@$=w>$*		$:$1<@$w>$2			user@us
R$*<@$->$*		$:$1<@$[$2$]>$3			ask nameserver
R$*<@$->$*		$:$1<@$2.$D>$3			if nameserver fails

# if not local, and not a "fake" domain, ask the nameserver
R$*<@$+.$~I>$*		$:$1<@$[$2.$3$]>$4		user@host.domain
R$*<@[$+]>$*		$:$1<@[$2]>$3			dotted quad

# pass <route-addr>'s through
R<@$+>$*		$@<@$[$1$]>$2			resolve <route-addr>

# output fake domains as user%fake@relay

R$+<@$+.CSNET>		$@$1%$2.CSNET<@relay.cs.net>	user@host.CSNET
R$+<@$+.BITNET>		$@$1%$2.BITNET<@jade.berkeley.edu>  user@host.bitnet
R$+<@$+.UUCP>		$@$2!$1<@$w>			user@host.UUCP


S27

# output local host as user@host.domain
R$-			$:$1<@$w>			user w/o host
R$*<@$=w>$*		$:$1<@$w>$2			user@us
R$*<@$->$*		$:$1<@$[$2$]>$3			ask nameserver
R$*<@$->$*		$:$1<@$2.$D>$3			if nameserver fails

# if not local, and not a "fake" domain, ask the nameserver
R$+<@$+.$~I>		$@$1<@$[$2.$3$]>		user@host.domain
R$+<@[$+]>		$@$1<@[$2]>			already ok

# pass <route-addr>'s through
R<@$+>$*		$@<@$[$1$]>$2			resolve <route-addr>

# output fake domains as user%fake@relay

R$+<@$+.CSNET>		$@$1%$2.CSNET<@relay.cs.net>	user@host.CSNET
R$+<@$+.BITNET>		$@$1<@$2.BITNET>		user@host.BITNET
R$+<@$+.UUCP>		$@$2!$1				user@host.UUCP
