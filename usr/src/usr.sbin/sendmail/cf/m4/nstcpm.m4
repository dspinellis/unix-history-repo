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
#	@(#)nstcpm.m4	1.21 (Berkeley) 1/3/89
#
divert(0)
############################################################
############################################################
#####
#####		Internet SMTP Mailer specification
#####
#####	Messages processed by this specification are assumed to leave
#####	the local domain -- hence, they must be canonical according to
#####	RFC822 etc.  This means that machines not registered with
#####	the NIC must be hidden behind our Internet relay.
#####
############################################################
############################################################

Mtcp,	P=[IPC], F=mDFMueXLC, S=14, R=24, A=IPC $h, E=\r\n

S14

# pass <route-addr>'s through
R<@$+>$*		$@<@$[$1$]>$2			resolve <route-addr>

# map colons to dots everywhere
R$*:$*			$1.$2				map colons to dots

# output local host in user@host.domain syntax
R$-			$1<@$w>				user w/o host
R$+<@$=w>		$:$1<@$w>			this host
R$+<@$->		$:$1<@$[$2$]>			canonicalize into dom
R$+<@$->		$:$1<@$2.$D>			if nameserver fails
R$+<@$=N.$D>		$@$1<@$2.$D>			nic-reg hosts are ok
R$+<@$*.$D>		$@$1%$2.$D<@$A>			else -> u%h@gateway

# if not local, and not a "fake" domain, ask the nameserver
R$+<@$+.$~I>		$@$1<@$[$2.$3$]>		user@host.domain
R$+<@[$+]>		$@$1<@[$2]>			already ok

# output internal ("fake") domains as "user%host@relay"

ifdef(`BITNET_RELAY',
R$+<@$+.BITNET>		$@$1%$2.BITNET<@$B>		user@host.BITNET)
ifdef(`CSNET_RELAY',
R$+<@$+.CSNET>		$@$1%$2.CSNET<@$C>		user@host.CSNET)
R$+<@$+.UUCP>		$@$2!$1<@$w>			user@host.UUCP


S24

# put in <> kludge
R$*<$*>$*		$1$2$3				defocus
R$*			$:$>3$1				now canonical form

# pass <route-addr>'s through
R<@$+>$*		$@<@$[$1$]>$2			resolve <route-addr>

# map colons to dots everywhere.....
R$*:$*			$1.$2				map colons to dots

# output local host in user@host.domain syntax
R$-			$1<@$w>				user w/o host
R$+<@$=w>		$:$1<@$w>			this host
R$+<@$->		$:$1<@$[$2$]>			canonicalize into dom
R$+<@$->		$:$1<@$2.$D>			if nameserver fails
R$+<@$=N.$D>		$@$1<@$2.$D>			nic-reg hosts are ok
R$+<@$*.$D>		$@$1%$2.$D<@$A>			else -> u%h@gateway

# if not local, and not a "fake" domain, ask the nameserver
R$+<@$+.$~I>		$@$1<@$[$2.$3$]>		user@host.domain
R$+<@[$+]>		$@$1<@[$2]>			already ok

# Hide fake domains behind relays

ifdef(`BITNET_RELAY',
R$+<@$+.BITNET>		$@$1%$2.BITNET<@$B>		user@host.BITNET)
ifdef(`CSNET_RELAY',
R$+<@$+.CSNET>		$@$1%$2.CSNET<@$C>		user@host.CSNET)
R$+<@$+.UUCP>		$@$2!$1				user@host.UUCP

