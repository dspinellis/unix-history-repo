divert(10)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)nstcpm.m4	1.22 (Berkeley) 2/15/89
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

