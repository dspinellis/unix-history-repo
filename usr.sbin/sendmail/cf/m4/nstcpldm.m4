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
#	@(#)nstcpldm.m4	1.9 (Berkeley) 1/24/89
#
divert(0)
############################################################
############################################################
#####
#####		Local Domain SMTP Mailer specification
#####
#####	Messages processed by this specification are assumed to remain
#####	the local domain.  Hence, they can refer to hosts that are
#####	not registered in the NIC host table.
#####
############################################################
############################################################

Mtcpld,	P=[IPC], F=mDFMueXLC, S=17, R=27, A=IPC $h, E=\r\n

S17

# cleanup forwarding a bit
R$*<$*>$*		$1$2$3				defocus
R$*			$:$>3$1				canonicalize
R$*%$*<@$w>		$:$>9$1%$2			user%localhost@localdomain

# pass <route-addr>'s through
R<@$+>$*		$@<@$[$1$]>$2			resolve <route-addr>

# map colons to dots everywhere
R$*:$*			$1.$2				map colons to dots

# output local host as user@host.domain
R$-			$@$1<@$w>			user w/o host
R$+<@$w>		$@$1<@$w>			this host
R$+<@$=w>		$@$1<@$w>			or an alias
R$+<@$->		$:$1<@$[$2$]>			ask nameserver
R$+<@$w>		$@$1<@$w>			this host
R$+<@$->		$@$1<@$2.$D>			if nameserver fails

# if not local, and not a "fake" domain, ask the nameserver
R$+<@$+.$~I>		$@$1<@$[$2.$3$]>		user@host.domain
R$+<@[$+]>		$@$1<@[$2]>			already ok

# output fake domains as user%fake@relay

ifdef(`BITNET_RELAY',
R$+<@$+.BITNET>		$@$1%$2.BITNET<@$B>		user@host.bitnet)
ifdef(`CSNET_RELAY',
R$+<@$+.CSNET>		$@$1%$2.CSNET<@$C>		user@host.CSNET)
R$+<@$+.UUCP>		$@$2!$1<@$w>			user@host.UUCP


S27

# cleanup
R$*<$*>$*		$1$2$3				defocus
R$*			$:$>3$1				now canonical form
R$*%$*<@$w>		$:$>9$1%$2			user%localhost@localdomain

# pass <route-addr>'s through
R<@$+>$*		$@<@$[$1$]>$2			resolve <route-addr>

# map colons to dots everywhere
R$*:$*			$1.$2				map colons to dots

# output local host as user@host.domain
R$-			$@$1<@$w>			user w/o host
R$+<@$w>		$@$1<@$w>			this host
R$+<@$=w>		$@$1<@$w>			or an alias
R$+<@$->		$:$1<@$[$2$]>			ask nameserver
R$+<@$w>		$@$1<@$w>			this host
R$+<@$->		$@$1<@$2.$D>			if nameserver fails

# if not local, and not a "fake" domain, ask the nameserver
R$+<@$+.$~I>		$@$1<@$[$2.$3$]>		user@host.domain
R$+<@[$+]>		$@$1<@[$2]>			already ok

# output fake domains as user%fake@relay

ifdef(`BITNET_RELAY',
R$+<@$+.BITNET>		$@$1%$2.BITNET<@$B>		user@host.BITNET)
ifdef(`CSNET_RELAY',
R$+<@$+.CSNET>		$@$1%$2.CSNET<@$C>		user@host.CSNET)
R$+<@$+.UUCP>		$@$2!$1				user@host.UUCP

