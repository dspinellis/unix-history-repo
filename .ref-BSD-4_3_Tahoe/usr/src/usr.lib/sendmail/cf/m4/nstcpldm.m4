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
#	@(#)nstcpldm.m4	1.7 (Berkeley) 3/31/88
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

R$+<@$+.CSNET>		$@$1%$2.CSNET<@relay.cs.net>	user@host.CSNET
R$+<@$+.BITNET>		$@$1%$2.BITNET<@jade.berkeley.edu>	 user@host.bitnet
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

R$+<@$+.CSNET>		$@$1%$2.CSNET<@relay.cs.net>	user@host.CSNET
R$+<@$+.BITNET>		$@$1<@$2.BITNET>		user@host.BITNET
R$+<@$+.UUCP>		$@$2!$1				user@host.UUCP

