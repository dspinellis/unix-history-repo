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
#	@(#)nstcpldm.m4	1.3 (Berkeley) 5/1/86
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
R$*%$*<@LOCAL>		$:$>9$1%$2			user%localhost@localdomain

# pass <route-addr>'s through
R<@$+>$*		$@<@$[$1$]>$2			resolve <route-addr>

# map colons to dots everywhere.....
R$*:$*			$1.$2				map colons to dots

# handle the simple case....
R$+<@$-.ARPA>		$@$1<@$[$2.ARPA$]>		user@host.ARPA

# fully qualify computer center names so the rest works nicely
R$+<@$=C>		$1<@$2.$D>		user@host.CC

# output local host in user@host.BERKELEY.EDU syntax
R$+<@LOCAL>		$:$1<@$w>				this host
R$+<@$->		$:$1<@$[$2$]>			check for local domain
R$+<@$+.LOCAL>		$@$1<@$2.$D>			nic-reg local hosts
R$+<@$=w>		$@$1<@$w.$D>			user@this_host.berkeley.edu

# output local hosts in user%host@BERKELEY.EDU syntax

R$+<@$+.CSNET>		$@$1%$2.CSNET<@CSNET-RELAY.ARPA>	user@host.CSNET
R$+<@$+.DEC>		$@$1%$2.DEC<@decwrl.dec.com>	user@host.DEC
R$+<@$+.OZ.AU>		$@$1%$2.OZ.AU<@seismo.css.gov>	user@host.OZ.AU
R$+<@$+.UUCP>		$@$2!$1<@$w>			user@host.UUCP
R$+<@$+.$=P>		$@$1<@$[$2.$3$]>		user@host.{mil,edu,...}
R$+<@$+.$=K>		$@$1%$2.$3<@$w>			user@host.fake
R$+<@$*$=Z>		$@$1%$2$3<@$w>

# handle other external cases
R$+<@$->		$@$1<@$[$2$]>			no .ARPA on simple names
#R$+<@$+.$-.ARPA>	$@$1%$2<@$[$3.ARPA$]>		approximate something
R$+<@[$+]>		$@$1<@[$2]>			already ok
R$+<@$+>		$@$1<@$[$2$]>			set for named

# convert remaining addresses to old format and externalize appropriately
R$+			$:$>5$1				=> old format
R$-:$+			$@$1.$2<@$w>			convert berk hosts
#R$+<@$+>		$@$1%$2<@$w>			pessmize
R$+			$:$1<@$w>			tack on our hostname
R$+<@$=w>		$:$1<@$w.$D>			wher $w doesn't have .b.e
R$+%$=R<@$=A>		$1<@$[$3$]>			strip unneeded relay

S27

# put in <> kludge
R$*<$*>$*		$1$2$3				defocus
R$*			$:$>3$1				now canonical form
R$*%$*<@LOCAL>		$:$>9$1%$2			user%localhost@localdomain

# pass <route-addr>'s through
R<@$+>$*		$@<@$[$1$]>$2			resolve <route-addr>

# map colons to dots everywhere.....
R$*:$*			$1.$2				map colons to dots

# handle the simple case....
R$+<@$-.ARPA>		$@$1<@$[$2.ARPA$]>		user@host.ARPA

# fully qualify computer center names so the rest works nicely
R$+<@$=C>		$1<@$2.$D>		user@host.CC

# output local host in user@host.BERKELEY.EDU syntax
R$+<@$=w>		$@$1<@$w>			this host
R$+<@LOCAL>		$@$1<@$w>			this host
R$+<@$+.LOCAL>		$@$1<@$2.$D>			local hosts

# output local hosts in user%host@BERKELEY.EDU syntax

R$+<@$+.CSNET>		$@$1%$2.CSNET<@CSNET-RELAY.ARPA>	user@host.CSNET
R$+<@$+.DEC>		$@$1%$2.DEC<@decwrl.dec.com>	user@host.DEC
R$+<@$+.OZ.AU>		$@$1%$2.OZ.AU<@seismo.css.gov>	user@host.OZ.AU
R$+<@$+.UUCP>		$@$2!$1				user@host.UUCP
R$+<@$+.BITNET>		$@$1<@$2.BITNET>		user@host.BITNET
R$+<@$+.$=P>		$@$1<@$[$2.$3$]>		user@host.{mil,edu,...}
R$+<@$+.$=K>		$@$1%$2.$3<@$w>			user@host.fake
R$+<@$=Z>		$@$1<@$2>			berkhosts

# handle other external cases
R$+<@$->		$@$1<@$[$2$]>			no .ARPA on simple names
#R$+<@$+.$-.ARPA>	$@$1%$2<@$[$3.ARPA$]>		approximate something
R$+<@[$+]>		$@$1<@[$2]>			already ok
R$+<@$+>		$@$1<@$[$2$]>			set for named

# convert remaining addresses to old format and externalize appropriately
R$+			$:$>5$1				=> old format
R$-:$+			$@$1.$2<@$w>			convert berk hosts
#R$+<@$+>		$@$1%$2<@$w>			pessmize
R$+			$:$1<@$w>			tack on our hostname
R$+%$=R<@$=A>		$1<@$[$3$]>			strip unneeded relay

