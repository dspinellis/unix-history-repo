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
#	@(#)tcpm.m4	5.8 (Berkeley) 10/2/85
#
divert(0)
############################################################
############################################################
#####
#####		Arpanet TCP Mailer specification
#####
#####	Messages processed by this specification are assumed to leave
#####	the local domain -- hence, they must be canonical according to
#####	RFC822 etc.
#####
############################################################
############################################################

Mtcp,	P=[IPC], F=mDFMueXL, S=14, R=14, A=IPC $h, E=\r\n

S14

# pass <route-addr>'s through
R<@$+>$*		$@<@$[$1$]>$2			resolve <route-addr>

# map colons to dots everywhere.....
R$*:$*			$1.$2				map colons to dots

# handle the simple case....
R$+<@$-.ARPA>		$@$1<@$[$2.ARPA$]>		user@host.ARPA

# output local hosts in user@host.Berkeley.EDU syntax
R$+<@LOCAL>		$@$1<@$w.$A>			local names
R$+<@$+.LOCAL>		$@$1<@$2.$A>			local hosts
R$+<@$*$=S>		$@$1<@$2$3.$A>
R$+<@$*$=Z>		$@$1<@$2$3.$A>
R$+<@$+.BITNET>		$@$1%$2.BITNET<@$w.$A>		user@host.BITNET
R$+<@$+.CSNET>		$@$1%$2.CSNET<@CSNET-RELAY.ARPA>	user@host.CSNET
R$+<@$+.DEC>		$@$1%$2.DEC<@decwrl.ARPA>	user@host.DEC
R$+<@$+.UUCP>		$@$2!$1<@$w.$A>			user@host.UUCP

# handle other external cases
R$+<@$->		$@$1<@$[$2$]>			no .ARPA on simple names
#R$+<@$+.$-.ARPA>	$@$1%$2<@$[$3.ARPA$]>		approximate something
R$+<@[$+]>		$@$1<@[$2]>			already ok
R$+<@$+>		$@$1<@$[$2$]>			set for named

# convert remaining addresses to old format and externalize appropriately
R$+			$:$>5$1				=> old format
R$-:$+			$@$2<@$1.$A>			convert berk hosts
#R$+<@$+>		$@$1<@$2@$A>			pessmize
R$+			$:$1<@$w.$A>			tack on our hostname
R$+%$=R<@$=A>		$1<@$[$3$]>			strip unneeded relay
