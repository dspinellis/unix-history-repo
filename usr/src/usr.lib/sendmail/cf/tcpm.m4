############################################################
############################################################
#####
#####		Arpanet TCP Mailer specification
#####
#####		@(#)tcpm.m4	4.1		7/25/83
#####
############################################################
############################################################

Mtcp,	P=[IPC], F=msDFMueXL, S=14, R=14, A=IPC $h, E=\r\n

S14

# pass <route-addr>'s through
R<@$+>$*		$@<@$1>$2			resolve <route-addr>

# map colons to dots everywhere.....
R$*:$*			$1.$2				map colons to dots

# handle the simple case....
R$+<@$-.ARPA>		$@$1<@$2.ARPA>			user@host.ARPA

# fully qualify computer center names so the rest works nicely
R$+<@$+.CC>		$1<@$2.CC.LOCAL>		user@host.CC

# output local hosts in user%host@Berkeley syntax
R$+<@LOCAL>		$@$1<@$A>			local names
R$+<@$+.LOCAL>		$@$1%$2<@$A>			local hosts
R$+<@$*$=S>		$@$1%$2$3<@$A>
R$+<@$*$=Z>		$@$1%$2$3<@$A>

# handle other external cases
R$+<@$->		$@$1<@$2>			no .ARPA on simple names
R$+<@$+.$-.ARPA>	$@$1%$2<@$3.ARPA>		approximate something
R$+<@[$+]>		$@$1<@[$2]>			already ok

# convert remaining addresses to old format and externalize appropriately
R$+			$:$>5$1				=> old format
R$-:$+			$@$1.$2<@$A>			convert berk hosts
R$+<@$+>		$@$1%$2<@$A>			pessmize
R$+			$:$1%$w<@$A>			tack on our hostname
R$+%$=R<@$=A>		$1<@$3>				strip unneeded relay
