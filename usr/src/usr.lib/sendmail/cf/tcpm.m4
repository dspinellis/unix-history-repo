############################################################
############################################################
#####
#####		Arpanet TCP Mailer specification
#####
#####		@(#)tcpm.m4	3.11		2/24/83
#####
############################################################
############################################################

Mtcp,	P=[IPC], F=msDFMueXL, S=14, R=14, A=IPC $h, E=\r\n

S14

# pass <route-addr>'s through unchanged
R<@$+>$*		$@<@$1>$2			<route-addr> syntax

# convert the address to old format
R$+			$:$>5$1				=> old format

# handle the simple case....
R$+<@$-.ARPA>		$@$1<@$2.ARPA>			user@host.ARPA

# output local hosts in host.user@Berkeley syntax (ugh!)
R$+<@$*LOCAL>		$@$2$1<@$A>			local names
R$+<@$*$=S>		$@$2$3.$1<@$A>			local names
R$+<@$*$=Z>		$@$2$3.$1<@$A>			local names

R$+<@$->		$@$1<@$2>			no .ARPA on simple names
R$+<@$+.$-.ARPA>	$@$1%$2<@$3.ARPA>		approximate something
R$+<@[$+]>		$@$1<@[$2]>			already ok
R$+<@$+>		$@$1<@$2>			hmmmmmmmmm......
R$-:$+			$@$1.$2<@$A>			convert berk hosts
R$+			$:$w.$1<@$A>			tack on our hostname
R$=R.$+<@$=A>		$2<@$3>				strip unneeded relay
