############################################################
############################################################
#####
#####		Arpanet TCP Mailer specification
#####
#####		%W%	%Y%	%G%
#####
############################################################
############################################################

Mtcp	[IPC]			msAueX	14  14	IPC $h

S14
R$+@$-.LOCAL		$@$2.$1@$D.ARPA			externalize local names
R$+@$-.$=D		$@$2.$1@$D.ARPA			externalize local names
R$+@$+.ARPA		$@$1@$2.ARPA			already ok
R$+@[$+]		$@$1@[$2]			already ok
R$+@$+			$@$1@$2.ARPA			tack on the domain
R$+			$@$H.$1@$A.ARPA			tack on our hostname
