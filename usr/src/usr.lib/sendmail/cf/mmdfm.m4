############################################################
############################################################
#####
#####		MMDF Phonenet Channel Mailer specification
#####
#####		@(#)mmdfm.m4	3.4		2/24/83
#####
############################################################
############################################################

Mmmdf,	P=/usr/lib/mmdf/sendmmdf, F=sDFMu, S=16, R=16, A=sendmmdf $f $h $u

S16
R$+<@$-.LOCAL>		$@$1<@$2.$D.ARPA>		externalize local names
R$+<@$+>		$@$1<@$2>			already ok
R$+			$@$1<@Berk-Test>		tack on our hostname
