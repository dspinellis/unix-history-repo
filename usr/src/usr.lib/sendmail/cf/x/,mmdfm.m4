############################################################
############################################################
#####
#####		MMDF Phonenet Channel Mailer specification
#####
#####		%W%	%Y%	%G%
#####
############################################################
############################################################

Mmmdf	/usr/lib/mmdf/sendmmdf	sAu	16  16	sendmmdf $f $h $u

S16
R$+@$-.LOCAL		$@$1@$2.$D.ARPA			externalize local names
R$+@$+			$@$1@$2				already ok
R$+			$@$1@Berk-Test			tack on our hostname
