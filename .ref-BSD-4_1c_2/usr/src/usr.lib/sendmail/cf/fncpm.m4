############################################################
############################################################
#####
#####		(Fake) Arpanet Mailer specification
#####
#####		@(#)fncpm.m4	3.3		2/3/83
#####
############################################################
############################################################

#Marpa,	P=/usr/lib/mailers/arpa, F=sDFMu, S=15, R=15, A=sendarpa $f $h $u
Marpa,	P=/usr/net/bin/sendberkmail, F=sDFMu, S=15, R=15,
	A=sendberkmail -m $G -h $c -t $u@$h

S15
R$+			$:$>5$1				convert to old form*
#R$+@$+.ARPA		$1@$2				convert to old form*
#R$+@$-.LOCAL		$2.$1@$A			externalize local hosts
#R$+@$+.$=D		$2.$1@$A			convert local domain*
#R$+@$=S		$2.$1@$A			stick our domain on*
#R$+@$=Z		$2.$1@$A			stick our domain on*
R$+@$+.CC		$2.$1@$A			externalize comp ctr*
R$+:$+			$1.$2				convert colon to dot*
R$+@$-			$@$1@$2				fine....*
R$+			$@$1@$A				tack on global info*
