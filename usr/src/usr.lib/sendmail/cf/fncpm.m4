############################################################
############################################################
#####
#####		(Fake) Arpanet Mailer specification
#####
#####		@(#)fncpm.m4	4.1		7/25/83
#####
############################################################
############################################################

#Marpa,	P=/usr/lib/mailers/arpa, F=sDFMu, S=15, R=15, A=sendarpa $f $h $u
Marpa,	P=/usr/net/bin/sendberkmail, F=sDFMu, S=15, R=15,
	A=sendberkmail -m $G -h $c -t $u@$h

S15
R$+			$:$>5$1				convert to old form
R$+<@$+.CC>		$2.$1<@$A>			externalize comp ctr
R$+:$+			$1.$2				convert colon to dot
R$+<@$->		$@$1<@$2>			fine....
R$+			$@$1<@$A>			tack on global info
