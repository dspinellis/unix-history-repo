############################################################
############################################################
#####
#####		UUCP Mailer specification
#####
#####		@(#)uucpm.m4	4.2		8/30/83
#####
############################################################
############################################################

ifdef(`m4COMPAT',, `include(compat.m4)')

Muucp,	P=/usr/bin/uux, F=sDFMhuU, S=13, R=23, M=100000,
	A=uux - -r $h!rmail ($u)

S13
R$+			$:$>5$1				convert to old style
R$=w!$+			$2				strip local name
R$*<@$=S>$*		$1<@$2.Berkeley.ARPA>$3		resolve abbreviations
R$*<@$=Z>$*		$1<@$2.Berkeley.ARPA>$3		resolve abbreviations
R$*<@$->$*		$1<@$2.ARPA>$3			resolve abbreviations
R$+			$:$U!$1				stick on our host name
R$=w!$=R:$+		$:$1!$3				ucbvax!ucbvax:xxx

S23
R$+			$:$>5$1				convert to old style
R$*<@$=S>$*		$1<@$2.Berkeley.ARPA>$3		resolve abbreviations
R$*<@$=Z>$*		$1<@$2.Berkeley.ARPA>$3		resolve abbreviations
R$*<@$->$*		$1<@$2.ARPA>$3			resolve abbreviations
