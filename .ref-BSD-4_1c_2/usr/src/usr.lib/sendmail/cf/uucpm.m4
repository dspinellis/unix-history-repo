############################################################
############################################################
#####
#####		UUCP Mailer specification
#####
#####		@(#)uucpm.m4	3.5		2/24/83
#####
############################################################
############################################################

ifdef(`m4COMPAT',, `include(compat.m4)')

Muucp,	P=/usr/bin/uux, F=sDFMhuU, S=13, R=23, M=100000,
	A=uux - -r $h!rmail ($u)

S13
R$+			$:$>5$1				convert to old style
R$=U!$+			$2				strip local name
R$+			$:$U!$1				stick on our host name
R$=U!$=R:$+		$:$1!$3				ucbvax!ucbvax:xxx

S23
R$+			$:$>5$1				convert to old style
