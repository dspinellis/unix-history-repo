############################################################
############################################################
#####
#####		Smart UUCP Mailer specification
#####
#####	The other end must speak domain-based  addresses for
#####	this to work.  Someday this should become the "suucp"
#####	mailer, and we should be able to select by host name.
#####
#####		@(#)suucpm.m4	4.1		7/25/83
#####
############################################################
############################################################

Muucp,	P=/usr/bin/uux, F=sDFMhu, S=15, R=15, M=100000,
	A=uux - -r $h!rmail ($u)

S15
R$*<@$+>$*		$@$1<@$2>$3			accept usual domain name
R$+			$:$1<@$U.UUCP>			stick on our host name
