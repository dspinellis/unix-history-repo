divert(10)
#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)postwriterule.m4	1.7 (Berkeley) 11/4/87
#
divert(0)
#################################
#  Final Output Post-rewriting  #
#################################
S4

R@			$@				handle <> error addr

# resolve numeric addresses to name if possible
R$*<@[$+]>$*		$:$1<@$[[$2]$]>$3		lookup numeric internet addr

# externalize local domain info
R$*<$+>$*		$1$2$3				defocus
R@$+:@$+:$+		@$1,@$2:$3			<route-addr> canonical

# UUCP must always be presented in old form (with domains, leave the ".uucp")
R$+@$-.UUCP		$2!$1				u@h.UUCP => h!u
R$+@$-.$-.UUCP		$2.$3.uucp!$1			u@h.UUCP => h!u
R$+@$-.$-.$-.UUCP	$2.$3.$4.uucp!$1		u@h.UUCP => h!u
R$+@$-.$-.$-.$-.UUCP	$2.$3.$4.$5.uucp!$1		u@h.UUCP => h!u
R$+@$-.$-.$-.$-.$-.UUCP	$2.$3.$4.$5.$6.uucp!$1		u@h.UUCP => h!u

# delete duplicate local names -- mostly for arpaproto.mc
R$+%$=w@$=w		$1@$w				u%UCB@UCB => u@UCB
R$+%$=w@$=w.$D		$1@$w				u%UCB@UCB => u@UCB

