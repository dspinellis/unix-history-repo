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
#	@(#)localm.m4	1.1 (Berkeley) 8/8/85
#
divert(0)
############################################################
############################################################
#####
#####		Local and Program Mailer specification
#####
############################################################
############################################################

Mlocal,	P=/usr/libexec/mail.local, F=lsDFMmn, S=10, R=20, A=mail -r $g -d $u
Mprog,	P=/bin/sh,   F=lsDFMe,   S=10, R=20, A=sh -c $u

S10
R@			$n			errors to mailer-daemon
