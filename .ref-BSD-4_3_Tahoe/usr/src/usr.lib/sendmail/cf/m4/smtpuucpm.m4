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
#	%W%	(Berkeley) %G%
#
divert(0)
############################################################
############################################################
#####
#####		SMTP UUCP Mailer specification
#####
#####	This mailer sends UUCP traffic over an SMTP connection.
#####	Obviously, we only want to do this with UUCP hosts with
#####	whom we have SMTP connectivity.  The idea here is to
#####	avoid having to double queue (once for sendmail, once
#####	for UUCP) when there's no need.  Since we need to
#####	preserve uucp-ness (e.g., bangs), we use the UUCP mailer
#####	rewriting rulesets.
#####
############################################################
############################################################

Msmtpuucp,	P=[IPC], F=mDFMueXLC, S=13, R=23, A=IPC $h, E=\r\n

define(SMTPUUCPPAIR,
`R<@$1.uucp>:$+		$`#'smtpuucp$@$2$:`$'2
R$*<@$1.uucp>		$`#'smtpuucp$@$2$:`$'1')
