PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
ifdef(`SMTP_MAILER_FLAGS',,
	`define(`SMTP_MAILER_FLAGS',
		`ifdef(`_OLD_SENDMAIL_', `L', `')')')
POPDIVERT
#####################################
###   SMTP Mailer specification   ###
#####################################

VERSIONID(`@(#)smtp.m4	8.14 (Berkeley) %G%')

Msmtp,		P=[IPC], F=CONCAT(mDFMuX, SMTP_MAILER_FLAGS), S=11/31, R=ifdef(`_ALL_MASQUERADE_', `11/31', `21'), E=\r\n,
		ifdef(`_OLD_SENDMAIL_',, `L=990, ')ifdef(`SMTP_MAILER_MAX', `M=SMTP_MAILER_MAX, ')A=IPC $h
Mesmtp,		P=[IPC], F=CONCAT(mDFMuXa, SMTP_MAILER_FLAGS), S=11/31, R=ifdef(`_ALL_MASQUERADE_', `11/31', `21'), E=\r\n,
		ifdef(`_OLD_SENDMAIL_',, `L=990, ')ifdef(`SMTP_MAILER_MAX', `M=SMTP_MAILER_MAX, ')A=IPC $h
Mrelay,		P=[IPC], F=CONCAT(mDFMuXa, SMTP_MAILER_FLAGS), S=11/31, R=61, E=\r\n,
		ifdef(`_OLD_SENDMAIL_',, `L=2040, ')A=IPC $h

#
#  envelope sender and masquerading recipient rewriting
#
S11
R$+			$: $>51 $1			sender/recipient common
R$* :; <@>		$@ $1 :;			list:; special case
R$*			$@ $>61 $1			qualify unqual'ed names


#
#  header recipient rewriting if not masquerading recipients
#
S21

# do sender/recipient common rewriting
R$+			$: $>51 $1

# unqualified names (e.g., "eric") are qualified by local host
R$* < @ $* > $*		$@ $1 < @ $2 > $3		already qualified
R$+			$: $1 < @ $j >			add local domain


#
#  header sender and masquerading recipient rewriting
#
S31
R$+			$: $>51 $1			sender/recipient common
R$* :; <@>		$@ $1 :;			list:; special case

# do special header rewriting
R$* <@> $*		$@ $1 <@> $2			pass null host through
R< @ $* > $*		$@ < @ $1 > $2			pass route-addr through
R$=E < @ $=w . >	$@ $1 < @ $2 >			exposed user as is
R$* < @ $=w . >		$: $1 < @ $M >			masquerade as domain
R$* < @ >		$: $1 < @ $j >			in case $M undefined
R$*			$@ $>61 $1			qualify unqual'ed names


#
#  convert pseudo-domain addresses to real domain addresses
#
S51

# pass <route-addr>s through
R< @ $+ > $*		$@ < @ $1 > $2			resolve <route-addr>

# output fake domains as user%fake@relay
ifdef(`BITNET_RELAY',
`R$+ <@ $+ .BITNET. >	$: $1 % $2 .BITNET < @ $B >	user@host.BITNET
R$+.BITNET <@ $+:$+ >	$: $1 .BITNET < @ $3 >		strip mailer: part',
	`dnl')
ifdef(`_NO_UUCP_', `dnl', `
# do UUCP heuristics; note that these are shared with UUCP mailers
R$+ < @ $+ .UUCP. >	$: < $2 ! > $1			convert to UUCP form
R$+ < @ $* > $*		$@ $1 < @ $2 > $3		not UUCP form

# leave these in .UUCP form to avoid further tampering
R< $&h ! > $- ! $+	$@ $2 < @ $1 .UUCP. >
R< $&h ! > $-.$+ ! $+	$@ $3 < @ $1.$2 >
R< $&h ! > $+		$@ $1 < @ $&h .UUCP. >
R< $+ ! > $+		$: $1 ! $2 < @ $Y >
R$+ < @ >		$: $1 < @ $j >			in case $Y undefined
R$+ < @ $+ : $+ >	$: $1 < @ $3 >			strip mailer: part')


#
#  common sender and masquerading recipient rewriting
#
S61

R$* < @ $* > $*		$@ $1 < @ $2 > $3		already qualified
R$=E			$@ $1 < @ $j>			show exposed names
R$+			$: $1 < @ $M >			user w/o host
R$+ <@>			$: $1 < @ $j >			in case $M undefined
