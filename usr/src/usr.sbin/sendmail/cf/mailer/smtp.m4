PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
POPDIVERT
#####################################
###   SMTP Mailer specification   ###
#####################################

VERSIONID(`@(#)smtp.m4	6.2 (Berkeley) %G%')

Msmtp,	P=[IPC], F=mDFMueXLC, S=11, R=21, A=IPC $h, E=\r\n

S11

# do sender/recipient common rewriting
R$+			$: $>19 $1

# if already @ qualified, we are done
R$* < @ $+ > $*		$@ $1 < @ $2 > $3		already qualified

# don't qualify list:; syntax
R$* :;			$@ $1 :;

# unqualified names (e.g., "eric") "come from" $M
R$=E			$: $1 < @ $j>			show exposed names
R$+			$: $1 < @ $M >			user w/o host
R$+ < @ >		$: $1 < @ $j >			in case $M undefined

S21

# do sender/recipient common rewriting
R$+			$: $>19 $1

# if already @ qualified, we are done
R$* < @ $+ > $*		$@ $1 < @ $2 > $3		already qualified

# don't qualify list:; syntax
R$* :;			$@ $1 :;

# unqualified names (e.g., "eric") are qualified by local host
R$+			$: $1 < @ $j >

S19

# pass <route-addr>s through
R< @ $+ > $*		$@ < @ $1 > $2			resolve <route-addr>

# output fake domains as user%fake@relay
ifdef(`BITNET_RELAY',
`R$+ <@ $+ . BITNET >	$: $1 % $2 .BITNET < @ $B >	user@host.BITNET',
	`dnl')
ifdef(`CSNET_RELAY',
`R$+ <@ $+ . CSNET >	$: $1 % $2 .CSNET < @ $C >	user@host.CSNET',
	`dnl')
R$+ <@ $+ . UUCP >	$: $2 ! $1 < @ $j >		user@host.UUCP
