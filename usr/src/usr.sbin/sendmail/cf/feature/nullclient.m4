PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
ifdef(`SMTP_MAILER_FLAGS',, `define(`SMTP_MAILER_FLAGS', `')')
define(_NULL_CLIENT_ONLY_, `1')
ifelse(_ARG_, `', `errprint(`Feature "nullclient" requires argument')',
	`define(`MAIL_HUB', _ARG_)')
POPDIVERT

#
#  This is used only for relaying mail from a client to a hub when
#  that client does absolutely nothing else -- i.e., it is a "null
#  mailer".  In this sense, it acts like the "R" option in Sun
#  sendmail.
#

VERSIONID(`@(#)nullclient.m4	8.5 (Berkeley) %G%')

PUSHDIVERT(6)
# hub host (to which all mail is sent)
DH`'ifdef(`MAIL_HUB', MAIL_HUB,
	`errprint(`MAIL_HUB not defined for nullclient feature')')

# route-addr separators
C: : ,
POPDIVERT
PUSHDIVERT(7)
############################################
###   Null Client Mailer specification   ###
############################################

ifdef(`confRELAY_MAILER',,
	`define(`confRELAY_MAILER', `nullclient')')dnl
ifdef(`confFROM_HEADER',,
	`define(`confFROM_HEADER', <$g>)')dnl

Mnullclient,	P=[IPC], F=CONCAT(mDFMuXa, SMTP_MAILER_FLAGS),ifdef(`SMTP_MAILER_MAX', ` M=SMTP_MAILER_MAX,')
		A=IPC $h
POPDIVERT
