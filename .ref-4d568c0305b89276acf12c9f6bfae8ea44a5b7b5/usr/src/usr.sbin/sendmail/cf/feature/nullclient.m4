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

VERSIONID(`@(#)nullclient.m4	8.2 (Berkeley) %G%')

PUSHDIVERT(7)
############################################
###   Null Client Mailer specification   ###
############################################

ifdef(`confRELAY_MAILER',,
	`define(`confRELAY_MAILER', `nullclient')')dnl

Mnullclient,	P=[IPC], F=CONCAT(mDFMuXa, SMTP_MAILER_FLAGS), A=IPC $h
POPDIVERT
