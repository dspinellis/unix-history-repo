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
############################################
###   Null Client Mailer specification   ###
############################################

#
#  This is used only for relaying mail from a client to a hub when
#  that client does absolutely nothing else -- i.e., it is a "null
#  mailer".  In this sense, it acts like the "R" option in Sun
#  sendmail.
#

VERSIONID(`@(#)nullclient.m4	8.1 (Berkeley) %G%')

ifdef(`confRELAY_MAILER',,
	`define(`confRELAY_MAILER', `nullclient')')dnl

Mnullclient,	P=[IPC], F=CONCAT(mDFMuXa, SMTP_MAILER_FLAGS), A=IPC $h
