PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

ifdef(`PROCMAIL_PATH',,
	`define(`PROCMAIL_PATH', /usr/local/bin/procmail)')
ifdef(`PROCMAIL_MAILER_FLAGS',,
	`define(`PROCMAIL_MAILER_FLAGS', `Shu')')
ifdef(`PROCMAIL_MAILER_ARGS',,
	`define(`PROCMAIL_MAILER_ARGS', `procmail -m $h $f $u')')

POPDIVERT

######################*****##############
###   PROCMAIL Mailer specification   ###
##################*****##################

VERSIONID(`@(#)procmail.m4	8.1 (Berkeley) %G%')

Mprocmail,	P=PROCMAIL_PATH, F=CONCAT(`DFMmn', PROCMAIL_MAILER_FLAGS), S=11/31, R=21/31,
		A=PROCMAIL_MAILER_ARGS
