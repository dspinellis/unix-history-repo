PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

ifdef(`POP_MAILER_PATH',, `define(`POP_MAILER_PATH', /usr/lib/mh/spop)')
ifdef(`POP_MAILER_FLAGS',, `define(`POP_MAILER_FLAGS', `eu')')
ifdef(`POP_MAILER_ARGS',, `define(`POP_MAILER_ARGS', `pop $u')')

POPDIVERT

LOCAL_CONFIG
# POP mailer is a pseudo-domain
CPPOP
POPDIVERT

####################################
###   POP Mailer specification   ###
####################################

VERSIONID(`@(#)pop.m4	8.1 (Berkeley) %G%')

Mpop,		P=POP_MAILER_PATH, F=CONCAT(`lsDFM', POP_MAILER_FLAGS), S=10, R=20/40,
		A=POP_MAILER_ARGS
