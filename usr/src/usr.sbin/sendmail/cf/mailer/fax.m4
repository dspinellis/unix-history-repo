PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
#	This assumes you already have Sam Leffler's FAX software.
#
# %sccs.include.redist.sh%
#

ifdef(`FAX_MAILER_PATH',,
	`define(`FAX_MAILER_PATH', /usr/local/lib/fax/mailfax)')
ifdef(`FAX_MAILER_MAX',,
	`define(`FAX_MAILER_MAX', 100000)')
POPDIVERT
####################################
###   FAX Mailer specification   ###
####################################

VERSIONID(`@(#)fax.m4	8.2 (Berkeley) %G%')

Mfax,		P=FAX_MAILER_PATH, F=DFMhu, S=14, R=24, M=FAX_MAILER_MAX,
		A=mailfax $u $h $f
