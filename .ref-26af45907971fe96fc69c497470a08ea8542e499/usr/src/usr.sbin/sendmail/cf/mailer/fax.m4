PUSHDIVERT(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
#	This assumes you already have Sam Leffler's FAX software.
#
# %sccs.include.redist.sh%
#

ifdef(`FAX_MAILER_PATH',,
	`define(`FAX_MAILER_PATH', /usr/local/lib/fax/mailfax)')
POPDIVERT
####################################
###   FAX Mailer specification   ###
####################################

VERSIONID(`@(#)fax.m4	6.2 (Berkeley) %G%')

Mfax,		P=FAX_MAILER_PATH, F=DFMhu, S=14, R=24, M=100000,
		A=mailfax $u $h $f
