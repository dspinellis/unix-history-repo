#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)Makefile.m4	5.1 (Berkeley) 6/7/85
#
#
#  Makefile for Sendmail library
#

ALL=	sendmail.hf
SRCS=	sendmail.hf

GET=	sccs get

all: $(ALL)

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

