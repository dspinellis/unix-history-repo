#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.4 (Berkeley) 3/12/86
#
######################################################################
#
#	Makefile for Sendmail UCB configuration files
#
######################################################################


NSSRCS=	arpa.mc \
	kim.mc \
	monet.mc \
	ucbvax.mc
GENSRCS=ucbtcp.mc

NSALL=	arpa.cf \
	kim.cf \
	monet.cf \
	ucbvax.cf
PROTOS=	ucbtcp.cf
ALL=	$(NSALL) $(PROTOS)

GET=	sccs get

.SUFFIXES: .mc .cf

.mc.cf:
	m4 $*.mc > $*.cf

all: $(ALL)

arpa.cf: arpa.mc
	m4 $*.mc > $*.cf
	sh arpa.ed


clean:
	rm -f $(ALL) a.out core make.out
	rm -f ,*

#
# Standard files included by all
#
$(ALL): nsmacros.m4 nsclasses.m4 cchosts.m4 nicregistered.m4 berkhosts.m4 \
	version.m4 boilerplate.m4 prewriterule.m4 postwriterule.m4 rule3.m4 \
	localm.m4 nstcpm.m4 nstcpldm.m4 rule0.m4

#
# Special include files used only by specific hosts
#
arpa.cf: mmdfm.m4 euucpm.m4

kim.cf: uucpm.m4

ucbvax.cf: uucphosts.m4 rule5.m4 berkm.m4 suucpm.m4 uucpm.m4 
