#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) 2/14/86
#
######################################################################
#
#	Makefile for Sendmail UCB configuration files
#
######################################################################


CSSRCS=	csbase.m4 \
	arpa.mc \
	calder.mc \
	cogsci.mc \
	dali.mc \
	degas.mc \
	ernie.mc \
	ji.mc \
	kim.mc \
	matisse.mc \
	miro.mc \
	monet.mc \
	okeeffe.mc \
	renoir.mc \
	ucbvax.mc
CCSRCS=	ccbase.m4 \
GENSRCS=arpaproto.mc \
	base.m4 \
	berkhosts.m4 \
	berkm.m4 \
	cchosts.m4 \
	compat.m4 \
	ether.m4 \
	etherm.m4 \
	fudge.m4 \
	ibmm.m4 \
	lanleaf.m4 \
	lanroot.m4 \
	localm.m4 \
	mmdfm.m4 \
	ncpm.m4 \
	suucpm.m4 \
	tcpm.m4 \
	uucpm.m4 \
	uucpproto.mc \
	version.m4 \
	zerobase.m4
SRCS=	$(GENSRCS) $(CSSRCS) $(CCSRCS)

CSNALL=	arpa.cf \
	calder.cf \
	cogsci.cf \
	dali.cf \
	degas.cf \
	ernie.cf \
	ji.cf \
	kim.cf \
	matisse.cf \
	miro.cf \
	monet.cf \
	okeeffe.cf \
	renoir.cf \
	ucbvax.cf
CSALL=	$(CSNALL)
PROTOS=	arpaproto.cf uucpproto.cf
ALL=	$(CSALL) $(PROTOS)

GET=	sccs get

.SUFFIXES: .mc .cf

.mc.cf:
	m4 $*.mc > $*.cf

all: $(ALL)

cs: $(CSALL)
############################
#   special dependencies   #
############################

# computer science department hosts....
$(CSALL): csbase.m4

# pessimize -- basic dependencies
$(ALL): ether.m4 etherm.m4 base.m4 localm.m4 tcpm.m4 version.m4 zerobase.m4

# hosts using UUCP
ucbvax.cf uucpproto.cf: uucpm.m4 compat.m4 berkhosts.m4
arpa.cf: suucpm.m4

# hosts using MMDF
arpa.cf: mmdfm.m4

# hosts knowing about berknet
ucbvax.cf: cchosts.m4 berkm.m4 compat.m4 berkhosts.m4

#####################
#   administrivia   #
#####################

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

clean:
	rm -f $(ALL) a.out core make.out
	rm -f ,*
