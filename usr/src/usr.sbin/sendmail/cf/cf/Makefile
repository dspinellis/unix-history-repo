#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
######################################################################
#
#	Makefile for Sendmail UCB configuration files
#
######################################################################


NSSRCS	=	cad.mc cadgroup.mc cogsci.mc okeeffe.mc ucbarpa.mc ucbvax.mc \
		cc.mc cchem.mc ic.mc id.mc

GENSRCS	=	proto.mc

NSALL	=	cad.cf cadgroup.cf cogsci.cf okeeffe.cf ucbarpa.cf ucbvax.cf \
		cc.cf cchem.cf ic.cf id.cf

PROTOS	=	ucbtcp.cf uucpproto.cf tcpproto.cf tcpuucpproto.cf

EXP	=	x-ucbtcp_fw.cf

UUCP	=	../m4/uucpm.m4 ../m4/suucpm.m4 ../m4/rule5.m4 \
		../m4/smtpuucpm.m4

ALL	=	$(NSALL) $(PROTOS) $(EXP)

GET	=	sccs get

BLDFILE	=	buildinfo

.SUFFIXES: .mc .cf

.mc.cf:
	rm -f $(BLDFILE)
	echo "# built by `whoami` on `date`" > $(BLDFILE)
	echo "# in `pwd` on `hostname`" >> $(BLDFILE)
	m4 < $*.mc > $*.cf
	rm -f $(BLDFILE)

all: $(ALL)

clean:
	rm -f $(ALL) a.out core make.out
	rm -f ,*

#
# Standard files included by all
#
$(ALL): ../m4/nsmacros.m4 ../m4/nsclasses.m4 ../sitedep/nicregistered.m4 \
	../m4/version.m4 ../m4/boilerplate.m4 ../m4/prewriterule.m4 \
	../m4/postwriterule.m4 ../m4/rule3.m4 ../m4/localm.m4 ../m4/nstcpm.m4 \
	../m4/nstcpldm.m4 ../m4/rule0.m4 ../m4/fake_domains.m4

#
# Special include files used only by specific hosts
#
cad.cf:		proto.mc $(UUCP) ../sitedep/uucp.cad.m4 \
		../sitedep/smtpuucp.cad.m4
cadgroup.cf:	proto.mc
cc.cf:		proto.mc
cchem.cf:	proto.mc
ic.cf:		proto.mc $(UUCP) ../sitedep/uucp.ic.m4
id.cf:		proto.mc $(UUCP) ../sitedep/uucp.id.m4
cogsci.cf:	proto.mc $(UUCP) ../sitedep/uucp.cogsci.m4
okeeffe.cf:	proto.mc $(UUCP) ../sitedep/uucp.okeeffe.m4
ucbarpa.cf:	proto.mc $(UUCP) ../sitedep/uucp.ucbarpa.m4
ucbvax.cf:	$(UUCP) ../sitedep/uucp.cad.m4 ../sitedep/uucp.cogsci.m4 \
		../sitedep/uucp.ucbarpa.m4 ../sitedep/uucp.ucbvax.m4 \
		../sitedep/smtpuucp.ucbvax.m4
# prototypes
uucpproto.cf:	proto.mc $(UUCP) ../sitedep/uucp.proto.m4
tcpproto.cf:	proto.mc
