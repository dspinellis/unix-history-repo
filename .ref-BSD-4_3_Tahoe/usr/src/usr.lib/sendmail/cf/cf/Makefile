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


NSSRCS	=	cad.mc cadgroup.mc cogsci.mc okeeffe.mc ucbarpa.mc ucbvax.mc

GENSRCS	=	ucbproto.mc

NSALL	=	cad.cf cadgroup.cf cogsci.cf okeeffe.cf ucbarpa.cf ucbvax.cf

PROTOS	=	ucbtcp.cf ucbtcp_fw.cf

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
	m4 $*.mc > $*.cf
	rm -f $(BLDFILE)

all: $(ALL)

ucbtcp.cf: ucbproto.mc
	m4 ucbproto.mc > ucbtcp.cf

clean:
	rm -f $(ALL) a.out core make.out
	rm -f ,*

#
# Standard files included by all
#
$(ALL): ../m4/nsmacros.m4 ../m4/nsclasses.m4 ../m4/nicregistered.m4 \
	../m4/version.m4 ../m4/boilerplate.m4 ../m4/prewriterule.m4 \
	../m4/postwriterule.m4 ../m4/rule3.m4 ../m4/localm.m4 ../m4/nstcpm.m4 \
	../m4/nstcpldm.m4 ../m4/rule0.m4

#
# Special include files used only by specific hosts
#
cad.cf:		ucbproto.mc $(UUCP) ../machdep/uucp.cad.m4 \
		../machdep/smtpuucp.cad.m4
cadgroup.cf:	ucbproto.mc
cogsci.cf:	ucbproto.mc $(UUCP) ../machdep/uucp.cogsci.m4
okeeffe.cf:	ucbproto.mc $(UUCP) ../machdep/uucp.okeeffe.m4
ucbarpa.cf:	ucbproto.mc $(UUCP) ../machdep/uucp.ucbarpa.m4
ucbvax.cf:	$(UUCP) ../machdep/uucp.cad.m4 ../machdep/uucp.cogsci.m4 \
		../machdep/uucp.ucbarpa.m4 ../machdep/uucp.ucbvax.m4 \
		../machdep/smtpuucp.ucbvax.m4
