#
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	1.20 (Berkeley) 3/3/89
#
######################################################################
#
#	Makefile for Sendmail UCB configuration files
#
######################################################################


HOSTS	=	cad.cf cogsci.cf ic.cf okeeffe.cf ucbarpa.cf ucbvax.cf

GROUPS	=	cadgroup.cf cc.cf cchem.cf

PROTOS	=	tcpproto.cf tcpuucpproto.cf uucpproto.cf ucbtcp.cf

ALL	=	$(HOSTS) $(GROUPS) $(PROTOS)

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

clean:
	rm -f $(ALL) a.out core make.out
	rm -f ,*

#
# Standard files included by all
#
$(ALL):	../m4/boilerplate.m4 ../m4/fake_domains.m4 ../m4/localm.m4 \
	../m4/nsclasses.m4 ../m4/nsmacros.m4 ../m4/nstcpldm.m4 \
	../m4/nstcpm.m4 ../m4/postwriterule.m4 ../m4/prewriterule.m4 \
	../m4/rule0.m4 ../m4/rule3.m4 ../m4/rule5.m4 ../m4/smtpuucpm.m4 \
	../m4/uucpm.m4 ../m4/version.m4 ../sitedep/nicregistered.m4 \
	proto.mc

#
# Special include files used only by specific hosts
#
cad.cf: ../sitedep/uucp.cad.m4
cogsci.cf: ../sitedep/uucp.cogsci.m4
okeeffe.cf: ../sitedep/uucp.okeeffe.m4
ucbarpa.cf: ../sitedep/uucp.ucbarpa.m4
ucbvax.cf: ../m4/suucpm.m4 ../sitedep/uucp.cad.m4 \
	../sitedep/uucp.cogsci.m4 ../sitedep/uucp.ucbarpa.m4 \
	../sitedep/uucp.ucbvax.m4

