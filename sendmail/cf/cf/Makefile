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
#	@(#)Makefile	1.19 (Berkeley) 2/15/89
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

ALL	=	$(NSALL) $(PROTOS)

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
	rm -f $(BLDFILE)
	echo "# built by `whoami` on `date`" > $(BLDFILE)
	echo "# in `pwd` on `hostname`" >> $(BLDFILE)
	m4 ucbproto.mc > ucbtcp.cf
	rm -f $(BLDFILE)

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
cad.cf: ucbproto.mc ../m4/uucp.cad.m4
cadgroup.cf: ucbproto.mc
cogsci.cf: ucbproto.mc ../m4/uucp.cogsci.m4
okeeffe.cf: ucbproto.mc ../m4/uucp.okeeffe.m4
ucbarpa.cf: ucbproto.mc ../m4/uucp.ucbarpa.m4
ucbvax.cf: ../m4/rule5.m4 ../m4/berkm.m4 ../m4/suucpm.m4 ../m4/uucpm.m4  \
	../m4/uucp.cad.m4 ../m4/uucp.cogsci.m4 ../m4/uucp.ucbarpa.m4 \
	../m4/uucp.ucbvax.m4
