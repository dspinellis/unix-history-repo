###############################################################################
#    Instructions to Make, for compilation of ISODE processes for 
#                                                             Olivetti LSX 30xx
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/osx.make,v 7.8 91/02/22 09:16:46 mrose Interim $
#
#
# $Log:	osx.make,v $
# Revision 7.8  91/02/22  09:16:46  mrose
# Interim 6.8
# 
# Revision 7.0  89/11/23  21:26:14  mrose
# Release 6.0
# 
###############################################################################

###############################################################################
#
#				 NOTICE
#
#    Acquisition, use, and distribution of this module and related
#    materials are subject to the restrictions of a license agreement.
#    Consult the Preface in the User's Manual for the full terms of
#    this agreement.
#
###############################################################################


###############################################################################
# Options
###############################################################################

OPTIONS	=	-I. -I$(TOPDIR)h -I$(TOPDIR)hstubssap $(PEPYPATH) -DDEBUG $(KRBOPT)
PROOF	=	/usr2/proof/ISODE-6.7b/

HDIR    =       $(TOPDIR)h/
UTILDIR =       $(TOPDIR)util/
BINDIR  =       $(PROOF)bin/
SBINDIR =       $(PROOF)etc/
ETCDIR  =       $(PROOF)etc/
LOGDIR	=	/usr/tmp/
INCDIRM =       $(PROOF)src/include/isode
INCDIR  =       $(INCDIRM)/
LOCINC	=	$(TOPDIR)hstubssap/
PEPYDIRM=       $(INCDIR)pepy
PEPYDIR =       $(PEPYDIRM)/
PEPSYDIRM=	$(INCDIR)pepsy
PEPSYDIR=	$(PEPSYDIRM)/
LIBDIR  =       $(PROOF)src/lib/
LINTDIR =       $(PROOF)src/lib/

LIBISODE=	$(TOPDIR)libisode.a
LIBDSAP=	$(TOPDIR)libdsap.a
 
SYSTEM  =       -bsd42
MANOPTS =       -bsd42


###############################################################################
# Programs and Libraries
###############################################################################

MAKE    =       ./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL   =       /bin/sh

CC      =       cc
CFLAGS  =       -O    $(OPTIONS)
LIBCFLAGS=      $(CFLAGS)
LINT    =       lint
LFLAGS  =       -bhuz $(OPTIONS)
LD      =       ld
LDCC    =       $(CC)
LDFLAGS =       -s
ARFLAGS =

LN      =       ln

LSOCKET =	$(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;          $(CC) $(LIBCFLAGS) -c $*.c
		-ld -x -r $@
		mv a.out $@
