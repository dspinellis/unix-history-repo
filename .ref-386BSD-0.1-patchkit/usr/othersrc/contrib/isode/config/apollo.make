###############################################################################
#   Instructions to Make, for compilation of ISODE processes for Apollo
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/apollo.make,v 7.9 91/02/22 09:16:20 mrose Interim $
#
# Contributed by John Brezak, Apollo Computer, Inc.
#
#
# $Log:	apollo.make,v $
# Revision 7.9  91/02/22  09:16:20  mrose
# Interim 6.8
# 
# Revision 7.8  91/01/14  13:35:53  mrose
# again
# 
# Revision 7.7  91/01/14  13:30:52  mrose
# kerberos
# 
# Revision 7.6  90/11/20  15:31:15  mrose
# update
# 
# Revision 7.5  90/10/15  18:17:54  mrose
# zap-AET
# 
# Revision 7.4  90/07/27  08:42:32  mrose
# update
# 
# Revision 7.3  90/07/01  21:02:23  mrose
# pepsy
# 
# Revision 7.2  90/04/18  08:44:56  mrose
# MANDIR
# 
# Revision 7.1  90/03/06  13:55:44  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:25:38  mrose
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

TOPDIR	=	/isode/
HDIR	=	$(TOPDIR)h/
UTILDIR	=	$(TOPDIR)util/
BINDIR	=	/isode/bin/
SBINDIR	=	/isode/etc/
ETCDIR	=	/isode/etc/
LOGDIR	=	/isode/logs/
MANDIR  =       /isode/man/
INCDIRM	=	/usr/include/isode
INCDIR	=	$(INCDIRM)/
PEPYDIRM=	$(INCDIR)pepy
PEPYDIR	=	$(PEPYDIRM)/
PEPSYDIRM=	$(INCDIR)pepsy
PEPSYDIR=	$(PEPSYDIRM)/
LIBDIR	=	/isode/lib/
LINTDIR	=	/isode/lib/lint/

LIBISODE=	$(TOPDIR)libisode.a
LIBDSAP	=	$(TOPDIR)libdsap.a

SYSTEM	=	-bsd42
MANDIR	=	/usr/man/
MANOPTS	=	-bsd42

OPTIONS	=	-I/isode/h -A systype,any -A cpu,3000 $(PEPYPATH) $(KRBOPT)


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	cc
CFLAGS	=	      $(OPTIONS)
LIBCFLAGS=	      $(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhuz $(OPTIONS)
LD	=	ld
LDCC	=	$(CC)
LDFLAGS =	#-s
ARFLAGS	=

LN	=	ln

LSOCKET	=	$(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
