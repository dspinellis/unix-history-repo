###############################################################################
#   Instructions to Make, for compilation of ISODE processes for HP-UX
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/hpux.make,v 7.9 91/02/22 09:16:38 mrose Interim $
#
#
# $Log:	hpux.make,v $
# Revision 7.9  91/02/22  09:16:38  mrose
# Interim 6.8
# 
# Revision 7.8  91/01/14  13:36:13  mrose
# again
# 
# Revision 7.7  91/01/14  13:31:13  mrose
# kerberos
# 
# Revision 7.6  90/11/20  15:31:32  mrose
# update
# 
# Revision 7.5  90/10/15  22:54:05  mrose
# typo
# 
# Revision 7.4  90/07/27  08:42:23  mrose
# update
# 
# Revision 7.3  90/07/01  21:02:32  mrose
# pepsy
# 
# Revision 7.2  90/04/18  08:45:11  mrose
# MANDIR
# 
# Revision 7.1  90/03/06  13:55:58  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:26:04  mrose
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

OPTIONS	=	-I. -I$(TOPDIR)h $(PEPYPATH) $(KRBOPT)

HDIR	=	$(TOPDIR)h/
UTILDIR	=	$(TOPDIR)util/
BINDIR	=	/usr/local/bin/
SBINDIR	=	/usr/etc/
ETCDIR	=	/usr/etc/
LOGDIR	=	/usr/tmp/
INCDIRM	=	/usr/include/isode
INCDIR	=	$(INCDIRM)/
PEPYDIRM=	$(INCDIR)pepy
PEPYDIR	=	$(PEPYDIRM)/
PEPSYDIRM=	$(INCDIR)pepsy
PEPSYDIR=	$(PEPSYDIRM)/
LIBDIR	=	/usr/lib/
LINTDIR	=	/usr/lib/lint/

LIBISODE=	$(TOPDIR)libisode.a
LIBDSAP	=	$(TOPDIR)libdsap.a

SYSTEM	=	-sys5
MANDIR	=	/usr/man/
MANOPTS	=	-hpux


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	cc
# may need +Ns2000 +Nd3000 to make compiler use larger tables on the Series 300
CFLAGS  =	-O    $(OPTIONS)
LIBCFLAGS=	      $(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhuz $(OPTIONS)
LD	=	ld
LDCC	=	$(CC)
LDFLAGS =	-ns
ARFLAGS	=

LN	=	ln

# TCP/IP is native to HP-UX, but we still need a non-standard library
LSOCKET	=	-lbsdipc $(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
