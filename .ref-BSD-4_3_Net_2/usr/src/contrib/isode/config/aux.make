###############################################################################
#   Instructions to Make, for compilation of ISODE processes for A/UX 1.1
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/aux.make,v 7.8 91/02/22 09:16:23 mrose Interim $
#
#
# $Log:	aux.make,v $
# Revision 7.8  91/02/22  09:16:23  mrose
# Interim 6.8
# 
# Revision 7.7  91/01/14  13:35:56  mrose
# again
# 
# Revision 7.6  91/01/14  13:30:55  mrose
# kerberos
# 
# Revision 7.5  90/11/20  15:31:17  mrose
# update
# 
# Revision 7.4  90/07/27  08:42:31  mrose
# update
# 
# Revision 7.3  90/07/01  21:02:24  mrose
# pepsy
# 
# Revision 7.2  90/04/18  08:44:58  mrose
# MANDIR
# 
# Revision 7.1  90/03/06  13:55:46  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:25:39  mrose
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
SBINDIR	=	//usr/local/etc/
ETCDIR	=	//usr/local/etc/
LOGDIR	=	/tmp/
INCDIRM	=	/usr/include/isode
INCDIR	=	$(INCDIRM)/
PEPYDIRM=	$(INCDIR)pepy
PEPYDIR	=	$(PEPYDIRM)/
PEPSYDIRM=	$(INCDIR)pepsy
PEPSYDIR=	$(PEPSYDIRM)/
LIBDIR	=	/usr/local/lib/
LINTDIR	=	/usr/lib/lint/

LIBISODE=	$(TOPDIR)libisode.a
LIBDSAP	=	$(TOPDIR)libdsap.a

SYSTEM	=	-sys5
MANDIR	=	/usr/man/
MANOPTS	=	-sys5


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	cc -B /usr/lib/big/
CFLAGS  =	-O    $(OPTIONS)
LIBCFLAGS=	      $(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhuz $(OPTIONS)
LD	=	ld
LDCC	=	$(CC)
LDFLAGS =	-s
ARFLAGS	=

LN	=	ln

LSOCKET	=	$(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

..c.o:;		$(CC) $(LIBCFLAGS) -c $*.c

