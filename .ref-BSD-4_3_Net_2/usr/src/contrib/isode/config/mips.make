###############################################################################
#   Instructions to Make, for compilation of ISODE processes for RISC/OS
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/mips.make,v 7.9 91/02/22 09:16:41 mrose Interim $
#
#
# $Log:	mips.make,v $
# Revision 7.9  91/02/22  09:16:41  mrose
# Interim 6.8
# 
# Revision 7.8  91/01/14  13:36:16  mrose
# again
# 
# Revision 7.7  91/01/14  13:31:15  mrose
# kerberos
# 
# Revision 7.6  90/11/20  15:31:34  mrose
# update
# 
# Revision 7.5  90/07/27  08:42:22  mrose
# update
# 
# Revision 7.4  90/07/01  21:02:34  mrose
# pepsy
# 
# Revision 7.3  90/04/18  08:45:13  mrose
# MANDIR
# 
# Revision 7.2  90/03/26  09:57:06  mrose
# MIPS
# 
# Revision 7.1  90/03/06  13:56:00  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:26:07  mrose
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
BINDIR	=	/usr/local/bin/isode/
SBINDIR	=	/usr/etc/isode/sbin/
ETCDIR	=	/usr/etc/isode/etc/
LOGDIR	=	/usr/spool/isode/
INCDIRM	=	/usr/include/isode
INCDIR	=	$(INCDIRM)/
PEPYDIRM=	$(INCDIR)pepy
PEPYDIR	=	$(PEPYDIRM)/
PEPSYDIRM=	$(INCDIR)pepsy
PEPSYDIR=	$(PEPSYDIRM)/
LIBDIR	=	/usr/lib/
### LINTDIR IS WRONG ###
LINTDIR	=	/usr/lib/lint/

LIBISODE=	$(TOPDIR)libisode.a
LIBDSAP	=	$(TOPDIR)libdsap.a

SYSTEM	=	-mips
MANDIR	=	/usr/man/
MANOPTS	=	-sys5


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	cc
CFLAGS  =	-O    $(OPTIONS)
LIBCFLAGS=	      $(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhuz $(OPTIONS)
LD	=	ld
LDCC	=	$(CC)
LDFLAGS =	
ARFLAGS	=

LN	=	ln

LSOCKET	=	$(KRBLIB)
LNLIST	=	-lmld


###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
		-ld -x -r $@
		mv a.out $@
