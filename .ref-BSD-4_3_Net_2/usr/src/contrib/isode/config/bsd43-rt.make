###############################################################################
#   Instructions to Make, for compilation of ISODE processes for RT PC running
#	4.3BSD UNIX
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/bsd43-rt.make,v 7.9 91/02/22 09:16:27 mrose Interim $
#
# Contributed by Jacob Rekhter, T.J. Watson Research Center, IBM Corp.
#
#
# $Log:	bsd43-rt.make,v $
# Revision 7.9  91/02/22  09:16:27  mrose
# Interim 6.8
# 
# Revision 7.8  91/01/14  13:36:00  mrose
# again
# 
# Revision 7.7  91/01/14  13:31:02  mrose
# kerberos
# 
# Revision 7.6  90/11/20  15:31:21  mrose
# update
# 
# Revision 7.5  90/07/27  08:42:28  mrose
# update
# 
# Revision 7.4  90/07/01  21:02:27  mrose
# pepsy
# 
# Revision 7.3  90/04/18  08:45:03  mrose
# MANDIR
# 
# Revision 7.2  90/03/06  13:55:50  mrose
# touch-up
# 
# Revision 7.1  90/01/27  10:26:02  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:25:57  mrose
# Release 6.0
# 
##############################################################################

##############################################################################
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

SYSTEM	=	-bsd42
MANDIR	=	/usr/man/
MANOPTS	=	-bsd42


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	hc
# don't use -O with hc 2.1s on RT/4.3
CFLAGS  =	      $(OPTIONS)
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

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
		-ld -x -r $@
		mv a.out $@
