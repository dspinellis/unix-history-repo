###############################################################################
#   Instructions to Make, for compilation of ISODE processes for ROS
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/ros.make,v 7.8 91/02/22 09:16:48 mrose Interim $
#
#
# $Log:	ros.make,v $
# Revision 7.8  91/02/22  09:16:48  mrose
# Interim 6.8
# 
# Revision 7.7  91/01/14  13:36:25  mrose
# again
# 
# Revision 7.6  91/01/14  13:31:22  mrose
# kerberos
# 
# Revision 7.5  90/11/20  15:31:40  mrose
# update
# 
# Revision 7.4  90/07/27  08:42:18  mrose
# update
# 
# Revision 7.3  90/07/01  21:02:37  mrose
# pepsy
# 
# Revision 7.2  90/04/18  08:45:19  mrose
# MANDIR
# 
# Revision 7.1  90/03/06  13:56:08  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:26:16  mrose
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

OPTIONS	=	-I. -I$(TOPDIR)h -I/usr/4.2/include $(PEPYPATH) $(KRBOPT)

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
LINTDIR	=	/usr/4.2/lib/lint/

LIBISODE=	$(TOPDIR)libisode.a
LIBDSAP	=	$(TOPDIR)libdsap.a

SYSTEM	=	-ros
MANDIR	=	/usr/man/
MANOPTS	=	-ros


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	rc
CFLAGS  =	      $(OPTIONS)
LIBCFLAGS=	      $(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhu  $(OPTIONS)
LD	=	ld
LDCC	=	$(LD)
LDFLAGS =	-V 5 -s /usr/4.2/lib/crt0.o
ARFLAGS	=

LN	=	ln

# get 4.2BSD libc
LSOCKET	=	/usr/4.2/lib/libc.a $(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
