###############################################################################
#   Instructions to Make, for compilation of ISODE processes for SunOS
#	release 4
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/sunos4.make,v 7.9 91/02/22 09:17:02 mrose Interim $
#
#
# $Log:	sunos4.make,v $
# Revision 7.9  91/02/22  09:17:02  mrose
# Interim 6.8
# 
# Revision 7.8  91/01/14  13:36:49  mrose
# again
# 
# Revision 7.7  91/01/14  13:31:36  mrose
# kerberos
# 
# Revision 7.6  90/12/23  17:23:41  mrose
# update
# 
# Revision 7.5  90/11/20  15:31:52  mrose
# update
# 
# Revision 7.4  90/07/27  08:42:09  mrose
# update
# 
# Revision 7.3  90/07/01  21:02:43  mrose
# pepsy
# 
# Revision 7.2  90/04/18  08:45:29  mrose
# MANDIR
# 
# Revision 7.1  90/03/06  13:56:16  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:26:25  mrose
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

SYSTEM	=	-bsd42
MANDIR	=	/usr/man/
MANOPTS	=	-bsd42


###############################################################################
# Shared libraries
###############################################################################

#    You can build a shared version of the ISODE library on suns under
#    SunOS 4.0 or greater. For non sparc based architecture, any
#    release greater than 4.0 will do. However, due to a bug in the
#    compiler and the size of ISODE, you will not be able to build a
#    shared ISODE unless you have SunOS 4.1 or greater.

#    First, comment out the definitions of LIBISODE and LIBDSAP above

#    Second, uncomment these three lines:

#SHAREDLIB=	shared
#LIBISODE=	-L$(TOPDIR) -lisode
#LIBDSAP=	-L$(TOPDIR) -ldsap

#    Third, add
#		-pic		Sun 3
#		-PIC		Sparc
#    to LIBCFLAGS below


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	cc
# -O loses...
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
