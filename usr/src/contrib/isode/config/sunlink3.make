###############################################################################
#   Instructions to Make, for compilation of ISODE processes for
#	SunLink OSI 6.0 on SunOS 3
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/sunlink3.make,v 7.8 91/02/22 09:16:53 mrose Interim $
#
#
# $Log:	sunlink3.make,v $
# Revision 7.8  91/02/22  09:16:53  mrose
# Interim 6.8
# 
# Revision 7.7  91/01/14  13:36:31  mrose
# again
# 
# Revision 7.6  91/01/14  13:31:27  mrose
# kerberos
# 
# Revision 7.5  90/11/20  15:31:44  mrose
# update
# 
# Revision 7.4  90/07/27  08:42:16  mrose
# update
# 
# Revision 7.3  90/07/01  21:02:40  mrose
# pepsy
# 
# Revision 7.2  90/04/18  08:45:23  mrose
# MANDIR
# 
# Revision 7.1  90/03/06  13:56:12  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:26:20  mrose
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

OPTIONS	=	-I. -I$(TOPDIR)h -I/usr/sunlink/osi/sys $(PEPYPATH) $(KRBOPT)

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

CC      =	cc
# -O loses...
CFLAGS  =	      $(OPTIONS)
LIBCFLAGS=	      $(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhuz $(OPTIONS)
LD	=	ld
LDCC	=	$(CC)
LDFLAGS =	-s -L/usr/sunlink/osi/lib
ARFLAGS	=

LN	=	ln

LSOCKET	=	-losi $(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

# -x may be harmful on earlier releases of SunOS, your mileage may vary...

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
		-ld -x -r $@
		mv a.out $@
