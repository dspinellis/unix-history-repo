###############################################################################
#   Instructions to Make, for compilation of ISODE processes for 4.4BSD UNIX
###############################################################################

###############################################################################
#
# $Header: /f/osi/config/RCS/bsd44.make,v 7.10 91/02/22 09:16:31 mrose Interim $
#
#
# $Log:	bsd44.make,v $
# Revision 7.10  91/02/22  09:16:31  mrose
# Interim 6.8
# 
# Revision 7.9  91/01/14  13:36:06  mrose
# again
# 
# Revision 7.8  91/01/14  13:31:08  mrose
# kerberos
# 
# Revision 7.7  90/11/20  15:31:26  mrose
# update
# 
# Revision 7.6  90/07/27  08:42:26  mrose
# update
# 
# Revision 7.5  90/07/09  14:32:44  mrose
# sync
# 
# Revision 7.4  90/07/01  21:02:30  mrose
# pepsy
# 
# Revision 7.3  90/04/18  08:45:07  mrose
# MANDIR
# 
# Revision 7.2  90/03/06  13:55:54  mrose
# touch-up
# 
# Revision 7.1  90/01/11  18:35:20  mrose
# real-sync
# 
# Revision 7.0  89/11/23  21:26:01  mrose
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
BINDIR	=	/usr/local/isode/
SBINDIR	=	/usr/sbin/
ETCDIR	=	/etc/
LOGDIR	=	/var/log/
INCDIRM	=	/usr/include/isode
INCDIR	=	$(INCDIRM)/
PEPYDIRM=	$(INCDIR)pepy
PEPYDIR	=	$(PEPYDIRM)/
PEPSYDIRM=	$(INCDIR)pepsy
PEPSYDIR=	$(PEPSYDIRM)/
LIBDIR	=	/usr/lib/
LINTDIR	=	/usr/libdata/lint/

LIBISODE=	$(TOPDIR)libisode.a
LIBDSAP	=	$(TOPDIR)libdsap.a

SYSTEM	=	-bsd42
MANDIR	=	/usr/share/man/
MANOPTS	=	-bsd44


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
LDFLAGS =	-s
ARFLAGS	=

# since flex is now standard (ugh!)
LEX	=	/usr/old/bin/lex

LN	=	ln

# -lutil for vt (of all things)
LSOCKET	=	-lutil $(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
		-ld -x -r $@
		mv a.out $@
