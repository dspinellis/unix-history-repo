###############################################################################
#   Instructions to Make, for compilation of the ISODE callback demo
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/callback/RCS/Makefile,v 7.2 91/02/22 09:26:35 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.2  91/02/22  09:26:35  mrose
# Interim 6.8
# 
# Revision 7.1  90/12/23  18:44:12  mrose
# update
# 
# Revision 7.0  89/11/23  21:58:09  mrose
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

BINDIR	=	/usr/new/


###############################################################################
# Generation Rules for program modules
###############################################################################

.c.o:;		$(CC) $(CFLAGS) -c $*.c


###############################################################################
# Programs and Libraries
###############################################################################

LIBES	=	$(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-lisode


###############################################################################
# Files
###############################################################################

HFILES	=	callback.h
CFILES	=	initiator.c responder.c report.c


##################################################################
# Here it is...
##################################################################

all:		initiator responder
inst-all:	inst-initiator inst-responder
install:	inst-all clean
lint:		l-initiator l-responder


##################################################################
# initiator
##################################################################

inst-initiator:	$(BINDIR)callback

$(BINDIR)callback:	xinitiator
		-cp $@ zcallback
		-rm -f $@
		cp xinitiator $@
		-@ls -gls $@
		-@echo ""

initiator:	xinitiator

xinitiator:	initiator.o report.o
		$(LDCC) $(LDFLAGS) -o $@ initiator.o report.o $(LIBES) \
			$(LSOCKET)

l-initiator:;	$(LINT) $(LFLAGS) initiator.c report.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

initiator.o:	$(HFILES)


##################################################################
# responder
##################################################################

inst-responder:	$(SBINDIR)iso.callback

$(SBINDIR)iso.callback:	xresponder
		-cp $@ ziso.callback
		-rm -f $@
		cp xresponder $@
		-@ls -gls $@
		-@echo ""

responder:	xresponder

xresponder:	responder.o report.o
		$(LDCC) $(LDFLAGS)  -o $@ responder.o report.o $(LIBES) \
			$(LSOCKET)

l-responder:;	$(LINT) $(LFLAGS) responder.c report.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

responder.o:	$(HFILES)


##################################################################
# clean
##################################################################

clean:;		rm -f *.o x* z* _* core 

grind:;		iprint Makefile
		tgrind -lc $(HFILES) $(CFILES)

true:;
