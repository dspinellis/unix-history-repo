###############################################################################
#   Instructions to Make, for compilation of ISODE/SunLink osilookup program
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/osilookup/RCS/Makefile,v 7.2 91/02/22 09:28:45 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.2  91/02/22  09:28:45  mrose
# Interim 6.8
# 
# Revision 7.1  90/12/23  18:44:33  mrose
# update
# 
# Revision 7.0  89/11/23  22:00:57  mrose
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
# Generation Rules for program modules
###############################################################################

.c.o:;		$(CC) $(CFLAGS) -c $*.c


###############################################################################
# Programs and Libraries
###############################################################################

LIBES	=	
LLIBS   =	


###############################################################################
# Files
###############################################################################

HFILES	=	
CFILES	=	osilookup.c


##################################################################
# Here it is...
##################################################################

all:		osilookup
inst-all:	inst-osilookup
install:	inst-all clean
lint:		l-osilookup


##################################################################
# osilookup
##################################################################

inst-osilookup:	$(SBINDIR)osilookup

$(SBINDIR)osilookup:	xosilookup
		-cp $@ zxosilookup
		-rm -f $@
		cp xosilookup $@
		-@ls -gls $@
		-@echo ""

osilookup:	xosilookup

xosilookup:	osilookup.o
		$(LDCC) $(LDFLAGS) -o $@ osilookup.o $(LIBES) $(LSOCKET)

l-osilookup:;	$(LINT) $(LFLAGS) osilookup.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


##################################################################
# clean
##################################################################

clean:;		rm -f *.o x* z* _* core 

grind:;		iprint Makefile
		tgrind -lc $(HFILES) $(CFILES)

true:;
