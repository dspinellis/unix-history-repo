###############################################################################
#   Instructions to Make, for compilation of ISODE TCP/X.25 TP0 bridge
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/tp0bridge/RCS/Makefile,v 7.2 91/02/22 09:34:23 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.2  91/02/22  09:34:23  mrose
# Interim 6.8
# 
# Revision 7.1  90/12/23  18:45:00  mrose
# update
# 
# Revision 7.0  89/11/23  22:10:59  mrose
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

LIBES	=	$(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-lisode


###############################################################################
# Files
###############################################################################

HFILES	=	
CFILES	=	tp0bridge.c


##################################################################
# Here it is...
##################################################################

all:		tp0bridge
inst-all:	inst-tp0bridge manuals
install:	inst-all clean
lint:		l-tp0bridge


##################################################################
# tp0bridge
##################################################################

inst-tp0bridge:	$(SBINDIR)tp0bridge

$(SBINDIR)tp0bridge:	xtp0bridge
		-cp $@ zxtp0bridge
		-rm -f $@
		cp xtp0bridge $@
		-@ls -gls $@
		-@echo ""

tp0bridge:	xtp0bridge

xtp0bridge:	tp0bridge.o
		$(LDCC) $(LDFLAGS) -o $@ tp0bridge.o $(LIBES) $(LSOCKET)

l-tp0bridge:;	$(LINT) $(LFLAGS) tp0bridge.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# manual pages
################################################################

MANUALS	=	tp0bridge.8c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


##################################################################
# clean
##################################################################

clean:;		rm -f *.o x* z* _* core 

grind:;		iprint Makefile
		tgrind -lc $(HFILES) $(CFILES)

true:;
