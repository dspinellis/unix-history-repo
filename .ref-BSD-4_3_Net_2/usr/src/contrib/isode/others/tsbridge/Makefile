###############################################################################
#   Instructions to Make, for compilation of ISODE Transport Service Bridge
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/tsbridge/RCS/Makefile,v 7.2 91/02/22 09:34:32 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.2  91/02/22  09:34:32  mrose
# Interim 6.8
# 
# Revision 7.1  90/12/23  18:45:05  mrose
# update
# 
# Revision 7.0  89/11/23  22:11:09  mrose
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
CFILES	=	tsbridge.c


##################################################################
# Here it is...
##################################################################

all:		tsbridge
inst-all:	inst-tsbridge manuals
install:	inst-all clean
lint:		l-tsbridge


##################################################################
# tsbridge
##################################################################

inst-tsbridge:	$(SBINDIR)tsbridge

$(SBINDIR)tsbridge:	xtsbridge
		-cp $@ zxtsbridge
		-rm -f $@
		cp xtsbridge $@
		-@ls -gls $@
		-@echo ""

tsbridge:	xtsbridge

xtsbridge:	tsbridge.o
		$(LDCC) $(LDFLAGS) -o $@ tsbridge.o $(LIBES) $(LSOCKET)

l-tsbridge:;	$(LINT) $(LFLAGS) tsbridge.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# manual pages
################################################################

MANUALS	=	tsbridge.8c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


##################################################################
# clean
##################################################################

clean:;		rm -f *.o x* z* _* core 

grind:;		iprint Makefile
		tgrind -lc $(HFILES) $(CFILES)

true:;
