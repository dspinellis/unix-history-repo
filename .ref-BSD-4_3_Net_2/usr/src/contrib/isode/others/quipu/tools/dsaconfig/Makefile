###############################################################################
#   Instructions to Make, for compilation of ISODE QUIPU DSA configurator
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/tools/dsaconfig/RCS/Makefile,v 7.6 91/02/22 09:29:39 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.6  91/02/22  09:29:39  mrose
# Interim 6.8
# 
# Revision 7.5  91/02/12  22:54:07  mrose
# dsaptailor.root-dsas
# 
# Revision 7.4  90/12/23  23:42:29  mrose
# one-more-time
# 
# Revision 7.3  90/12/23  18:47:16  mrose
# update
# 
# Revision 7.2  90/08/29  15:08:55  mrose
# update
# 
# Revision 7.1  90/07/09  14:40:38  mrose
# sync
# 
# Revision 7.0  89/11/23  22:02:36  mrose
# Release 6.0
# 
###############################################################################

###############################################################################
#
#                                NOTICE
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

LIBES	=	$(TOPDIR)libisode.a  $(TOPDIR)quipu/quipuvrsn.o
LLIBS   =	$(TOPDIR)llib-lisode 


###############################################################################
# FILES
###############################################################################

CFILES  =       dsaconfig.c


##############################################################
# Here it is...
##############################################################

all:            dsaconfig
inst-all:       inst-pilot inst-dsaconfig manuals
install:        inst-all clean
lint:           l-dsaconfig


###################################################################
# pilot
###################################################################

inst-pilot:	$(ETCDIR)dsaptailor true
		cp -r quipu $(ETCDIR)

$(ETCDIR)dsaptailor: true
		if ln $(ETCDIR)dsaptailor $(ETCDIR)dsaptailor.old; \
		    then rm -f $@; cat dsaptailor $(TOPDIR)dsap/dsaptailor.root-dsas > $@; ls -gls $@; \
		    else exit 0; \
		fi


###################################################################
# dsaconfig
###################################################################

inst-dsaconfig:	$(SBINDIR)dsaconfig

$(SBINDIR)dsaconfig:	xdsaconfig
		-cp $@ zxdsaconfig
		-rm -f $@
		cp xdsaconfig $@
		-@ls -gls $@
		-@echo ""

dsaconfig:	xdsaconfig

xdsaconfig:	dsaconfig.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ dsaconfig.o $(LIBES) $(LSOCKET)

l-dsaconfig:;	$(LINT) $(LFLAGS) dsaconfig.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# manual pages
################################################################

MANUALS	=	dsaconfig.8

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


##############################################################
# clean
##############################################################

clean:;         rm -f *.ph *.o *.a a.out _* x* z* *.orig core

grind:;         iprint Makefile
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
