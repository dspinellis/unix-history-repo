###############################################################################
#   Instructions to Make, for compilation of ISODE RONOT library
###############################################################################

###############################################################################
#
# $Header: /f/osi/ronot/RCS/Makefile,v 
#
#
# $Log:	Makefile,v $
# Revision 7.4  91/02/22  09:50:24  mrose
# Interim 6.8
# 
# Revision 7.3  90/12/23  18:43:34  mrose
# update
# 
# Revision 7.2  90/10/29  18:38:30  mrose
# updates
# 
# Revision 7.1  90/07/27  09:06:30  mrose
# update
# 
# Revision 7.0  90/07/26  14:28:24  mrose
# *** empty log message ***
# 
# Revision 7.1  90/03/06  13:56:28  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:48:06  mrose
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


PEPYPATH=	-DPEPYPATH

LIBES   =       libronot.a
LLIBS   =       $(TOPDIR)llib-lrosap $(TOPDIR)llib-lacsap $(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)ronot.h $(HDIR)rosap.h $(HDIR)acsap.h \
		$(HDIR)psap2.h $(HDIR)psap.h $(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h


#############################################################
# Here it is...
#############################################################

all:            libronot
inst-all:	# inst-libronot
install:        inst-all clean
lint:           l-libronot


#############################################################
# libronot
#############################################################

PYFILES	=	ronot.py

CFILES  =       ronotbind1.c ronotbind2.c ronotunbind1.c \
		ronotunbind2.c ronotabort.c ronotlose.c RONOT_tables.c

OFILES  =       ronotbind1.o ronotbind2.o ronotunbind1.o \
		ronotunbind2.o ronotabort.o ronotlose.o RONOT_tables.o

inst-libronot:	$(LIBDIR)libronot.a $(LINTDIR)llib-lronot

$(LIBDIR)libronot.a:	libronot.a
		-rm -f $@
		cp libronot.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lronot:	llib-lronot
		-cp $@ zllib-lronot
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lronot | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libronot:	libronot.a

libronot.a:	ronotvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			ronotvrsn.o
		-@rm -f $(TOPDIR)libronot.a $(TOPDIR)llib-lronot
		-@$(LN) libronot.a $(TOPDIR)libronot.a
		-@$(LN) llib-lronot $(TOPDIR)llib-lronot
		-@ls -l $@
		-@echo "RO-notation library built normally"

ronotvrsn.c:	$(OFILES)
		@$(UTILDIR)version.sh ronot > $@

l-libronot:	true
		$(LINT) $(LFLAGS) $(CFILES) ronotvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


RONOT_tables.o:  RONOT_tables.c RONOT-types.h

RONOT_tables.c RONOT-types.h:	ronot.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m ronot.py


#############################################################
# clean
#############################################################

clean:;         rm -f  *.o *.a *.ph RONOT* z* _* core ronotvrsn.c

grind:;         iprint Makefile
		tgrind -lc $(CFILES) ronotvrsn.c llib-lronot
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(PYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;

# DO NOT DELETE THIS LINE
# Dependencies follow
ronotabort.o: $(HDIR)tailor.h $(HDIR)logger.h $(HFILES)
ronotbind1.o: $(HDIR)tailor.h $(HDIR)logger.h RONOT-types.h $(HFILES)
ronotbind2.o: $(HDIR)tailor.h $(HDIR)logger.h RONOT-types.h $(HFILES)
ronotlose.o: $(HDIR)tailor.h $(HDIR)logger.h $(HFILES)
ronotunbind1.o: $(HDIR)tailor.h $(HDIR)logger.h RONOT-types.h $(HFILES)
ronotunbind2.o: $(HDIR)tailor.h $(HDIR)logger.h RONOT-types.h $(HFILES)
