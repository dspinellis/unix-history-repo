###############################################################################
#   Instructions to Make, for compilation of ISODE LPP processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/psap2-lpp/RCS/Makefile,v 7.7 91/02/22 09:37:56 mrose Interim $
#
# Contributed by The Wollongong Group, Inc.
#
#
# $Log:	Makefile,v $
# Revision 7.7  91/02/22  09:37:56  mrose
# Interim 6.8
# 
# Revision 7.6  90/12/23  18:44:05  mrose
# update
# 
# Revision 7.5  90/07/09  14:44:53  mrose
# sync
# 
# Revision 7.4  90/07/01  21:05:18  mrose
# pepsy
# 
# Revision 7.3  90/03/06  13:56:45  mrose
# touch-up
# 
# Revision 7.2  89/12/19  17:57:47  mrose
# touch-up
# 
# Revision 7.1  89/12/19  16:17:46  mrose
# dgram
# 
# Revision 7.0  89/11/23  22:15:45  mrose
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


PEPYPATH=	-DPEPYPATH

LLIBS   =	$(TOPDIR)llib-lpsap $(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)psap2.h $(HDIR)psap.h $(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h  $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		libpsap2 libisode-lpp
inst-all:	inst-libisode-lpp # inst-libpsap2 manuals
install:	inst-all clean
lint:		l-libpsap2


################################################################
# libpsap2-lpp
################################################################

CFILES	=	psaprovider.c $(TOPDIR)psap2/psap2error.c psaplose.c \
		psaprespond.c $(TOPDIR)tsap/tsaplisten.c psapinitiate.c \
		psapabort.c psaprelease1.c psaprelease2.c psapselect.c \
		ps2tcp.c ps2udp.c \
		$(TOPDIR)acsap/acsapstub.c \
		$(TOPDIR)tsap/tsaperror.c $(TOPDIR)tsap/tsaplose.c
PYFILES	=	ps.py
OFILES	=	psaprovider.o psap2error.o psaplose.o \
		psaprespond.o tsaplisten.o psapinitiate.o \
		psapabort.o psaprelease1.o psaprelease2.o psapselect.o \
		ps2tcp.o ps2udp.o \
		acsapstublpp.o \
		tsaperror.o tsaplose.o \
		$(OSTRINGS)

inst-libpsap2:	$(LIBDIR)libpsap2-lpp.a $(LINTDIR)llib-lpsap2-lpp

$(LIBDIR)libpsap2-lpp.a:	libpsap2-lpp.a
		-rm -f $@
		cp libpsap2-lpp.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lpsap2-lpp:	llib-lpsap2-lpp
		-cp $@ zllib-lpsap2-lpp
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lpsap2-lpp | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libpsap2:	libpsap2-lpp.a

libpsap2-lpp.a:	psap2vrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			PS_tables.o psap2vrsn.o
		-@rm -f $(TOPDIR)libpsap2-lpp.a
		-@$(LN) libpsap2-lpp.a $(TOPDIR)libpsap2-lpp.a
		-@ls -l $@
		-@echo "PSAP2 (lightweight) library built normally"

PS_tables.o:	PS_tables.c PS-types.h

PS_tables.c PS-types.h:	ps.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m ps.py


psap2vrsn.c:	$(OFILES) PS_tables.o
		@$(UTILDIR)version.sh psap2 '(lightweight)' > $@

l-libpsap2:	PS_tables.c true
		$(LINT) $(LFLAGS) -DLPP $(CFILES) PS_tables.c \
			psap2vrsn.c $(LLIBS) \
			| grep -v 'LPP redefined' \
			| grep -v "warning: possible pointer alignment problem"

psaprovider.o:	$(HDIR)ppkt.h PS-types.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
psap2error.o:	$(HFILES) $(TOPDIR)psap2/psap2error.c
		$(CC) $(CFLAGS) -c $(TOPDIR)psap2/psap2error.c
psaplose.o:	$(HDIR)ppkt.h PS-types.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
psaprespond.o:	$(HDIR)ppkt.h PS-types.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
tsaplisten.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h \
			$(HDIR)dgram.h $(HDIR)internet.h \
			$(TOPDIR)tsap/tsaplisten.c 
		$(CC) $(CFLAGS) -DLPP -c $(TOPDIR)tsap/tsaplisten.c
psapinitiate.o:	$(HDIR)ppkt.h PS-types.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
psapabort.o:	$(HDIR)ppkt.h PS-types.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
psaprelease1.o:	$(HDIR)ppkt.h PS-types.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
psaprelease2.o:	$(HDIR)ppkt.h PS-types.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
psapselect.o:	$(HDIR)ppkt.h $(HFILES)
ps2tcp.o:	$(HDIR)ppkt.h PS-types.h $(HFILES) $(HDIR)tsap.h \
		$(HDIR)tailor.h $(HDIR)logger.h
ps2udp.o:	$(HDIR)ppkt.h PS-types.h $(HFILES) $(HDIR)tsap.h \
		$(HDIR)tailor.h $(HDIR)logger.h $(HDIR)dgram.h
tsaperror.o:	$(HFILES) $(TOPDIR)tsap/tsaperror.c
		$(CC) $(CFLAGS) -c $(TOPDIR)tsap/tsaperror.c
tsaplose.o:	$(HFILES) $(TOPDIR)tsap/tsaplose.c
		$(CC) $(CFLAGS) -c $(TOPDIR)tsap/tsaplose.c

acsapstublpp.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h \
			$(TOPDIR)acsap/acsapstub.c
		$(CC) $(CFLAGS) -DLPP -c $(TOPDIR)acsap/acsapstub.c
		mv acsapstub.o acsapstublpp.o


################################################################
# libisode-lpp
################################################################

LIBES	=	$(TOPDIR)librosy.a $(TOPDIR)librosap.a $(TOPDIR)libacsap.a \
		$(TOPDIR)libpsap2-lpp.a \
		$(TOPDIR)libpepsy.a $(TOPDIR)libpepy.a $(TOPDIR)libpsap.a \
		$(TOPDIR)libdirent.a $(TOPDIR)libcompat.a
LLLIBS	=	$(TOPDIR)llib-lrosy $(TOPDIR)llib-lrosap $(TOPDIR)llib-lacsap \
		llib-lpsap2-lpp $(TOPDIR)llib-lpsap $(TOPDIR)llib-ldirent \
		$(TOPDIR)llib-lcompat


inst-libisode-lpp:	$(LIBDIR)libisode-lpp.a $(LINTDIR)llib-lisode-lpp

$(LIBDIR)libisode-lpp.a:	libisode-lpp.a
		-rm -f $@
		cp libisode-lpp.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lisode-lpp:	llib-lisode-lpp
		-cp $@ zllib-lisode-lpp
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lisode-lpp | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libisode-lpp:	libisode-lpp.a llib-lisode-lpp

libisode-lpp.a:	isodevrsn.o
		-rm -f $@ $(TOPDIR)libisode-lpp.a
		-rm -rf tmp
		-mkdir tmp
		ln isodevrsn.o tmp
		for i in $(LIBES); do (cd tmp; ar x ../$$i; rm -f acsapstub.o acsapdse.o; \
		    ../$(UTILDIR)make-lib.sh -quick $(SYSTEM) $(ARFLAGS) ../$@ *.o; \
		    rm -f *); done
		$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-rm -rf tmp
		-@rm -f $(TOPDIR)libisode-lpp.a
		-@$(LN) $@ $(TOPDIR)libisode-lpp.a
		-@ls -l $@
		-@echo "LPP library built normally"

llib-lisode-lpp:	$(LLLIBS)
		-@echo '/* llib-lisode-lpp - lint library for -lisode-lpp */' > $@
		-@echo '' >> $@
		cat $(LLLIBS) > $@
		-@rm -f $(TOPDIR)llib-lisode-lpp
		-@$(LN) llib-lisode-lpp $(TOPDIR)llib-lisode-lpp


isodevrsn.o:	$(LIBES)
		@cd $(TOPDIR)support; $(UTILDIR)version.sh isode > $(TOPDIR)psap2-lpp/isodevrsn.c
		$(CC) $(CFLAGS) -c isodevrsn.c


################################################################
# manual pages
################################################################

MANUALS	=	libpsap2-lpp.3n

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.ph *.o *.a PS* z* _* core psap2vrsn.c isodevrsn.c \
			llib-lisode-lpp

grind:;		iprint Makefile
		tgrind -lc $(CFILES) psap2vrsn.c
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(PYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
