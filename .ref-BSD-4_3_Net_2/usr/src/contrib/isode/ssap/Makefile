###############################################################################
#   Instructions to Make, for compilation of ISODE SSAP processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/ssap/RCS/Makefile,v 7.3 91/02/22 09:45:31 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.3  91/02/22  09:45:31  mrose
# Interim 6.8
# 
# Revision 7.2  90/12/23  18:43:14  mrose
# update
# 
# Revision 7.1  90/07/09  14:50:07  mrose
# sync
# 
# Revision 7.0  89/11/23  22:25:16  mrose
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


LIBES	=	libssap.a $(TOPDIR)libcompat.a
LLIBS	=	$(TOPDIR)llib-ltsap $(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)spkt.h $(HDIR)ssap.h $(HDIR)tsap.h $(HDIR)isoaddrs.h \
		$(HDIR)general.h $(HDIR)manifest.h  $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		libssap
inst-all:	# inst-libssap manuals
install:	inst-all clean
lint:		l-libssap


################################################################
# libssap
################################################################

CFILES	=	ssaprovider.c ssaperror.c \
		str2spkt.c text2spkt.c tsdu2spkt.c \
		ssapexec.c ssaprespond.c ssapinitiate.c ssapexpd.c \
		ssaptyped.c ssapcapd1.c ssapcapd2.c ssaptoken.c \
		ssapactivity.c ssapmajor1.c ssapmajor2.c ssapminor1.c \
		ssapminor2.c ssapresync1.c ssapresync2.c ssapabort.c \
		ssapreport.c ssaprelease1.c ssaprelease2.c ssapwrite.c \
		ssapactchk.c ssapselect.c ssaplose.c
OFILES	=	ssaprovider.o ssaperror.o \
		str2spkt.o text2spkt.o tsdu2spkt.o \
		ssapexec.o ssaprespond.o ssapinitiate.o ssapexpd.o \
		ssaptyped.o ssapcapd1.o ssapcapd2.o ssaptoken.o \
		ssapactivity.o ssapmajor1.o ssapmajor2.o ssapminor1.o \
		ssapminor2.o ssapresync1.o ssapresync2.o ssapabort.o \
		ssapreport.o ssaprelease1.o ssaprelease2.o ssapwrite.o \
		ssapactchk.o ssapselect.o ssaplose.o \
		$(OSTRINGS)

inst-libssap:	$(LIBDIR)libssap.a $(LINTDIR)llib-lssap

$(LIBDIR)libssap.a:	libssap.a
		-rm -f $@
		cp libssap.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lssap:	llib-lssap
		-cp $@ zllib-lssap
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lssap | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libssap:	libssap.a

libssap.a:	ssapvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			ssapvrsn.o
		-@rm -f $(TOPDIR)libssap.a $(TOPDIR)llib-lssap
		-@$(LN) libssap.a $(TOPDIR)libssap.a
		-@$(LN) llib-lssap $(TOPDIR)llib-lssap
		-@ls -l $@
		-@echo "SSAP library built normally"

ssapvrsn.c:	$(OFILES)
		@$(UTILDIR)version.sh ssap > $@

l-libssap:;	$(LINT) $(LFLAGS) $(CFILES) ssapvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

ssaprovider.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
ssaperror.o:	$(HFILES)
str2spkt.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
text2spkt.o:	$(HFILES) $(HDIR)logger.h
tsdu2spkt.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
ssapexec.o:	$(HFILES) $(HDIR)isoservent.h $(HDIR)tailor.h $(HDIR)logger.h
ssaprespond.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
ssapinitiate.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
ssapexpd.o:	$(HFILES)
ssaptyped.o:	$(HFILES)
ssapcapd1.o:	$(HFILES)
ssapcapd2.o:	$(HFILES)
ssaptoken.o:	$(HFILES)
ssapactivity.o:	$(HFILES)
ssapabort.o:	$(HFILES)
ssapreport.o:	$(HFILES)
ssapmajor1.o:	$(HFILES)
ssapmajor2.o:	$(HFILES)
ssapminor1.o:	$(HFILES)
ssapminor2.o:	$(HFILES)
ssapresync1.o:	$(HFILES)
ssapresync2.o:	$(HFILES)
ssaprelease1.o:	$(HFILES)
ssaprelease2.o:	$(HFILES)
ssapwrite.o:	$(HFILES)
ssapactchk.o:	$(HFILES)
ssapselect.o:	$(HFILES)
ssaplose.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h


################################################################
# manual pages
################################################################

MANUALS	=	libssap.3n

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a z* _* core ssapvrsn.c

grind:;		iprint Makefile
		tgrind -lc $(CFILES) ssapvrsn.c llib-lssap
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

