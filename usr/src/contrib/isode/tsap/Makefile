###############################################################################
#   Instructions to Make, for compilation of ISODE TSAP processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/tsap/RCS/Makefile,v 7.3 91/02/22 09:47:03 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.3  91/02/22  09:47:03  mrose
# Interim 6.8
# 
# Revision 7.2  90/12/23  18:43:23  mrose
# update
# 
# Revision 7.1  90/07/09  14:51:03  mrose
# sync
# 
# Revision 7.0  89/11/23  22:30:25  mrose
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


LIBES	=	libtsap.a $(TOPDIR)libcompat.a
LLIBS	=	$(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)tsap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		libtsap
inst-all:	# inst-libtsap manuals
install:	inst-all clean
lint:		l-libtsap


################################################################
# libtsap
################################################################

CFILES	=	tsaprovider.c tsaperror.c tsapstate.c \
		fd2tpkt.c str2tpkt.c text2tpkt.c \
		tsaprespond.c tsapinitiate.c tsaplose.c \
		tsaplisten.c tsapmgmt.c tsapmisc.c \
		tp0ts.c ts2tcp.c ts2x25.c ts2bridge.c \
		ts2bsd.c ts2sunlink.c $(CTSAP)
OFILES	=	tsaprovider.o tsaperror.o tsapstate.o \
		fd2tpkt.o str2tpkt.o text2tpkt.o \
		tsaprespond.o tsapinitiate.o tsaplose.o \
		tsaplisten.o tsapmgmt.o tsapmisc.o \
		tp0ts.o ts2tcp.o ts2x25.o ts2bridge.o \
		ts2bsd.o ts2sunlink.o $(OTSAP) \
		$(OSTRINGS)


inst-libtsap:	$(LIBDIR)libtsap.a $(LINTDIR)llib-ltsap

$(LIBDIR)libtsap.a:	libtsap.a
		-rm -f $@
		cp libtsap.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-ltsap:	llib-ltsap
		-cp $@ zllib-ltsap
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-ltsap | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libtsap:	libtsap.a

libtsap.a:	tsapvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			tsapvrsn.o
		-@rm -f $(TOPDIR)libtsap.a $(TOPDIR)llib-ltsap
		-@$(LN) libtsap.a $(TOPDIR)libtsap.a
		-@$(LN) llib-ltsap $(TOPDIR)llib-ltsap
		-@ls -l $@
		-@echo "TSAP library built normally"

tsapvrsn.c:	$(OFILES)
		@$(UTILDIR)version.sh tsap > $@

l-libtsap:;	$(LINT) $(LFLAGS) $(CFILES) tsapvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

tsaprovider.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)x25.h $(HDIR)isoservent.h \
		$(HDIR)tailor.h $(HDIR)logger.h $(HDIR)mpkt.h
tsaperror.o:	$(HFILES)
tsapstate.o:	$(HDIR)tpkt.h $(HFILES)
fd2tpkt.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
str2tpkt.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
text2tpkt.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)logger.h
tsaprespond.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
tsapinitiate.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)isoservent.h $(HDIR)tailor.h \
		$(HDIR)logger.h $(HDIR)mpkt.h
tsaplose.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)tailor.h $(HDIR)logger.h \
		$(HDIR)mpkt.h
tsaplisten.o:	$(HDIR)tpkt.h $(HFILES)  $(HDIR)mpkt.h $(HDIR)internet.h \
		$(HDIR)x25.h $(HDIR)tp4.h
tsapmgmt.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)mpkt.h
tsapmisc.o:	$(HDIR)tpkt.h $(HFILES)
tp0ts.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)tailor.h $(HDIR)logger.h \
		$(HDIR)mpkt.h
ts2tcp.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)internet.h $(HDIR)tailor.h \
		$(HDIR)logger.h
ts2x25.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)x25.h $(HDIR)tailor.h \
		$(HDIR)logger.h
ts2x25.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
ts2bridge.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
ts2bsd.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)tp4.h $(HDIR)mpkt.h
ts2sunlink.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)tp4.h $(HDIR)mpkt.h


################################################################
# manual pages
################################################################

MANUALS	=	libtsap.3n

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a z* _* core tsapvrsn.c

grind:;		iprint Makefile
		tgrind -lc $(CFILES) tsapvrsn.c llib-ltsap
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve
