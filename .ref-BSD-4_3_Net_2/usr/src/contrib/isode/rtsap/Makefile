###############################################################################
#   Instructions to Make, for compilation of ISODE RtSAP processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/rtsap/RCS/Makefile,v 7.4 91/02/22 09:42:10 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.4  91/02/22  09:42:10  mrose
# Interim 6.8
# 
# Revision 7.3  90/12/23  18:42:55  mrose
# update
# 
# Revision 7.2  90/07/09  14:48:12  mrose
# sync
# 
# Revision 7.1  90/07/01  21:06:44  mrose
# pepsy
# 
# Revision 6.1  89/07/22  16:05:55  mrose
# bsd44
# 
# Revision 6.0  89/03/18  23:43:01  mrose
# Release 5.0
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

.SUFFIXES:	.py .c .o


LIBES	=	librtsap.a
LLIBS   =	$(TOPDIR)llib-lacsap $(TOPDIR)llib-lpsap2 $(TOPDIR)llib-lpsap \
		$(TOPDIR)llib-lssap $(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)rtsap.h $(HDIR)acsap.h $(HDIR)psap2.h $(HDIR)psap.h \
		$(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h

##################################################################
# Here it is...
##################################################################

all:		librtsap
inst-all:	# inst-librtsap manuals
install:	inst-all clean
lint:		l-librtsap


################################################################
# librtsap
################################################################

CFILES	=	rtsaperror.c \
		rtsappturn.c rtsapgturn.c rtsaptrans.c rtsapwait.c \
		rtsapasync.c rtsapselect.c rtsaplose.c \
		rtsapdtrans.c rtsaputrans.c \
		rt2psrespond.c rt2psinitiat.c rt2psabort.c rt2psreleas1.c \
		rt2psreleas2.c rt2ps.c \
		rt2ssexec.c rt2ssrespond.c rt2ssinitiat.c rt2ssreleas1.c \
		rt2ssreleas2.c rt2ss.c
PYFILES	=	rts.py
OFILES	=	rtsaperror.o \
		rtsappturn.o rtsapgturn.o rtsaptrans.o rtsapwait.o \
		rtsapasync.o rtsapselect.o rtsaplose.o \
		rtsapdtrans.o rtsaputrans.o \
		rt2psrespond.o rt2psinitiat.o rt2psabort.o rt2psreleas1.o \
		rt2psreleas2.o rt2ps.o \
		rt2ssexec.o rt2ssrespond.o rt2ssinitiat.o rt2ssreleas1.o \
		rt2ssreleas2.o rt2ss.o \
		$(OSTRINGS)

inst-librtsap:	$(LIBDIR)librtsap.a $(LINTDIR)llib-lrtsap

$(LIBDIR)librtsap.a:	librtsap.a
		-rm -f $@
		cp librtsap.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lrtsap:	llib-lrtsap
		-cp $@ zllib-lrtsap
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lrtsap | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

librtsap:	librtsap.a

librtsap.a:	rtsapvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			RTS_tables.o rtsapvrsn.o
		-@rm -f $(TOPDIR)librtsap.a $(TOPDIR)llib-lrtsap
		-@$(LN) librtsap.a $(TOPDIR)librtsap.a
		-@$(LN) llib-lrtsap $(TOPDIR)llib-lrtsap
		-@ls -l $@
		-@echo "RtSAP library built normally"

RTS_tables.o:	RTS_tables.c RTS-types.h

RTS_tables.c RTS-types.h: rts.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m rts.py


rtsapvrsn.c:	$(OFILES) RTS_tables.o 
		@$(UTILDIR)version.sh rtsap > $@

l-librtsap:	RTS_tables.c true
		$(LINT) $(LFLAGS) $(CFILES) RTS_tables.c rtsapvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

rtsaperror.o:	$(HFILES)
rtsappturn.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rtsapgturn.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rtsaptrans.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rtsapwait.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rtsapasync.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rtsapselect.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rtsaplose.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
rtsapdtrans.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rtsaputrans.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rt2psrespond.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h RTS-types.h
rt2psinitiat.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h RTS-types.h
rt2psabort.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
rt2psreleas1.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rt2psreleas2.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rt2ps.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h RTS-types.h
rt2ssexec.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)isoservent.h \
		$(HDIR)tailor.h $(HDIR)logger.h RTS-types.h
rt2ssrespond.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h RTS-types.h
rt2ssinitiat.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)isoservent.h \
		$(HDIR)tailor.h $(HDIR)logger.h RTS-types.h
rt2ssreleas1.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rt2ssreleas2.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES)
rt2ss.o:	$(HDIR)rtpkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h RTS-types.h


################################################################
# manual pages
################################################################

MANUALS	=	librtsap.3n

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a *.ph RTS* rts.c vrts.c z* _* core rtsapvrsn.c

grind:;		iprint Makefile
		tgrind -lc $(CFILES) rtsapvrsn.c llib-lrtsap
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(PYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
