###############################################################################
#   Instructions to Make, for compilation of ISODE AcSAP processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/acsap/RCS/Makefile,v 7.11 91/02/22 09:13:58 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.11  91/02/22  09:13:58  mrose
# Interim 6.8
# 
# Revision 7.10  91/01/24  14:49:52  mrose
# update
# 
# Revision 7.9  90/12/23  18:38:56  mrose
# update
# 
# Revision 7.8  90/11/20  15:33:24  mrose
# update
# 
# Revision 7.7  90/10/15  18:19:25  mrose
# zap-AET
# 
# Revision 7.6  90/07/27  08:41:38  mrose
# update
# 
# Revision 7.5  90/07/09  14:30:21  mrose
# sync
# 
# Revision 7.4  90/07/01  21:01:46  mrose
# pepsy
# 
# Revision 7.3  90/03/06  13:55:40  mrose
# touch-up
# 
# Revision 7.2  89/12/20  01:40:07  mrose
# touch-up
# 
# Revision 7.1  89/12/19  16:18:05  mrose
# dgram
# 
# Revision 7.0  89/11/23  21:21:41  mrose
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

.SUFFIXES:	.py .c .o


LIBES	=	libacsap.a $(TOPDIR)libpepy.a $(TOPDIR)libpepsy.a \
		$(TOPDIR)libpsap.a $(TOPDIR)libtsap.a $(TOPDIR)libcompat.a
LLIBS   =	$(TOPDIR)llib-lpsap2 $(TOPDIR)llib-lpsap  $(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)acsap.h $(HDIR)psap2.h $(HDIR)psap.h \
		$(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		libacsap acsaptest
inst-all:	inst-headers # inst-libacsap manuals
install:	inst-all clean
lint:		l-libacsap l-acsaptest


################################################################
# libacsap
################################################################

CFILES	=	acsaprovider.c acsapblock.c acsaperror.c acsaplose.c \
		acsaprespond.c acsapinitiat.c acsapreleas1.c acsapreleas2.c \
		acsapreleas3.c acsapabort1.c acsapabort2.c acsapfindpci.c \
		acsapaddr.c \
		acsapdse.c aetdase.c isoalias.c \
		acsapstub.c aetseq.c isoentity.c \
		oid2aei.c sprintaei.c \
		is2paddr.c is2saddr.c is2taddr.c \
		acserver1.c acserver2.c \
		dse-c.c
PYFILES	=	acs.py acsold.py dse.py ns.py
OFILES	=	acsaprovider.o acsapblock.o acsaperror.o acsaplose.o \
		acsaprespond.o acsapinitiat.o acsapreleas1.o acsapreleas2.o \
		acsapreleas3.o acsapabort1.o acsapabort2.o acsapfindpci.o \
		acsapaddr.o \
		acsapdse.o aetdase.o isoalias.o \
		acsapstub.o aetseq.o isoentity.o \
		oid2aei.o sprintaei.o \
		is2paddr.o is2saddr.o is2taddr.o \
		acserver1.o acserver2.o \
		dse-c.o \
		$(OSTRINGS)

HEADERS	=	ACS.ph ACS-types.h ACS_defs.h ACS_pre_defs.h \
		DSE.ph DSE-types.h DSE_defs.h DSE_pre_defs.h 
# Possibly DASE and OACS as well


inst-libacsap:	$(LIBDIR)libacsap.a $(LINTDIR)llib-lacsap

$(LIBDIR)libacsap.a:	libacsap.a
		-rm -f $@
		cp libacsap.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lacsap:	llib-lacsap
		-cp $@ zllib-lacsap
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lacsap | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

inst-headers:;	@for h in $(HEADERS); do $(MAKE) TARGET=$$h inst-target; done

inst-target:	$(PEPSYDIR)$(TARGET)

$(PEPSYDIR)$(TARGET):	$(TARGET)
		-cp $@ z$(TARGET)
		cp $(TARGET) $@
		-@ls -gls $@
		-@echo ""

libacsap:	libacsap.a true
		-@rm -f $(TOPDIR)h/ACS*.h $(TOPDIR)h/OACS*.h
		-@$(LN) ACS-types.h $(TOPDIR)h/ACS-types.h
		-@$(LN) ACS_defs.h $(TOPDIR)h/ACS_defs.h
		-@$(LN) ACS_pre_defs.h $(TOPDIR)h/ACS_pre_defs.h
		-@$(LN) OACS-types.h $(TOPDIR)h/OACS-types.h
		-@$(LN) OACS_defs.h $(TOPDIR)h/OACS_defs.h
		-@$(LN) OACS_pre_defs.h $(TOPDIR)h/OACS_pre_defs.h

libacsap.a:	acsapvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			ACS_tables.o OACS_tables.o DSE_tables.o DASE_tables.o \
			acsapvrsn.o
		-@rm -f $(TOPDIR)libacsap.a $(TOPDIR)llib-lacsap
		-@$(LN) libacsap.a $(TOPDIR)libacsap.a
		-@$(LN) llib-lacsap $(TOPDIR)llib-lacsap
		-@ls -l $@
		-@echo "AcSAP library built normally"

ACS_tables.o:	ACS_tables.c ACS-types.h

ACS_tables.c ACS-types.h: acs.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m acs.py

OACS_tables.o:	OACS_tables.c OACS-types.h

OACS_tables.c OACS-types.h: acsold.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m acsold.py
		sed -e 's/decode_/parse_/' < OACS_defs.h > OACS_defs.tmp
		mv OACS_defs.tmp OACS_defs.h

DSE_tables.o:	DSE_tables.c DSE-types.h

DSE_tables.c DSE-types.h: dse.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m dse.py

DASE_tables.o:	DASE_tables.c DASE-types.h

DASE_tables.c DASE-types.h: dase.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m dase.py

acsapvrsn.c:	$(OFILES) ACS_tables.o OACS_tables.o DSE_tables.o DASE_tables.o
		@$(UTILDIR)version.sh acsap > $@

l-libacsap:	ACS_tables.c OACS_tables.c DSE_tables.c DASE_tables.c true
		$(LINT) $(LFLAGS) $(CFILES) ACS_tables.c OACS_tables.c \
			DSE_tables.c DASE_tables.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

acsaprovider.o:	ACS-types.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
acsapblock.o:	$(HDIR)acpkt.h $(HFILES)
acsaperror.o:	$(HFILES)
acsaplose.o: 	ACS-types.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
acsaprespond.o: ACS-types.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
acsapinitiat.o: ACS-types.h $(HDIR)acpkt.h $(HFILES) $(HDIR)isoservent.h \
		$(HDIR)tailor.h $(HDIR)logger.h
acsapreleas1.o: ACS-types.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
acsapreleas2.o: ACS-types.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
acsapreleas3.o: ACS-types.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
acsapabort1.o: 	ACS-types.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
acsapabort2.o: 	ACS-types.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
acsapabort2.o: 	$(HDIR)acpkt.h $(HFILES)
acsapaddr.o: 	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
acsapdse.o: 	DSE-types.h $(HDIR)psap.h $(HDIR)manifest.h $(HDIR)general.h \
		$(HDIR)config.h $(HDIR)isoaddrs.h $(HDIR)tailor.h \
		$(HDIR)logger.h
aetdase.o: 	DASE-types.h $(HDIR)psap.h $(HDIR)manifest.h $(HDIR)general.h \
		$(HDIR)config.h $(HDIR)tsap.h $(HDIR)isoaddrs.h \
		$(HDIR)dgram.h $(HDIR)tailor.h $(HDIR)logger.h
isoalias.o:	$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h \
		$(HDIR)tailor.h $(HDIR)logger.h
acsapstub.o: 	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
aetseq.o:	$(HFILES)
isoentity.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
oid2aei.o:	$(HFILES)
sprintaei.o:	$(HFILES)
is2paddr.o:	$(HFILES)
is2saddr.o:	$(HFILES)
is2taddr.o:	$(HFILES)
acserver1.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
acserver2.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
acsold.o:	$(HDIR)acpkt.h $(HFILES)
acsold.o:	acsold.c
acsold.c:	acsold.py
dse-c.o:	DSE-types.h


################################################################
# acsaptest
################################################################

acsaptest:	acsaptest.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ acsaptest.o $(LIBES) $(LSOCKET)

acsaptest.o:	$(HFILES) $(HDIR)isoservent.h $(HDIR)tailor.h $(HDIR)logger.h \
			acsaptest.c
		$(CC) $(CFLAGS) -c $*.c

l-acsaptest:;	$(LINT) $(LFLAGS) acsaptest.c llib-lacsap $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# dased
################################################################

inst-dased:	$(SBINDIR)dased

$(SBINDIR)dased:	xdased
		-cp $@ zxdased
		-rm -f $@
		cp xdased $@
		-@ls -gls $@
		-@echo ""

dased:		xdased

xdased:		dased.o DASE_tables.o $(TOPDIR)libdsap.a $(TOPDIR)libisode.a
		$(LDCC) $(LDFLAGS) -o $@ dased.o DASE_tables.o \
			$(LIBDSAP) $(LIBISODE) $(LSOCKET) $(LIBGDBM)

dased.o: 	$(HDIR)manifest.h $(HDIR)config.h DASE-types.h $(HDIR)psap.h \
		$(HDIR)general.h $(HDIR)tsap.h $(HDIR)isoaddrs.h \
		$(HDIR)dgram.h $(HDIR)tailor.h $(HDIR)logger.h
		$(CC) $(CFLAGS) -c $*.c

l-dased:;	$(LINT) $(LFLAGS) dased.c $(TOPDIR)llib-ldsap llib-lacsap \
			$(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

man-dased:;	@$(UTILDIR)inst-man.sh $(MANOPTS) dased.8c
		-@echo ""


################################################################
# manual pages
################################################################

MANUALS	=	libacsap.3n

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a *.ph ACS* OACS* DSE* DASE* x* z* _* core \
			acsaptest acsapvrsn.c $(TOPDIR)h/ACS*.h \
			$(TOPDIR)h/OACS*.h

grind:;		iprint Makefile
		tgrind -lc $(CFILES) acsapvrsn.c acsaptest.c llib-lacsap
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(PYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
