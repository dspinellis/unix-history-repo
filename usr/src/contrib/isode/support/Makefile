###############################################################################
#   Instructions to Make, for compilation of ISODE support processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/support/RCS/Makefile,v 7.10 91/02/22 09:46:19 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.10  91/02/22  09:46:19  mrose
# Interim 6.8
# 
# Revision 7.9  91/01/24  14:50:37  mrose
# update
# 
# Revision 7.8  90/12/23  18:43:17  mrose
# update
# 
# Revision 7.7  90/11/20  15:33:04  mrose
# update
# 
# Revision 7.6  90/11/04  19:16:46  mrose
# update
# 
# Revision 7.5  90/10/15  18:19:03  mrose
# iaed
# zap-AET
# 
# Revision 7.4  90/08/14  14:30:30  mrose
# oops
# 
# Revision 7.3  90/07/27  08:48:06  mrose
# update
# 
# Revision 7.2  90/07/09  14:50:34  mrose
# sync
# 
# Revision 7.1  90/07/01  21:07:47  mrose
# pepsy
# 
# Revision 7.0  89/11/23  22:27:04  mrose
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

.c.o:;		$(CC) $(CFLAGS) -c $*.c


LIBES   =       $(TOPDIR)librosy.a  $(TOPDIR)libronot.a $(TOPDIR)librosap.a \
		$(TOPDIR)librtsap.a $(TOPDIR)libacsap.a $(TOPDIR)libpsap2.a \
		$(TOPDIR)libpepy.a  $(TOPDIR)libpepsy.a $(TOPDIR)libpsap.a \
		$(TOPDIR)libssap.a  $(TOPDIR)libtsap.a  $(TOPDIR)libdirent.a \
		$(TOPDIR)libcompat.a \
		$(TP4LIBES)


LLIBS   =       $(TOPDIR)llib-lrosy $(TOPDIR)llib-lronot $(TOPDIR)llib-lrosap \
		$(TOPDIR)llib-lrtsap $(TOPDIR)llib-lacsap $(TOPDIR)llib-lpsap2 \
                $(TOPDIR)llib-lpsap $(TOPDIR)llib-lssap $(TOPDIR)llib-ltsap \
                $(TOPDIR)llib-ldirent $(TOPDIR)llib-lcompat $(TP4LLIBS)

CFILES	=	tsapd.c isore.c isod.c isoc.c
HFILES	=	$(HDIR)rosap.h $(HDIR)rtsap.h $(HDIR)acsap.h $(HDIR)psap2.h \
		$(HDIR)psap.h $(HDIR)ssap.h $(HDIR)tsap.h \
		$(HDIR)isoaddrs.h $(HDIR)isoservent.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		libisode tsapd isore isod isoc \
		isoaliases isoentities isomacros isobjects isoservices
inst-all:	inst-tsapd inst-isore inst-isod inst-isoc \
		inst-libisode aliases entities macros objects services manuals
install:	inst-all clean
lint:		l-tsapd l-isore l-isod l-isoc

all-lpp:	lppd isoentities
inst-lpp:	inst-lppd entities macros objects services manuals-lpp
install-lpp:	inst-lpp clean
lint-lpp:	l-lppd

inst-:;
man-:;
l-:;


##################################################################
# tsapd
##################################################################

inst-tsapd:	$(SBINDIR)tsapd

$(SBINDIR)tsapd:	xtsapd
		-cp $@ zxtsapd
		-rm -f $@
		cp xtsapd $@
		-@ls -gls $@
		-@echo ""

tsapd:		xtsapd

xtsapd:		tsapd.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ tsapd.o $(LIBISODE) $(LSOCKET)

l-tsapd:;	$(LINT) $(LFLAGS) tsapd.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

tsapd.o:	$(HFILES) $(HDIR)x25.h $(HDIR)logger.h $(HDIR)tailor.h


inst-iaed:	$(SBINDIR)iaed

$(SBINDIR)iaed:	xiaed
		-cp $@ zxiaed
		-rm -f $@
		cp xiaed $@
		-@ls -gls $@
		-@echo ""

iaed:		xiaed

xiaed:		iaed.o $(TOPDIR)libdsap.a $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ iaed.o \
			$(LIBDSAP) $(LIBISODE) $(LSOCKET) $(LIBGDBM)

iaed.o:		$(HFILES) $(HDIR)x25.h $(HDIR)logger.h $(HDIR)tailor.h \
			$(HDIR)quipu/util.h $(HDIR)quipu/config.h \
			$(HDIR)quipu/bind.h $(HDIR)quipu/name.h \
				$(HDIR)quipu/authen.h \
			$(HDIR)quipu/list.h $(HDIR)quipu/commonarg.h \
				$(HDIR)quipu/ds_error.h $(HDIR)quipu/dap.h \
			$(HDIR)quipu/ds_search.h tsapd.c
		$(CC) -o $@ $(CFLAGS) -DIAE -c tsapd.c

l-iaed:;	$(LINT) $(LFLAGS) -DIAE tsapd.c $(TOPDIR)llib-ldsap $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

man-iaed:;	@$(UTILDIR)inst-man.sh $(MANOPTS) iaed.8c
		-@echo ""

##################################################################
# isore
##################################################################

inst-isore:	$(SBINDIR)isore

$(SBINDIR)isore:	xisore
		-cp $@ zxisore
		-rm -f $@
		cp xisore $@
		-@ls -gls $@
		-@echo ""

isore:		xisore

xisore:		isore.o
		$(LDCC) $(LDFLAGS) -o $@ isore.o $(TOPDIR)libcompat.a \
			$(LSOCKET)

l-isore:;	$(LINT) $(LFLAGS) isore.c \
			| grep -v "warning: possible pointer alignment problem"


##################################################################
# isod
##################################################################

inst-isod:	$(SBINDIR)isod.tsap

$(SBINDIR)isod.tsap:	xisod
		-cp $@ zxisod
		-rm -f $@ \
			$(SBINDIR)isod.ssap \
			$(SBINDIR)isod.psap \
			$(SBINDIR)isod.acsap \
			$(SBINDIR)isod.rtsap \
			$(SBINDIR)isod.rosap
		cp xisod $@
		-ln $@  $(SBINDIR)isod.ssap
		-ln $@  $(SBINDIR)isod.psap
		-ln $@  $(SBINDIR)isod.acsap
		-ln $@  $(SBINDIR)isod.rtsap
		-ln $@  $(SBINDIR)isod.rosap
		-@ls -gls $@
		-@echo ""

isod:		xisod

xisod:		isod.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ isod.o $(LIBISODE) $(LSOCKET)

l-isod:;	$(LINT) $(LFLAGS) isod.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

isod.o:		$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h


##################################################################
# isoc
##################################################################

inst-isoc:	$(BINDIR)isoc

$(BINDIR)isoc:	xisoc
		-cp $@ zxisoc
		-rm -f $@
		cp xisoc $@
		-@ls -gls $@
		-@echo ""

isoc:		xisoc

xisoc:		isoc.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ isoc.o $(LIBISODE) $(LSOCKET)

l-isoc:;	$(LINT) $(LFLAGS) isoc.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

isoc.o:		$(HFILES) $(HDIR)internet.h


################################################################
# libisode
################################################################

inst-libisode:	$(LIBDIR)libisode.a $(LINTDIR)llib-lisode

$(LIBDIR)libisode.a:	libisode.a
		@for i in libisode.* ;\
		do \
			rm -f $(LIBDIR)$$i; \
			echo cp $$i $(LIBDIR)$$i; \
			cp $$i $(LIBDIR)$$i; \
			case "$$i" in *.a) \
			$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib ;;\
			esac; \
			ls -gls $(LIBDIR)$$i ; \
		done
		-@echo ""

$(LINTDIR)llib-lisode:	llib-lisode
		-cp $@ zllib-lisode
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lisode | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libisode:	libisode.a libisode-$(SHAREDLIB) llib-lisode

libisode.a:	isodevrsn.o
		-rm -f $@ $(TOPDIR)libisode.a
		ar cr libisode.a isodevrsn.o
		@for i in $(LIBES); do ( echo adding $$i;\
		    cp $$i /tmp/k.a; ar d /tmp/k.a __.SYMDEF; \
		    (dd bs=8 count=1 of=/dev/null; dd bs=4096) < /tmp/k.a  \
		    >> libisode.a 2> /dev/null); done
		rm /tmp/k.a
		$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@rm -f $(TOPDIR)libisode.a
		-@$(LN) $@ $(TOPDIR)libisode.a
		-@ls -l $@
		-@echo "ISODE library built normally"

libisode-:;

libisode-shared: isodevrsn.o
		@rm -f libisode.so.* $(TOPDIR)libisode.so.*
		@$(UTILDIR)make-lib.sh $(SYSTEM) -shared \
			-major `cat version.major``cat version.minor` \
			-minor `cat version.local` \
			libisode.a 
		@for i in libisode.s[ao].* ;\
		do \
			rm -f $(TOPDIR)$$i; \
			$(LN) $$i $(TOPDIR)$$i; \
			ls -l $$i; \
		done
		@echo "shared ISODE library built normally"
		@touch $@
		

llib-lisode:	$(LLIBS)
		-@echo '/* llib-lisode - lint library for -lisode */' > $@
		-@echo '' >> $@
		cat $(LLIBS) >> $@
		-@rm -f $(TOPDIR)llib-lisode
		-@$(LN) llib-lisode $(TOPDIR)llib-lisode

isodevrsn.c:	$(LIBES)
		@$(UTILDIR)version.sh isode > $@


################################################################
# aliases/entities/macros/objects/services/tailor
################################################################

aliases:	$(ETCDIR)isoaliases

$(ETCDIR)isoaliases:	isoaliases
		-cp $@ zisoaliases
		cp isoaliases $@
		-@ls -gls $@
		-@echo ""

isoaliases:	aliases.local aliases.db
		cat aliases.local aliases.db > $@


entities:	$(ETCDIR)isoentities

$(ETCDIR)isoentities:	isoentities
		-cp $@ zisoentities
		cp isoentities $@
		-@ls -gls $@
		-@if [ -f $(SBINDIR)aetbuild ]; then \
		    rm -f $(SBINDIR)aetbuild; \
		    rm -f $@.dir $@.pag; fi
		-@echo ""

isoentities:	entities.prefix entities.local entities.db
		cat entities.prefix entities.local entities.db > $@


macros:		$(ETCDIR)isomacros

$(ETCDIR)isomacros:	isomacros
		-cp $@ zisomacros
		cp isomacros $@
		-@ls -gls $@
		-@echo ""

isomacros:	macros.prefix macros.local macros.db
		cat macros.prefix macros.local macros.db > $@


objects:	$(ETCDIR)isobjects

$(ETCDIR)isobjects:	isobjects
		-cp $@ zisobjects
		cp isobjects $@
		-@ls -gls $@
		-@echo ""

isobjects:	objects.local objects.db
		cat objects.local objects.db > $@


services:	$(ETCDIR)isoservices

$(ETCDIR)isoservices:	isoservices
		-cp $@ zisoservices
		cp isoservices $@
		-@ls -gls $@
		-@echo ""

isoservices:	services.local services.db
		cat services.local services.db > $@


tailor:		$(ETCDIR)isotailor

$(ETCDIR)isotailor:	isotailor
		-cp $@ zisotailor
		cp isotailor $@
		-@ls -gls $@
		-@echo ""


################################################################
# lppd
################################################################

LPP-LIBES=	$(TOPDIR)libisode-lpp.a
LPP-LLIBS=	$(TOPDIR)llib-lisode-lpp

inst-lppd:	$(SBINDIR)lppd

$(SBINDIR)lppd:	xlppd
		-cp $@ zxlppd
		-rm -f $@
		cp xlppd $@
		-@ls -gls $@
		-@echo ""

lppd:		xlppd

xlppd:		lppd.o $(LPP-LIBES)
		$(LDCC) $(LDFLAGS) -o $@ lppd.o $(LPP-LIBES) $(LSOCKET)

l-lppd:;	$(LINT) $(LFLAGS) lppd.c $(LPP-LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

lppd.o:		$(HFILES)


MANUALS-LPP=	isoentities.5 isobjects.5 isoservices.5 isotailor.5 lppd.8c

manuals-lpp:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS-LPP)
		-@echo ""


################################################################
# manual pages
################################################################

MANUALS	=	isoc.1c isoaliases.5 isoentities.5 isomacros.5 isobjects.5 \
		isoservices.5 isotailor.5 tsapd.8c isore.8c isod.8c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a *.so.* x* z* _* core isodevrsn.c llib-lisode \
		    isoaliases isoentities isomacros isobjects isoservices \
		    libisode-shared

grind:		isoaliases isoentities isomacros isobjects isoservices \
			isotailor true
		iprint Makefile isoaliases isoentities isomacros isobjects \
			isoservices isotailor
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;


################################################################
# testing...
################################################################

HOST	=	localhost
FILE1	=	/etc/mount
FILE2	=	/etc/fstab


test:;		-xisoc $(HOST) tsap echo < $(FILE1)
		-xisoc $(HOST) ssap echo < $(FILE1)
		-xisoc $(HOST) psap echo < $(FILE1)
		-xisoc $(HOST) psap isode/echo < $(FILE1)
		-xisoc $(HOST) rtsap echo < $(FILE1)
		-xisoc $(HOST) rtsap ros_echo < $(FILE1)
		-xisoc $(HOST) rtsap "isode/rtse echo" < $(FILE1)
		-xisoc $(HOST) rtsap isode/ros_echo < $(FILE1)
		-xisoc $(HOST) rosap echo < $(FILE1)
		-xisoc $(HOST) rosap isode/echo < $(FILE1)
		-xisoc $(HOST) tsap sink < $(FILE1)
		-xisoc $(HOST) ssap sink < $(FILE1)
		-xisoc $(HOST) psap sink < $(FILE1)
		-xisoc $(HOST) psap isode/sink < $(FILE1)
		-xisoc $(HOST) rtsap sink < $(FILE1)
		-xisoc $(HOST) rtsap ros_sink < $(FILE1)
		-xisoc $(HOST) rtsap "isode/rtse sink" < $(FILE1)
		-xisoc $(HOST) rtsap isode/ros_sink < $(FILE1)
		-xisoc $(HOST) rosap sink < $(FILE1)
		-xisoc $(HOST) rosap isode/sink < $(FILE1)
		-cat $(FILE2) | xisoc $(HOST) tsap echo
		-cat $(FILE2) | xisoc $(HOST) ssap echo
		-cat $(FILE2) | xisoc $(HOST) psap echo
		-cat $(FILE2) | xisoc $(HOST) psap isode/echo
		-cat $(FILE2) | xisoc $(HOST) rtsap echo
		-cat $(FILE2) | xisoc $(HOST) rtsap ros_echo
		-cat $(FILE2) | xisoc $(HOST) rtsap "isode/rtse echo"
		-cat $(FILE2) | xisoc $(HOST) rtsap isode/ros_echo
		-cat $(FILE2) | xisoc $(HOST) rosap echo
		-cat $(FILE2) | xisoc $(HOST) rosap isode/echo
