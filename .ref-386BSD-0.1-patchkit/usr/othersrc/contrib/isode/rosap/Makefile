###############################################################################
#   Instructions to Make, for compilation of ISODE RoSAP processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/rosap/RCS/Makefile,v 7.4 91/02/22 09:41:05 mrose Interim $
#
# Based on an TCP-based implementation by George Michaelson of University
# College London.
#
#
# $Log:	Makefile,v $
# Revision 7.4  91/02/22  09:41:05  mrose
# Interim 6.8
# 
# Revision 7.3  90/12/23  18:42:43  mrose
# update
# 
# Revision 7.2  90/07/09  14:47:37  mrose
# sync
# 
# Revision 7.1  90/07/01  21:05:40  mrose
# pepsy
# 
# Revision 6.1  89/07/22  16:05:51  mrose
# bsd44
# 
# Revision 6.0  89/03/18  23:42:03  mrose
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


LIBES	=	librosap.a
LLIBS   =	$(TOPDIR)llib-lrtsap $(TOPDIR)llib-lacsap \
		$(TOPDIR)llib-lpsap2 $(TOPDIR)llib-lpsap $(TOPDIR)llib-lssap \
		$(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)rosap.h $(HDIR)rtsap.h $(HDIR)acsap.h $(HDIR)psap2.h \
		$(HDIR)psap.h \
		$(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h

##################################################################
# Here it is...
##################################################################

all:		librosap
inst-all:	# inst-librosap manuals
install:	inst-all clean
lint:		l-librosap


################################################################
# librosap
################################################################

CFILES	=	rosaperror.c \
		rosapinvoke.c rosapresult.c rosapuerror.c rosapureject.c \
		rosapintr.c rosapwait.c rosapservice.c rosapapdu.c \
		rosapasync.c rosapselect.c rosaplose.c \
		ro2rts.c \
		ro2ps.c \
		ro2ssexec.c ro2ssrespond.c ro2ssinitiat.c ro2ssreleas1.c \
		ro2ssreleas2.c ro2ss.c ro2ssthorn.c
PYFILES	=	ros.py
OFILES	=	rosaperror.o \
		rosapinvoke.o rosapresult.o rosapuerror.o rosapureject.o \
		rosapintr.o rosapwait.o rosapservice.o rosapapdu.o \
		rosapasync.o rosapselect.o rosaplose.o \
		ro2rts.o \
		ro2ps.o \
		ro2ssexec.o ro2ssrespond.o ro2ssinitiat.o ro2ssreleas1.o \
		ro2ssreleas2.o ro2ss.o ro2ssthorn.o \
		$(OSTRINGS)

inst-librosap:	$(LIBDIR)librosap.a $(LINTDIR)llib-lrosap

$(LIBDIR)librosap.a:	librosap.a
		-rm -f $@
		cp librosap.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lrosap:	llib-lrosap
		-cp $@ zllib-lrosap
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lrosap | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

librosap:	librosap.a

librosap.a:	rosapvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			ROS_tables.o rosapvrsn.o
		-@rm -f $(TOPDIR)librosap.a $(TOPDIR)llib-lrosap
		-@$(LN) librosap.a $(TOPDIR)librosap.a
		-@$(LN) llib-lrosap $(TOPDIR)llib-lrosap
		-@ls -l $@
		-@echo "RoSAP library built normally"

ROS_tables.o:	ROS_tables.c ROS-types.h

ROS_tables.c ROS-types.h: ros.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m ros.py


rosapvrsn.c:	$(OFILES) ROS_tables.o
		@$(UTILDIR)version.sh rosap > $@

l-librosap:	ROS_tables.c true
		$(LINT) $(LFLAGS) $(CFILES) ROS_tables.c rosapvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


rosaperror.o:	$(HFILES)
rosapinvoke.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) ROS-types.h
rosapresult.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) ROS-types.h
rosapuerror.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES)
rosapureject.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES)
rosapintr.o:	$(HFILES)
rosapwait.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES)
rosapservice.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES)
rosapapdu.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h ROS-types.h
rosapapdu.c: ROS-types.h
rosapasync.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES)
rosapselect.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES)
rosaplose.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
ro2rts.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h ROS-types.h
ro2ps.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h ROS-types.h
ro2ssexec.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)isoservent.h \
		$(HDIR)tailor.h $(HDIR)logger.h ROS-types.h
ro2ssrespond.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
ro2ssinitiat.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)isoservent.h \
		$(HDIR)tailor.h $(HDIR)logger.h
ro2ssreleas1.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES)
ro2ssreleas2.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES)
ro2ss.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h ROS-types.h
ro2ssthorn.o:	$(HDIR)ropkt.h $(HDIR)acpkt.h $(HFILES)


################################################################
# manual pages
################################################################

MANUALS	=	librosap.3n

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a *.ph ROS* z* _* core rosapvrsn.c

grind:;		iprint Makefile
		tgrind -lc $(CFILES) rosapvrsn.c rosaptest.c llib-lrosap
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(PYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
