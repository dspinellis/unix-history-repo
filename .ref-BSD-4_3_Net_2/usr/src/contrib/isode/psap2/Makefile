###############################################################################
#   Instructions to Make, for compilation of ISODE PSAP2 processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/psap2/RCS/Makefile,v 7.5 91/02/22 09:37:20 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.5  91/02/22  09:37:20  mrose
# Interim 6.8
# 
# Revision 7.4  90/12/23  18:42:30  mrose
# update
# 
# Revision 7.3  90/07/09  14:44:35  mrose
# sync
# 
# Revision 7.2  90/07/01  21:04:54  mrose
# pepsy
# 
# Revision 7.1  90/03/06  13:56:40  mrose
# touch-up
# 
# Revision 7.0  89/11/23  22:14:10  mrose
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


LIBES	=	libpsap2.a
LLIBS   =	$(TOPDIR)llib-lpsap $(TOPDIR)llib-lssap $(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)psap2.h $(HDIR)psap.h $(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h  $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		libpsap2
inst-all:	# inst-libpsap2 manuals
install:	inst-all clean
lint:		l-libpsap2


################################################################
# libpsap2
################################################################

CFILES	=	psaprovider.c psap2error.c \
		psapexec.c psaprespond.c psapinitiate.c psaptoken.c \
		psapactivity.c psapmajor1.c psapmajor2.c psapminor1.c \
		psapminor2.c psapresync1.c psapresync2.c psapabort.c \
		psapreport.c psaprelease1.c psaprelease2.c psapselect.c \
		psaplose.c
PYFILES	=	ps.py
OFILES	=	psaprovider.o psap2error.o \
		psapexec.o psaprespond.o psapinitiate.o psaptoken.o \
		psapactivity.o psapmajor1.o psapmajor2.o psapminor1.o \
		psapminor2.o psapresync1.o psapresync2.o psapabort.o \
		psapreport.o psaprelease1.o psaprelease2.o psapselect.o \
		psaplose.o \
		$(OSTRINGS)

inst-libpsap2:	$(LIBDIR)libpsap2.a $(LINTDIR)llib-lpsap2

$(LIBDIR)libpsap2.a:	libpsap2.a
		-rm -f $@
		cp libpsap2.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lpsap2:	llib-lpsap2
		-cp $@ zllib-lpsap2
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lpsap2 | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libpsap2:	libpsap2.a

libpsap2.a:	psap2vrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			PS_tables.o psap2vrsn.o
		-@rm -f $(TOPDIR)libpsap2.a $(TOPDIR)llib-lpsap2
		-@$(LN) libpsap2.a $(TOPDIR)libpsap2.a
		-@$(LN) llib-lpsap2 $(TOPDIR)llib-lpsap2
		-@ls -l $@
		-@echo "PSAP2 library built normally"

PS_tables.o:	PS_tables.c PS-types.h

PS_tables.c PS-types.h: ps.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m ps.py


psap2vrsn.c:	$(OFILES) PS_tables.o
		@$(UTILDIR)version.sh psap2 > $@

l-libpsap2:	PS_tables.c true
		$(LINT) $(LFLAGS) $(CFILES) PS_tables.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

psaprovider.o:	PS-types.h $(HDIR)ppkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
psap2error.o:	$(HFILES)
psapexec.o:	PS-types.h $(HDIR)ppkt.h $(HFILES) $(HDIR)isoservent.h \
		$(HDIR)tailor.h $(HDIR)logger.h
psaprespond.o:	PS-types.h $(HDIR)ppkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
psapinitiate.o:	PS-types.h $(HDIR)ppkt.h $(HFILES) $(HDIR)isoservent.h \
		$(HDIR)tailor.h $(HDIR)logger.h
psaptoken.o:	$(HDIR)ppkt.h $(HFILES)
psapactivity.o:	$(HDIR)ppkt.h $(HFILES)
psapmajor1.o:	$(HDIR)ppkt.h $(HFILES)
psapmajor2.o:	$(HDIR)ppkt.h $(HFILES)
psapminor1.o:	$(HDIR)ppkt.h $(HFILES)
psapminor2.o:	$(HDIR)ppkt.h $(HFILES)
psapresync1.o:	$(HDIR)ppkt.h $(HFILES)
psapresync2.o:	$(HDIR)ppkt.h $(HFILES)
psapabort.o:	PS-types.h $(HDIR)ppkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h
psapreport.o:	$(HDIR)ppkt.h $(HFILES)
psaprelease1.o:	$(HDIR)ppkt.h $(HFILES)
psaprelease2.o:	$(HDIR)ppkt.h $(HFILES)
psapselect.o:	$(HDIR)ppkt.h $(HFILES)
psaplose.o:	PS-types.h $(HDIR)ppkt.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h


################################################################
# manual pages
################################################################

MANUALS	=	libpsap2.3n

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a PS* z* _* core psap2vrsn.c

grind:;		iprint Makefile
		tgrind -lc $(CFILES) psap2vrsn.c llib-lpsap2
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(PYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
