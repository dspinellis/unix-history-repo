###############################################################################
#   Instructions to Make, for compilation of ISODE ROSY processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/rosy/RCS/Makefile,v 7.7 91/02/22 09:41:41 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.7  91/02/22  09:41:41  mrose
# Interim 6.8
# 
# Revision 7.6  91/01/07  12:41:18  mrose
# update
# 
# Revision 7.5  90/12/23  18:42:46  mrose
# update
# 
# Revision 7.4  90/11/04  19:16:03  mrose
# update
# 
# Revision 7.3  90/07/27  08:47:32  mrose
# update
# 
# Revision 7.2  90/07/09  14:47:56  mrose
# sync
# 
# Revision 7.1  90/07/01  21:06:17  mrose
# pepsy
# 
# Revision 6.2  89/07/30  12:16:24  mrose
# real
# 
# Revision 6.1  89/07/22  16:05:53  mrose
# bsd44
# 
# Revision 6.0  89/03/18  23:42:35  mrose
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

.SUFFIXES:	.ry .py .c .o

.ry.py:;	./xrosy $(RYFLAGS) $<


PEPYLIBES=	$(TOPDIR)psap/sprintoid.o \
		$(TOPDIR)psap/oid_cmp.o $(TOPDIR)psap/oid_cpy.o \
		$(TOPDIR)psap/oid_free.o \
		$(TOPDIR)compat/asprintf.o $(TOPDIR)compat/serror.o \
		$(TOPDIR)compat/sprintb.o $(TOPDIR)compat/sstr2arg.o
LIBES   =	$(TOPDIR)libpsap.a $(TOPDIR)libcompat.a
LLIBS   =	$(TOPDIR)llib-lpsap $(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)rosy.h $(HDIR)rosap.h $(HDIR)acsap.h $(HDIR)psap2.h \
		$(HDIR)psap.h $(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h
PEPSYFILES=	$(HDIR)pepsy.h $(TOPDIR)pepsy/xpepsy
RYHFILES=	rosy-defs.h $(HDIR)psap.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		rosy librosy
inst-all:	inst-rosy manuals # inst-librosy
install:	inst-all clean
lint:		l-rosy l-librosy


##################################################################
# rosy
##################################################################

inst-rosy:	$(BINDIR)rosy

$(BINDIR)rosy:	xrosy
		-cp $@ zxrosy
		-rm -f $@
		cp xrosy $@
		-@ls -gls $@
		-@echo ""

rosy:		xrosy

xrosy:		rosyvrsn.o $(PEPYLIBES)
		$(LDCC) $(LDFLAGS) -o $@ rosy.o yacc.o pepsy_misc.o \
			rosyvrsn.o $(PEPYLIBES) $(LSOCKET) -lm

rosy.o:		rosy.c $(RYHFILES)
		$(CC) $(CFLAGS) -c $*.c

pepsy_misc.o:	$(TOPDIR)pepsy/pepsy_misc.c
		$(CC) $(CFLAGS) -c $?

rosyvrsn.c:	rosy.o yacc.o pepsy_misc.o
		@$(UTILDIR)version.sh rosy > $@

yacc.o:		yacc.c lex.c $(RYHFILES)
		$(CC) $(CFLAGS) -c $*.c

yacc.c:		yacc.y
		-@echo "expect 20 shift/reduce and 9 reduce/reduce conflicts"
		yacc $(YACCFLAGS) yacc.y
		mv y.tab.c $@

yacc.y:		$(TOPDIR)pepy/yacc.y.gnrc
		$(UTILDIR)extract.sh ROSY < $? > $@

lex.c:		lex.l
		$(LEX) $(LEXFLAGS) lex.l
		mv lex.yy.c $@

lex.l:		$(TOPDIR)pepy/lex.l.gnrc
		$(UTILDIR)extract.sh ROSY < $? > $@

rosy-defs.h:	$(TOPDIR)pepsy/pepsy.h.gnrc
		$(UTILDIR)extract.sh ROSY < $? > $@

l-rosy:		yacc.c lex.c true
		$(LINT) $(LFLAGS) rosy.c yacc.c $(TOPDIR)pepsy/pepsy_misc.c \
			rosyvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# librosy
################################################################

CFILES	=	rystub.c rydiscard.c ryoperation.c ryopinvoke.c rygenid.c \
		rydispatch.c rydsresult.c rydserror.c rydsureject.c \
		rywait.c ryopblock.c rydsblock.c ryfind.c rylose.c

# pepsy based rosy library routines
P-OFILES=	p-rystub.o p-rydiscard.o p-ryoperation.o p-ryopinvoke.o \
		p-rygenid.o p-rydispatch.o p-rydsresult.o p-rydserror.o \
		p-rydsureject.o p-rywait.o p-ryopblock.o p-rydsblock.o \
		p-ryfind.o p-rylose.o

# (old) normal rosy library routines
N-OFILES=	rystub.o rydiscard.o ryoperation.o ryopinvoke.o \
		rygenid.o rydispatch.o rydsresult.o rydserror.o \
		rydsureject.o rywait.o ryopblock.o rydsblock.o \
		ryfind.o rylose.o

OFILES=		$(P-OFILES) $(N-OFILES) $(OSTRINGS)

inst-librosy:	$(LIBDIR)librosy.a $(LINTDIR)llib-lrosy

$(LIBDIR)librosy.a:	librosy.a
		-rm -f $@
		cp librosy.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lrosy:	llib-lrosy
		-cp $@ zllib-lrosy
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lrosy | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

librosy:	librosy.a

librosy.a:	$(OFILES)
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES)
		-@rm -f $(TOPDIR)librosy.a $(TOPDIR)llib-lrosy
		-@$(LN) librosy.a $(TOPDIR)librosy.a
		-@$(LN) llib-lrosy $(TOPDIR)llib-lrosy
		-@ls -l $@
		-@echo "ROSY library built normally"

l-librosy:	$(CFILES) true
		$(LINT) $(LFLAGS) $(CFILES) $(TOPDIR)llib-lrosap $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

p-rydiscard.o:	$(HFILES) $(PEPSYFILES) rydiscard.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rydiscard.c 

p-rygenid.o:	$(HFILES) $(PEPSYFILES) rygenid.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rygenid.c

p-rystub.o:	$(HFILES) $(PEPSYFILES) rystub.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rystub.c

p-rywait.o:	$(HFILES) $(PEPSYFILES) rywait.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rywait.c

p-ryoperation.o: $(HFILES) $(PEPSYFILES) ryoperation.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c ryoperation.c

p-ryopinvoke.o: $(HFILES) $(PEPSYFILES) ryopinvoke.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c ryopinvoke.c

p-rydispatch.o:	$(HFILES) $(PEPSYFILES) rydispatch.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rydispatch.c

p-rydsresult.o:	$(HFILES) $(PEPSYFILES) rydsresult.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rydsresult.c

p-rydserror.o:	$(HFILES) $(PEPSYFILES) rydserror.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rydserror.c

p-rydsureject.o: $(HFILES) $(PEPSYFILES) rydsureject.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rydsureject.c

p-ryopblock.o:	$(HFILES) $(PEPSYFILES) ryopblock.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c ryopblock.c

p-rydsblock.o:	$(HFILES) $(PEPSYFILES) rydsblock.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rydsblock.c

p-ryfind.o:	$(HFILES) $(PEPSYFILES) ryfind.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c ryfind.c

p-rylose.o:	$(HFILES) $(PEPSYFILES) rylose.c
		$(CC) $(LIBCFLAGS) -DPEPSY_VERSION=2 -o $@ -c rylose.c


rystub.o:	$(HFILES)
rydiscard.o:	$(HFILES)
ryoperation.o:	$(HFILES)
ryopinvoke.o:	$(HFILES)
rygenid.o:	$(HFILES)
rydispatch.o:	$(HFILES)
rydsresult.o:	$(HFILES)
rydserror.o:	$(HFILES)
rydsureject.o:	$(HFILES)
rywait.o:	$(HFILES)
ryopblock.o:	$(HFILES)
rydsblock.o:	$(HFILES)
ryfind.o:	$(HFILES)
rylose.o:	$(HFILES)

true:;


################################################################
# manual pages
################################################################

MANUALS	=	rosy.1 # librosy.3n

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a x* z* _* core rosy-defs.h yacc.y yacc.c lex.l \
			lex.c rosyvrsn.c

grind:;		iprint Makefile
		tgrind -lc rosy-defs.h rosy.c rosyvrsn.c
		tgrind -ly yacc.y lex.l
		tgrind -lc $(CFILES) llib-lrosy
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve
