###############################################################################
#   Instructions to Make, for compilation of ISODE PEPY processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/pepy/RCS/Makefile,v 7.8 91/02/22 09:34:41 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.8  91/02/22  09:34:41  mrose
# Interim 6.8
# 
# Revision 7.7  90/12/23  18:42:22  mrose
# update
# 
# Revision 7.6  90/11/04  19:15:59  mrose
# update
# 
# Revision 7.5  90/08/08  14:15:10  mrose
# update
# 
# Revision 7.4  90/07/27  08:47:22  mrose
# update
# 
# Revision 7.3  90/07/09  14:43:06  mrose
# sync
# 
# Revision 7.2  90/07/01  21:04:51  mrose
# pepsy
# 
# Revision 7.1  90/01/11  18:37:02  mrose
# real-sync
# 
# Revision 7.0  89/11/23  22:11:30  mrose
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


PEPYPATH=	-DPEPYPATH=\".:$(PEPYDIRM)\" 

.SUFFIXES:	.py .c .o

.py.c:;		./xpepy -a PY_advise -m $(PYFLAGS) $<


# to prevent a lot of unnecessary re-loading
PEPYLIBES=	$(TOPDIR)psap/sprintoid.o $(TOPDIR)psap/pl_tables.o \
		$(TOPDIR)psap/oid_cmp.o $(TOPDIR)psap/oid_cpy.o \
		$(TOPDIR)psap/oid_free.o \
		$(TOPDIR)compat/asprintf.o $(TOPDIR)compat/serror.o \
		$(TOPDIR)compat/sprintb.o
LIBES   =	$(TOPDIR)libpepsy.a $(TOPDIR)libpsap.a $(TOPDIR)libcompat.a
LLIBS   =	$(TOPDIR)llib-lpsap $(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)psap.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		pepy posy libpepy pepytest pp
inst-all:	inst-pepy inst-posy inst-headers manuals # inst-libpepy
install:	inst-all clean
tests:		pepytest pp mpp salary hello_world bigpepytest true
		-@for f in test/test*.pe; \
			do (echo pepytest $$f; pepytest < $$f); done
		-@for f in test/test*.pe test/mhs/msg*.pe; \
			do (echo pp $$f; pp < $$f); done
		-@for f in test/mhs/msg*.pe; \
			do (echo mpp $$f; mpp < $$f); done
		-salary fritz 10
		-hello_world
		-bigpepytest
lint:		l-pepy l-posy l-libpepy l-pepytest l-pp l-mpp l-salary \
		l-hello_world l-bigpepytest


##################################################################
# pepy
##################################################################

inst-pepy:	$(BINDIR)pepy

$(BINDIR)pepy:	xpepy
		-cp $@ zxpepy
		-rm -f $@
		cp xpepy $@
		-@ls -gls $@
		-@echo ""

pepy:		xpepy

xpepy:		pepyvrsn.o $(PEPYLIBES)
		$(LDCC) $(LDFLAGS) -o $@ pepy.o pepy_do.o pepy_undo.o \
			yacc.o pepy_misc.o pepyvrsn.o $(PEPYLIBES) \
			$(LSOCKET) -lm

pepy.o:		pepy.c pepy.h $(HFILES)
		$(CC) $(CFLAGS) -c pepy.c

pepyvrsn.c:	pepy.o yacc.o pepy_do.o pepy_undo.o pepy_misc.o
		@$(UTILDIR)version.sh pepy > $@

pepy_do.o:	pepy_do.c pepy.h
		$(CC) $(CFLAGS) -c pepy_do.c

pepy_undo.o:	pepy_undo.c pepy.h
		$(CC) $(CFLAGS) -c pepy_undo.c

pepy_misc.o:	pepy_misc.c pepy.h
		$(CC) $(CFLAGS) -c pepy_misc.c

yacc.o:		yacc.c lex.c pepy.h $(HFILES)
		$(CC) $(CFLAGS) -c $*.c

yacc.c:		yacc.y
		-@echo "expect 20 shift/reduce and 9 reduce/reduce conflicts"
		yacc $(YACCFLAGS) yacc.y
		mv y.tab.c $@

yacc.y:		yacc.y.gnrc
		$(UTILDIR)extract.sh PEPY < $? > $@

lex.c:		lex.l
		$(LEX) $(LEXFLAGS) lex.l
		mv lex.yy.c $@

lex.l:		lex.l.gnrc
		$(UTILDIR)extract.sh PEPY < $? > $@

pepy.h:		$(TOPDIR)pepsy/pepsy.h.gnrc
		$(UTILDIR)extract.sh PEPY < $? > $@

l-pepy:		pepy.h yacc.c lex.c pepyvrsn.c true
		$(LINT) $(LFLAGS) pepy.c yacc.c pepy_do.c pepy_undo.c \
			pepy_misc.c  pepyvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


##################################################################
# posy
##################################################################

inst-posy:	$(BINDIR)posy

$(BINDIR)posy:	xposy
		-cp $@ zxposy
		-rm -f $@
		cp xposy $@
		-@ls -gls $@
		-@echo ""

posy:		xposy

xposy:		posy.o pepy_misc.o yacc.o pepyvrsn.o $(PEPYLIBES)
		$(LDCC) $(LDFLAGS) -o $@ posy.o pepy_misc.o yacc.o \
			pepyvrsn.o $(PEPYLIBES) $(LSOCKET) -lm

posy.o:		posy.c pepy.h $(HFILES)
		$(CC) $(CFLAGS) -c $*.c

l-posy:		yacc.c lex.c true
		$(LINT) $(LFLAGS) posy.c yacc.c pepy_misc.c \
			pepyvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# libpepy
################################################################

CFILES	=	py_pp.c testdebug.c
OFILES	=	py_pp.o testdebug.o
HEADERS	=	UNIV.ph UNIV-types.h

inst-libpepy:	$(LIBDIR)libpepy.a inst-headers

$(LIBDIR)libpepy.a:	libpepy.a
		-rm -f $@
		cp libpepy.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

inst-headers:;	-mkdir $(PEPYDIRM)
		@for h in $(HEADERS); do $(MAKE) TARGET=$$h inst-target; done

inst-target:	$(PEPYDIR)$(TARGET)

$(PEPYDIR)$(TARGET):	$(TARGET)
		-cp $@ z$(TARGET)
		cp $(TARGET) $@
		-@ls -gls $@
		-@echo ""

libpepy:	libpepy.a

libpepy.a:	UNIV-O $(OFILES)
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			$(UNIV-O)
		-@rm -f $(TOPDIR)libpepy.a
		-@$(LN) libpepy.a $(TOPDIR)libpepy.a
		-@ls -l $@
		-@echo "PEPY library built normally"

UNIV-O	=	UPEPY-[0-9]*.o VPEPY-[0-9]*.o
UNIV-C	=	UPEPY-[0-9]*.c VPEPY-[0-9]*.c

UNIV-O:		UPEPY-C VPEPY-C
		@$(MAKE) `/bin/ls $(UPEPY-C) $(VPEPY-C) | sed 's/\.c$$/.o/'`
		-@touch $@

UNIV-C:		UPEPY-C VPEPY-C

UPEPY-O =	UPEPY-[0-9]*.o
UPEPY-C =	UPEPY-[0-9]*.c

UPEPY-C:	$(TOPDIR)pepsy/UNIV.py xpepy
		-@rm -f $(UPEPY-C) $(UPEPY-O)
		./xpepy -a PY_advise -m -A -b UPEPY $(TOPDIR)pepsy/UNIV.py
		-@touch $@

VPEPY-O	=	VPEPY-[0-9]*.o
VPEPY-C	=	VPEPY-[0-9]*.c

VPEPY-C:	UNIV-types.py xpepy
		-@rm -f $(VPEPY-C) $(VPEPY-O)
		./xpepy -a PY_advise -m -b VPEPY UNIV-types.py
		-@touch $@

UNIV-types.h UNIV-types.py: $(TOPDIR)pepsy/UNIV.py xposy
		./xposy -f -h -m -o UNIV-types.py $(TOPDIR)pepsy/UNIV.py

l-libpepy:	$(CFILES) UNIV-C true
		$(LINT) $(LFLAGS) $(CFILES) $(UNIV-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

testdebug.c:	testdebug.py xpepy


py_pp.o:	py_pp.c $(HDIR)psap.h $(HDIR)general.h $(HDIR)manifest.h

##################################################################
# pepytest
##################################################################

pepytest:	pepytest.o libpepy.a $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ pepytest.o libpepy.a $(LIBES) \
			$(LSOCKET)

pepytest.o:	pepytest.c $(HFILES)

pepytest.c:	pepytest.py xpepy

l-pepytest:	pepytest.c $(CFILES) UNIV-C true
		$(LINT) $(LFLAGS) pepytest.c $(CFILES) $(UNIV-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


##################################################################
# pp
##################################################################

pp:		pp.o libpepy.a $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ pp.o libpepy.a $(LIBES) $(LSOCKET)

pp.o:		pp.c $(HFILES)

pp.c:		pp.py xpepy

l-pp:		pp.c $(CFILES) UNIV-C true
		$(LINT) $(LFLAGS) pp.c $(CFILES) $(UNIV-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


##################################################################
# tests
##################################################################

mpp:		mpp.o P1.o P2.o P3.o SFD.o T73.o EAN.o libpepy.a $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ mpp.o P1.o P2.o P3.o SFD.o T73.o \
			EAN.o libpepy.a $(LIBES) $(LSOCKET)

mpp.o:		mpp.c $(HFILES)
mpp.c:		mpp.py xpepy

EAN.o:		EAN.c $(HFILES)
EAN.c:		EAN.py xpepy

P1.o:		P1.c $(HFILES)
P1.c:		P1.py xpepy

P2.o:		P2.c $(HFILES)
P2.c:		P2.py xpepy

P3.o:		P3.c $(HFILES)
P3.c:		P3.py xpepy

SFD.o:		SFD.c $(HFILES)
SFD.c:		SFD.py xpepy

T73.o:		T73.c $(HFILES)
T73.c:		T73.py xpepy

l-mpp:		mpp.c P1.c P2.c P3.c SFD.c T73.c EAN.c $(CFILES) UNIV-C true
		$(LINT) $(LFLAGS) mpp.c P1.c P2.c P3.c SFD.c T73.c EAN.c \
			$(CFILES) $(UNIV-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


salary:		salary.o
		$(LDCC) $(LDFLAGS) -o $@ salary.o libpepy.a $(LIBES) \
			$(LSOCKET)

salary.c:	salary.py xpepy

l-salary:	salary.c $(CFILES) UNIV-C true
		$(LINT) $(LFLAGS) salary.c $(CFILES) $(UNIV-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


hello_world:	hello_world.o
		$(LDCC) $(LDFLAGS) -o $@ hello_world.o libpepy.a \
			$(LIBES) $(LSOCKET)

hello_world.c:	hello_world.py xpepy

l-hello_world:	hello_world.c $(CFILES) UNIV-C true
		$(LINT) $(LFLAGS) hello_world.c $(CFILES) $(UNIV-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


bigpepytest:	bigpepytest.o
		$(LDCC) $(LDFLAGS) -o $@ bigpepytest.o libpepy.a $(LIBES) \
			$(LSOCKET)

bigpepytest.c:  bigpepytest.py xpepy

l-bigpepytest:	bigpepytest.c $(CFILES) UNIV-C true
		$(LINT) $(LFLAGS) bigpepytest.c $(CFILES) $(UNIV-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# manual pages
################################################################

MANUALS	=	pepy.1 posy.1 # libpepy.3

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a x* z* _* core pepy.h *yacc.y *yacc.c lex.l \
			lex.c *.ph testdebug.c UNIV-* UPEPY-* VPEPY-* \
			pepytest.c pepytest pp.c pp mpp mpp.c P1.c P2.c \
			P3.c SFD.c T73.c EAN.c pepyvrsn.c salary.c salary \
			hello_world.c hello_world bigpepytest.c bigpepytest

grind:;		iprint Makefile
		tgrind -lc pepy.h pepy.c posy.c pepyvrsn.c
		tgrind -ly yacc.y lex.l
		tgrind -lpepy -d grindefs pepytest.py \
			mpp.py P1.py P2.py P3.py SFD.py T73.py EAN.py \
			salary.py hello_world.py bigpepytest.py
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
