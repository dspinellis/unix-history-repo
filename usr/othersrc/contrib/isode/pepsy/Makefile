###############################################################################
#   Instructions to Make, for compilation of ISODE PEPSY process
###############################################################################

###############################################################################
#
# $Header: /f/osi/pepsy/RCS/Makefile,v 7.12 91/02/22 09:48:38 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.12  91/02/22  09:48:38  mrose
# Interim 6.8
# 
# Revision 7.11  91/01/09  13:51:43  mrose
# foo
# 
# Revision 7.10  91/01/08  12:49:17  mrose
# update
# 
# Revision 7.9  90/12/23  18:42:01  mrose
# update
# 
# Revision 7.8  90/12/11  10:33:05  mrose
# sync
# 
# Revision 7.7  90/11/20  15:27:02  mrose
# update
# 
# Revision 7.6  90/11/11  10:53:31  mrose
# update
# 
# Revision 7.5  90/11/04  19:17:04  mrose
# update
# 
# Revision 7.4  90/09/07  17:34:18  mrose
# touch-up
# 
# Revision 7.3  90/08/08  14:14:12  mrose
# update
# 
# Revision 7.2  90/07/27  08:49:06  mrose
# update
# 
# Revision 7.1  90/07/09  14:52:16  mrose
# sync
# 
# Revision 7.0  90/07/01  19:54:10  mrose
# *** empty log message ***
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


PEPYPATH=	-DPEPYPATH -DPEPSYPATH=\".:$(PEPSYDIRM)\" 

# to prevent a lot of unnecessary re-loading
PEPSYLIBES=	$(TOPDIR)psap/sprintoid.o $(TOPDIR)psap/pl_tables.o \
		$(TOPDIR)psap/oid_cmp.o $(TOPDIR)psap/oid_cpy.o \
		$(TOPDIR)psap/oid_free.o \
		$(TOPDIR)compat/asprintf.o $(TOPDIR)compat/serror.o \
		$(TOPDIR)compat/sprintb.o
LIBES   =	$(TOPDIR)libpsap.a $(TOPDIR)libcompat.a
LLIBS   =	$(TOPDIR)llib-lpsap $(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)psap.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h

DRFILES	=	enc.o dec.o fre.o prnt.o util.o
SUPFILES=	vprint.o UNIV_tables.o pepsy_strings.o py_advise.o
LIBFILES=	$(DRFILES) $(SUPFILES)

CFILES	=	enc.c dec.c fre.c prnt.c main.c util.c  \
		vprint.c pepsy_strings.c py_advise.c

HFILES	=	$(TOPDIR)h/pepsy.h test_table.h
HEADERS	=	UNIV.ph UNIV-types.h UNIV_defs.h UNIV_pre_defs.h

OFILES	=	$(LIBFILES) main.o

T_FILES =	T1_tables.c T2_tables.c T3_tables.c


##################################################################
# Here it is...
##################################################################

all:		pepsy libpepsy # peptest
inst-all:	inst-pepsy inst-headers manuals # inst-libpepsy
install:	inst-all clean
lint:		l-pepsy l-libpepsy


##################################################################
# pepsy
##################################################################

inst-pepsy:	$(BINDIR)pepsy

$(BINDIR)pepsy:	xpepsy
		-cp $@ zxpepsy
		-rm -f $@
		cp xpepsy $@
		-@ls -gls $@
		-@echo ""

pepsy:		xpepsy

xpepsy:		pepsyvrsn.o $(PEPSYLIBES)
		$(LDCC) $(LDFLAGS) -o $@ pepsy.o pepsy-yacc.o pepsy_misc.o \
			pass2.o etabs.o dtabs.o ptabs.o dfns.o mod.o \
			pepsyvrsn.o $(PEPSYLIBES) $(LSOCKET) -lm

pepsy.o:	pepsy.c pepsydefs.h $(HFILES)
		$(CC) $(CFLAGS) -c pepsy.c

pepsyvrsn.c:	pepsy.o pepsy-yacc.o pepsy_misc.o pass2.o \
			etabs.o dtabs.o ptabs.o dfns.o mod.o 
		$(UTILDIR)version.sh pepsy > $@

pepsy-yacc.o:	pepsy-yacc.c lex.c
		$(CC) $(CFLAGS) -c pepsy-yacc.c

pepsy-yacc.c:	pepsy-yacc.y
		-@echo "expect 40 shift/reduce and 11 reduce/reduce conflicts"
		yacc $(YACCFLAGS) pepsy-yacc.y
		mv y.tab.c $@

pepsy-yacc.y:	yacc.y.gnrc
		$(UTILDIR)extract.sh PEPSY SCTRL < $? > $@

pepsy_misc.o:	pepsy_misc.c pepsydefs.h
		$(CC) $(CFLAGS) -c pepsy_misc.c

lex.c:		lex.l
		$(LEX) $(LEXFLAGS) lex.l
		mv lex.yy.c $@

lex.l:		lex.l.gnrc
		$(UTILDIR)extract.sh PEPSY < $? > $@

pepsydefs.h:	pepsydefs.h.gnrc
		$(UTILDIR)extract.sh PEPSY < $? > $@

pass2.o:	pass2.c
		$(CC) $(CFLAGS) -c pass2.c
etabs.o:	etabs.c
		$(CC) $(CFLAGS) -c etabs.c
dtabs.o:	dtabs.c
		$(CC) $(CFLAGS) -c dtabs.c
ptabs.o:	ptabs.c
		$(CC) $(CFLAGS) -c ptabs.c
dfns.o:		dfns.c
		$(CC) $(CFLAGS) -c dfns.c
mod.o:		mod.c
		$(CC) $(CFLAGS) -c mod.c


l-pepsy:	pepsy.c pepsy_misc.c pepsydefs.h pepsyvrsn.c true
		$(LINT) $(LFLAGS) -DPEPSYPATH=\".:$(PEPSYDIRM)\" pepsy.c \
			pepsy_misc.c  pepsyvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# libpepsy
################################################################

inst-libpepsy:	$(LIBDIR)libpepsy.a inst-headers

libpepsy:	libpepsy.a true
		-@rm -f $(TOPDIR)h/UNIV*.h
		-@$(LN) UNIV-types.h $(TOPDIR)h/UNIV-types.h
		-@$(LN) UNIV_defs.h $(TOPDIR)h/UNIV_defs.h
		-@$(LN) UNIV_pre_defs.h $(TOPDIR)h/UNIV_pre_defs.h

libpepsy.a:     $(LIBFILES)
	        -rm -f $@
	        @$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(LIBFILES)
	        -@rm -f $(TOPDIR)libpepsy.a
	        -@$(LN) libpepsy.a $(TOPDIR)libpepsy.a
	        -@ls -l $@
	        -@echo "PEPSY library built normally"

inst-headers:;	-mkdir $(PEPSYDIRM)
		@for h in $(HEADERS); do $(MAKE) TARGET=$$h inst-target; done

inst-target:	$(PEPSYDIR)$(TARGET)

$(PEPSYDIR)$(TARGET):	$(TARGET)
		-cp $@ z$(TARGET)
		cp $(TARGET) $@
		-@ls -gls $@
		-@echo ""

UNIV_tables.o:	UNIV_tables.c UNIV-types.h

UNIV-types.h UNIV_tables.c: UNIV.py xpepsy
		./xpepsy -A -f -h -m UNIV.py

l-libpepsy:	$(CFILES) $(T_FILES) true
		$(LINT) $(LFLAGS) $(CFILES) $(T_FILES) \
			$(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

vprint.o:	UNIV-types.h $(HDIR)psap.h $(HDIR)general.h $(HDIR)manifest.h \
		$(HDIR)logger.h


##################################################################
# pep
##################################################################
alltests:	tabletest peptest

peptest:	pep.results pep.output
		-diff pep.output pep.results
		touch peptest

pep.results:	pep
		./pep > pep.results

tabletest:	xpepsy t1test t2test t3test psap2test acsaptest ftamtest 

psap2test:	../psap2/PS_tables.c ../psap2/PS-types.h
	./xpepsy -A -f -h -m ../psap2/ps.py
	-diff ../psap2/PS_tables.c . | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-diff ../psap2/PS-types.h . | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-rm -f PS_tables.c PS-types.h 

acsaptest:	../acsap/ACS_tables.c ../acsap/ACS-types.h
	./xpepsy -A -f -h -m ../acsap/acs.py
	-diff ../acsap/ACS_tables.c . | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-diff ../acsap/ACS-types.h . | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-rm -f ACS_tables.c ACS-types.h 

ftamtest:	../ftam/FTAM_tables.c ../ftam/FTAM-types.h
	./xpepsy -A -f -h -m ../ftam/ftam.py
	-diff ../ftam/FTAM_tables.c . | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-diff ../ftam/FTAM-types.h . | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-rm -f FTAM_tables.c FTAM-types.h 

fadutest:	../ftam/FADU_tables.c ../ftam/FADU-types.h
	./xpepsy -A -f -h -m ../ftam/fadu.py
	-diff ../ftam/FADU_tables.c . | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-diff ../ftam/FADU-types.h . | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-rm -f FADU_tables.c FADU-types.h 

t1test:	T1_tables.c T1-types.h
	mv T1_tables.c T1_tables.c_orig
	mv T1-types.h T1-types.h_orig
	./xpepsy -A -f -m t1.py
	-diff T1_tables.c_orig T1_tables.c | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-diff T1-types.h_orig T1-types.h | grep -v "^[><] # line" \
		| grep -v "do not edit"

t2test:	T2_tables.c T2-types.h
	mv T2_tables.c T2_tables.c_orig
	mv T2-types.h T2-types.h_orig
	./xpepsy -A -f -m t2.py
	-diff T2_tables.c_orig T2_tables.c | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-diff T2-types.h_orig T2-types.h | grep -v "^[><] # line" \
		| grep -v "do not edit"

t3test:	T3_tables.c T3-types.h
	mv T3_tables.c T3_tables.c_orig
	mv T3-types.h T3-types.h_orig
	./xpepsy -A -f -m -i pepsy-refs.h t3.py
	-diff T3_tables.c_orig T3_tables.c | grep -v "^[><] # line" \
		| grep -v "do not edit"
	-diff T3-types.h_orig T3-types.h | grep -v "^[><] # line" \
		| grep -v "do not edit"

PEPLIBS	=	../libpsap.a ../libcompat.a
PEPFILES=	T1_tables.o T2_tables.o T3_tables.o $(OFILES) $(PEPLIBS) # malloctrace.a

pep:		$(PEPFILES)
		#$(LDCC) $(LDFLAGS) -o pep $(PEPFILES) malloctrace.a $(LSOCKET)
		$(LDCC) $(LDFLAGS) -o pep $(PEPFILES) $(LSOCKET) -lm

pep-saber:	$(PEPFILES)
		#load $(CFLAGS) (PEPFILES)

main.o: 	T1-types.h test_table.h main.c
		$(CC) $(CFLAGS) -DPRNT=1 -c main.c

enc.o:		enc.c pepsy-driver.h
		$(CC) $(LIBCFLAGS) -c enc.c

dec.o:		dec.c pepsy-driver.h
		$(CC) $(LIBCFLAGS) -c dec.c

prnt.o: 	prnt.c pepsy-driver.h
		$(CC) $(LIBCFLAGS) -c prnt.c

util.o: 	util.c pepsy-driver.h
		$(CC) $(LIBCFLAGS) -c util.c

T1_tables.o:	T1_tables.c T1-types.h UNIV-types.h

T1_tables.c T1-types.h:	t1.py xpepsy
		./xpepsy -A -f -m t1.py

T1_tables.c:	T2-types.h T3-types.h

T2_tables.o:	T2_tables.c T2-types.h UNIV-types.h

T2_tables.c T2-types.h:	t2.py xpepsy
		./xpepsy -A -f -m t2.py

T3_tables.o:	T3_tables.c T3-types.h UNIV-types.h

T3_tables.c T3-types.h:	t3.py xpepsy
		./xpepsy -A -f -m -i pepy-refs.h t3.py

prt.o:		prnt.c $(TOPDIR)h/pepsy.h
		$(CC) $(CFLAGS) -Dprintf=vprint -o $@ prnt.c


################################################################
# manual pages
################################################################

MANUALS	=	pepsy.1 # libpepsy.3

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a x* z* _* core pepsydefs.h *yacc.y *yacc.c lex.l \
			lex.c *.ph UNIV_* UNIV-* _* core $(TOPDIR)h/UNIV*.h \
			pp.c pp pep libpepsy.a pepsy-yacc.y T1* T2* T3* t? \
			peptest pep.results

grind:;		iprint Makefile
		tgrind -lc pepsydefs.h pepsy.c pepsy.c pepsyvrsn.c
		tgrind -ly yacc.y lex.l
		tgrind -lpepsy -d $(TOPDIR)pepy/grindefs UNIV.py
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
