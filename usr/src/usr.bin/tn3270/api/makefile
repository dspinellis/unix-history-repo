CFLAGS = -g

PRINT = lpr -p

KBD	= 3270pc.kbd
KBD	= unix.kbd

ALLC = apilib.c api_bsd.c api_exch.c asc_ebc.c astosc.c dctype.c ebc_disp.c

ALLH = apilib.h api_exch.h asc_ebc.h astosc.h disp_asc.h dctype.h ebc_disp.h

ALLPRINT = ${ALLH} ${ALLC}

ALLSOURCE = ${ALLPRINT} makefile makefile.mak

ALLO = apilib.o api_bsd.o api_exch.o astosc.o asc_ebc.o disp_asc.o \
		dctype.o ebc_disp.o

apilib.a: ${ALLO}
	@-/bin/rm -f $@
	ar cr $@ ${ALLO}
	ranlib $@

.DEFAULT:
	sccs get $<

clean:
	-/bin/rm -f ${ALLO} errs apilib.a makefile.bak \
		disp_out asc_disp.out astosc.out disp_asc.out \
		test* test.o t1* t1.o t2* t2.o
	-sccs clean
	-sccs get makefile

action:
	${ACTION}

test:	apilib.a test.o
	${CC} ${CFLAGS} -o $@ test.o apilib.a

t1:	apilib.a t1.o
	${CC} ${CFLAGS} -o $@ t1.o apilib.a

t2:	apilib.a t2.o
	${CC} ${CFLAGS} -o $@ t2.o apilib.a

print:
	${PRINT} ${ALLPRINT}


sourcelist:	${ALLSOURCE}
	@for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}$$i); done

astosc.out:	../ctlr/function.h ../ctlr/hostctlr.h ../ctlr/$(KBD)
	(cd ../tools; make mkastosc $(MFLAGS) )
	-/bin/rm -f $@
	../tools/mkastosc < ../ctlr/$(KBD) > $@

asc_disp.out:	ebc_disp.o
	(cd ../tools; make mkastods $(MFLAGS) )
	-/bin/rm -f $@
	../tools/mkastods > $@

disp_asc.out:	ebc_disp.o
	(cd ../tools; make mkdstoas $(MFLAGS) )
	-/bin/rm -f $@
	../tools/mkdstoas > $@

disp_asc.o:	disp_asc.h asc_disp.out disp_asc.out
	-/bin/rm -f $@ TMPfunc.c
	/bin/echo \#include \"disp_asc.h\" > TMPfunc.c
	/bin/echo \#include \"asc_disp.out\" >> TMPfunc.c
	/bin/echo \#include \"disp_asc.out\" >> TMPfunc.c
	$(CC) $(CFLAGS) -c TMPfunc.c
	-/bin/rm -f TMPfunc.c
	-/bin/mv TMPfunc.o $@

depend:
	grep '^#include' ${ALLC} | grep -v '<' | \
	sed -e 's/:[^"]*"\([^"]*\)".*/: \1/' \
	    -e 's/\.c/.o/' | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '$$r makedep' >>eddep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp makefile makefile.bak
	ed - makefile < eddep
	rm eddep makedep

# DO NOT DELETE THIS LINE

apilib.o: ../api/api.h apilib.h
api_bsd.o: ../api/api.h api_exch.h
api_exch.o: api_exch.h
asc_ebc.o: asc_ebc.h
astosc.o: ../general/general.h ../ctlr/function.h astosc.h astosc.out
dctype.o: dctype.h
