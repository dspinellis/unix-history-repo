CFLAGS = -g
PRINT = lpr -p

ALLC = apilib.c api_bsd.c api_exch.c
ALLH = apilib.h api_exch.h

ALLPRINT = ${ALLH} ${ALLC}

ALLSOURCE = ${ALLPRINT}

ALLO = apilib.o api_bsd.o api_exch.o

apilib.a: ${ALLO}
	@-/bin/rm $@
	ar cr $@ ${ALLO}
	ranlib $@

clean:
	-/bin/rm ${ALLO} errs apilib.a

action:
	${ACTION}

test:	apilib.a test.o
	${CC} ${CFLAGS} -o $@ test.o apilib.a

print:
	${PRINT} ${ALLPRINT}


sourcelist:
	for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}/$$i); done

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
