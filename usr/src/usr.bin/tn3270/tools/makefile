CC	= cc
PRINT	= print

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = -g

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

ALLC = dohits.c ectype.c mkastosc.c mkdctype.c mkdstoas.c mkhits.c prt3270.c

ALLH = dohits.h ectype.h

ALLO =	prt3270.o mkdctype.o ectype.o mkdstoas.o mkhits.o dohits.o mkastosc.o

ALLEXE = prt3270 mkdctype ectype mkdstoas mkhits mkastosc

tools:
	@echo Need to specify WHICH tool ...

all:	mkastosc mkhits mkdstoas mkdctype prt3270
	@echo done.

prt3270:	prt3270.o ../system/globals.o ../ascii/ebctab.o
	${CC} ${CFLAGS} -o prt3270 prt3270.o ../system/globals.o \
							../ascii/ebctab.o

mkastosc:	mkastosc.o dohits.o ../ascii/asctab.o ../ctlr/ebc_disp.o
	$(CC) $(CFLAGS) -o mkastosc mkastosc.o dohits.o \
			../ascii/asctab.o ../ctlr/ebc_disp.o

mkdstoas:	mkdstoas.o ../ascii/ebctab.o ../ctlr/ebc_disp.o
	$(CC) $(CFLAGS) -o mkdstoas mkdstoas.o ../ascii/ebctab.o \
						../ctlr/ebc_disp.o

mkhits:	mkhits.o dohits.o ../ascii/asctab.o ../ctlr/ebc_disp.o
	$(CC) $(CFLAGS) -o mkhits mkhits.o dohits.o ../ascii/asctab.o \
						../ctlr/ebc_disp.o

mkdctype:	mkdctype.o ../ctlr/ebc_disp.o ectype.o
	$(CC) $(CFLAGS) -o mkdctype mkdctype.o ../ctlr/ebc_disp.o ectype.o

print:
	${PRINT} ${ALLH} ${ALLC}

tags:	${ALLC} ${ALLH}
	ctags -t ${ALLC} ${ALLH}

action:
	${ACTION}

clean:
	-rm errs makefile.bak ${ALLO} ${ALLEXE}

lint:
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 \
				${TNMAIN} ${MOSTC} -lcurses
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${MSMAIN} map3270.c -lcurses

.DEFAULT:
	sccs get $<

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

dohits.o: ../ascii/ascebc.h ../ctlr/ebc_disp.h ../ctlr/function.h dohits.h
ectype.o: ectype.h
mkastosc.o: ../keyboard/m4.out ../ascii/ascebc.h ../ctlr/ebc_disp.h
mkastosc.o: ../ctlr/function.h dohits.h
mkdctype.o: ../ctlr/ebc_disp.h ectype.h
mkdstoas.o: ../ascii/ascebc.h ../ctlr/ebc_disp.h
mkhits.o: ../ascii/ascebc.h ../ctlr/ebc_disp.h ../ctlr/function.h dohits.h
prt3270.o: ../ascii/ascebc.h ../ctlr/hostctlr.h ../ctlr/screen.h
prt3270.o: ../keyboard/m4.out ../system/globals.h
