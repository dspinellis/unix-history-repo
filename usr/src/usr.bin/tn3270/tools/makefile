CC	= cc
PRINT	= print

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = -g

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

ALLH = dohits.h ectype.h

ALLC = dohits.c ectype.c mkastods.c mkastosc.c mkdctype.c \
		mkdstoas.c mkhits.c prt3270.c

ALLO =	prt3270.o mkdctype.o ectype.o \
		mkastods.o mkdstoas.o mkhits.o dohits.o mkastosc.o

ALLPRINT =	${ALLH} ${ALLC}

ALLSOURCE =	${ALLPRINT} makefile makefile.mak

ALLEXE	=	mkastods mkastosc mkdctype mkdstoas mkhits prt3270

tools:
	@echo Need to specify WHICH tool ...

all:	${ALLEXE}
	@echo done.

prt3270:	prt3270.o ../general/globals.o ../apilib/asc_ebc.o \
				../apilib/astosc.o ../ctlr/kbd.out
	${CC} ${CFLAGS} -o prt3270 prt3270.o ../general/globals.o \
					../apilib/asc_ebc.o ../apilib/astosc.o

mkastosc:	mkastosc.o dohits.o ../apilib/asc_ebc.o ../apilib/ebc_disp.o
	$(CC) $(CFLAGS) -o mkastosc mkastosc.o dohits.o \
			../apilib/asc_ebc.o ../apilib/ebc_disp.o

mkastods:	mkastods.o ../apilib/asc_ebc.o ../apilib/ebc_disp.o
	$(CC) $(CFLAGS) -o mkastods mkastods.o ../apilib/asc_ebc.o \
						../apilib/ebc_disp.o

mkdstoas:	mkdstoas.o ../apilib/asc_ebc.o ../apilib/ebc_disp.o
	$(CC) $(CFLAGS) -o mkdstoas mkdstoas.o ../apilib/asc_ebc.o \
						../apilib/ebc_disp.o

mkhits:	mkhits.o dohits.o ../apilib/asc_ebc.o ../apilib/ebc_disp.o
	$(CC) $(CFLAGS) -o mkhits mkhits.o dohits.o ../apilib/asc_ebc.o \
						../apilib/ebc_disp.o

mkdctype:	mkdctype.o ../apilib/ebc_disp.o ectype.o
	$(CC) $(CFLAGS) -o mkdctype mkdctype.o ../apilib/ebc_disp.o ectype.o

../apilib/astosc.o:
	(cd ../apilib; make astosc.o)

../apilib/asc_ebc.o:
	(cd ../apilib; make asc_ebc.o)

../ctlr/kbd.out:
	(cd ../ctlr; make kbd.out)

../apilib/ebc_disp.o:
	(cd ../apilib; make ebc_disp.o)

../general/globals.o:
	(cd ../general; make globals.o)

sourcelist:	${ALLSOURCE}
	@for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}$$i); done

print:
	${PRINT} ${ALLPRINT}

tags:	${ALLC} ${ALLH}
	ctags -t ${ALLC} ${ALLH}

action:
	${ACTION}

clean:
	-rm -f errs makefile.bak ${ALLO} ${ALLEXE}
	-sccs clean
	-sccs get makefile

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

dohits.o: ../general/general.h ../apilib/asc_ebc.h ../apilib/ebc_disp.h
dohits.o: ../ctlr/function.h dohits.h
ectype.o: ectype.h
mkastods.o: ../apilib/asc_ebc.h ../apilib/ebc_disp.h
mkastosc.o: ../general/general.h ../ctlr/function.h dohits.h
mkdctype.o: ../apilib/ebc_disp.h ectype.h
mkdstoas.o: ../apilib/asc_ebc.h ../apilib/ebc_disp.h
mkhits.o: ../ctlr/function.h dohits.h
prt3270.o: ../general/general.h ../apilib/asc_ebc.h ../ctlr/hostctlr.h
prt3270.o: ../ctlr/screen.h ../ctlr/function.h ../apilib/astosc.h
prt3270.o: ../general/globals.h ../ctlr/kbd.out
