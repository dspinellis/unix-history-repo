#	@(#)makefile	4.3 (Berkeley) %G%

# msdos versus unix defines
O	= .o
#PC_O	= .obj

X	=
#PC_X	= .exe

L	=
#PC_L	= -link

CC	= cc
#PC_CC	= cl

MV	= mv
#PC_MV	= rename

RM	= rm -f
#PC_RM= erase

LINT_ARGS =
#PC_LINT_ARGS = -DLINT_ARGS

DEBUG_FLAGS = -g
#PC_DEBUG_FLAGS = -Zi -Od

AR	= ar
AR1	= cr
AR2	=
AR3	=
#PC_AR	= lib
#PC_AR1	=
#PC_AR2	= +
#PC_AR3	= ";"

RANLIB	= ranlib
#PC_RANLIB = echo "Done with "

PRINT	= print

DEFINES	= ${LINT_ARGS}

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = ${DEBUG_FLAGS}

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

ALLH = dohits.h ectype.h

ALLY = mkmake.y

ALLC = dohits.c ectype.c mkastods.c mkastosc.c mkdctype.c \
		mkdstoas.c mkhits.c prt3270.c

ALLO =	prt3270$O mkdctype$O ectype$O \
		mkastods$O mkdstoas$O mkhits$O mkmake$O dohits$O mkastosc$O

ALLPRINT =	${ALLH} ${ALLC} ${ALLY}

ALLSOURCE =	${ALLPRINT} makefile makefile.mak

MKMAKE		= mkmake$X
#PC_MKMAKE	=

ALLEXE	=	mkastods$X mkastosc$X mkdctype$X mkdstoas$X \
			mkhits$X ${MKMAKE} prt3270$X

#.c.obj:
#	${CC} ${CFLAGS} -c $<

tools:
	@echo Need to specify WHICH tool ...

all:	${ALLEXE}
	@echo done.

prt3270$X:	prt3270$O ../general/globals$O ../api/asc_ebc$O \
				../api/astosc$O ../ctlr/kbd.out
	${CC} ${CFLAGS} -o prt3270 prt3270$O ../general/globals$O \
					../api/asc_ebc$O ../api/astosc$O

mkastosc$X:	mkastosc$O dohits$O ../api/asc_ebc$O ../api/ebc_disp$O
	$(CC) $(CFLAGS) -o mkastosc mkastosc$O dohits$O \
			../api/asc_ebc$O ../api/ebc_disp$O

mkastods$X:	mkastods$O ../api/asc_ebc$O ../api/ebc_disp$O
	$(CC) $(CFLAGS) -o mkastods mkastods$O ../api/asc_ebc$O \
						../api/ebc_disp$O

mkdstoas$X:	mkdstoas$O ../api/asc_ebc$O ../api/ebc_disp$O
	$(CC) $(CFLAGS) -o mkdstoas mkdstoas$O ../api/asc_ebc$O \
						../api/ebc_disp$O

mkhits$X:	mkhits$O dohits$O ../api/asc_ebc$O ../api/ebc_disp$O
	$(CC) $(CFLAGS) -o mkhits mkhits$O dohits$O ../api/asc_ebc$O \
						../api/ebc_disp$O

mkmake:	mkmake$O
	${CC} ${CFLAGS} -o mkmake mkmake$O

mkdctype$X:	mkdctype$O ../api/ebc_disp$O ectype$O
	$(CC) $(CFLAGS) -o mkdctype mkdctype$O ../api/ebc_disp$O ectype$O

../api/astosc$O:
	(cd ${.CURDIR}/../api; make astosc$O)

../api/asc_ebc$O:
	(cd ${.CURDIR}/../api; make asc_ebc$O)

../ctlr/kbd.out:
	(cd ${.CURDIR}/../ctlr; make kbd.out)

../api/ebc_disp$O:
	(cd ${.CURDIR}/../api; make ebc_disp$O)

../general/globals$O:
	(cd ${.CURDIR}/../general; make globals$O)

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
	for i in errs makefile.bak y.tab.c y.output ${ALLO} ${ALLEXE}; \
		do (${RM} $$i); done

sccsclean:
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
	    -e 's/\.c/$$O/' | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '$$r makedep' >>eddep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	-rm -f makefile.bak
	cp makefile makefile.bak
	ed - makefile < eddep
	rm eddep makedep

# DO NOT DELETE THIS LINE

dohits$O: ../general/general.h ../api/asc_ebc.h ../api/ebc_disp.h
dohits$O: ../ctlr/function.h dohits.h
ectype$O: ectype.h
mkastods$O: ../api/asc_ebc.h ../api/ebc_disp.h
mkastosc$O: ../general/general.h ../ctlr/function.h dohits.h
mkdctype$O: ../api/ebc_disp.h ectype.h
mkdstoas$O: ../api/asc_ebc.h ../api/ebc_disp.h
mkhits$O: ../ctlr/function.h dohits.h
prt3270$O: ../general/general.h ../api/asc_ebc.h ../ctlr/hostctlr.h
prt3270$O: ../ctlr/screen.h ../ctlr/function.h ../api/astosc.h
prt3270$O: ../general/globals.h ../ctlr/kbd.out
