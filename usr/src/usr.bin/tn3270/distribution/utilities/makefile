# msdos versus unix defines
O	= .o
PC_O	= .obj

X	=
PC_X	= .exe

CC	= cc
PC_CC	= cl

MV	= mv
PC_MV	= rename

RM	= rm -f
PC_RM= erase

LINT_ARGS =
PC_LINT_ARGS = -DLINT_ARGS

DEBUG_FLAGS = -g
PC_DEBUG_FLAGS = -Zi -Od

AR	= ar
C	=
PC_AR	= lib
PC_C	= +

RANLIB	= ranlib
PC_RANLIB = echo "Done with "

DEFINES = ${LINT_ARGS}

CFLAGS = ${DEBUG_FLAGS} -I..

PRINT = lpr -p

ALLC = tnrecv.c
ALLH = tncomp.h

ALLPRINT = ${ALLH} ${ALLC}

ALLSOURCE = ${ALLPRINT} makefile makefile.mak

ALLO = tnrecv$O

.c.obj:
	${CC} ${CFLAGS} -c $<

.DEFAULT:
	sccs get $<

all:	tnrecv

tnrecv:	tnrecv$O
	${CC} ${CFLAGS} -o $@ tnrecv$O ../apilib/apilib.a

clean:
	for i in makefile.bak ${ALLO} errs; \
		do (${RM} $$i); done

sccsclean:
	-sccs clean
	-sccs get makefile

action:
	${ACTION}

print:
	${PRINT} ${ALLPRINT}

sourcelist:	${ALLSOURCE}
	@for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}$$i); done

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

#include "apilib.h" "tncomp.h" "../api/api.h" "../ctlr/function.h"
#include "../ctlr/hostctlr.h" "../ctlr/oia.h" "../ctlr/screen.h"
#include "../ascii/disp_asc.h" "../ascii/astosc.h" "../general/general.h"
