CFLAGS = -g -I..

PRINT = lpr -p

ALLC = tnrecv.c
ALLH = tncomp.h

ALLPRINT = ${ALLH} ${ALLC}

ALLSOURCE = ${ALLPRINT} makefile makefile.mak

ALLO = tnrecv.o

.DEFAULT:
	sccs get $<

all:	tnrecv

tnrecv:	tnrecv.o
	${CC} ${CFLAGS} -o $@ tnrecv.o ../apilib/apilib.a

clean:
	-/bin/rm -f makefile.bak ${ALLO} errs
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

#include "apilib.h" "tncomp.h" "../api/api.h" "../ctlr/function.h"
#include "../ctlr/hostctlr.h" "../ctlr/oia.h" "../ctlr/screen.h"
#include "../ascii/disp_asc.h" "../ascii/astosc.h" "../general/general.h"
