CC	= cc
PRINT	= print

DEFINES =

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = -g

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

ALLH =	spint.h termout.ext video.h

ALLC = spintc.c system.c termout.c

ALLASM =	spintasm.asm

ALLO = termout.o system.o

ALLPRINT =	${ALLH} ${ALLASM} ${ALLC}

ALLSOURCE =	${ALLPRINT} makefile makefile.mak

screenlib:	$(ALLO)
	ar cr screenlib $(ALLO)
	ranlib screenlib

clean:
	rm -f $(ALLO) errs makefile.bak screenlib
	-sccs clean
	-sccs get makefile

sourcelist:	${ALLSOURCE}
	@for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}$$i); done

print:
	${PRINT} ${ALLPRINT}

tags:	${ALLC} ${ALLH}
	ctags -t ${ALLC} ${ALLH}

action:
	${ACTION}

lint:
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 \
				${TNMAIN} ${MOSTC} -lcurses
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${MSMAIN} map3270.c -lcurses

.DEFAULT:
	sccs get $<

depend:
	grep '^#include' ${ALLC} ${ALLH} | grep -v '<' | \
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

termout.o: ../general/general.h ../telnet.ext ../ascii/disp_asc.h
termout.o: ../ascii/map3270.ext ../ctlr/hostctlr.h ../ctlr/inbound.ext
termout.o: ../ctlr/oia.h ../ctlr/options.ext ../ctlr/outbound.ext
termout.o: ../ctlr/screen.h ../general/globals.h video.h
system.o: ../general/general.h ../api/api.h spint.h ../general/globals.h
