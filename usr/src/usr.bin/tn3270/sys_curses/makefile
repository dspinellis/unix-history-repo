CC	= cc
PRINT	= print

DEFINES =

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = -g

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

ALLH = terminal.h termout.ext

ALLC = system.c termout.c

ALLO = system.o termout.o

ALLPRINT =	${ALLH} ${ALLC}

ALLSOURCE =	${ALLPRINT} makefile

sys.lib:	$(ALLO)
	ar cr sys.lib $(ALLO)
	ranlib sys.lib

clean:
	rm -f $(ALLO) errs makefile.bak sys.lib

sourcelist:	${ALLSOURCE}
	@for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}/$$i); done

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

system.o: ../general/general.h ../api/api.h ../apilib/api_exch.h
system.o: ../general/globals.h
termout.o: ../general/general.h terminal.h ../telnet.ext ../ascii/disp_asc.h
termout.o: ../ctlr/hostctlr.h ../ctlr/inbound.ext ../ctlr/oia.h
termout.o: ../ctlr/options.ext ../ctlr/outbound.ext ../ctlr/screen.h
termout.o: ../ascii/map3270.ext ../general/globals.h
