CC	= cc
PRINT	= print

DEFINES =

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = -g

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

ALLC = termout.c

ALLO = termout.o

ALLH = terminal.h

sys.lib:	$(ALLO)
	ar cr sys.lib $(ALLO)
	ranlib sys.lib

clean:
	rm -f $(ALLO) disp_asc.out errs makefile.bak sys.lib

print:
	${PRINT} ${ALLC} ${ALLH}

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

disp_asc.out:	../ctlr/ebc_disp.o
	(cd ../tools; make mkdstoas $(MFLAGS) )
	-/bin/rm $@
	../tools/mkdstoas > $@

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

termout.o: ../general/general.h terminal.h ../telnet.ext ../ctlr/hostctlr.h
termout.o: ../ctlr/inbound.ext ../ctlr/oia.h ../ctlr/options.ext
termout.o: ../ctlr/outbound.ext ../ctlr/screen.h ../ascii/map3270.ext
termout.o: ../general/globals.h disp_asc.out
