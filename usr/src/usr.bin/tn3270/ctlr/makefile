CC	= cc
PRINT	= print

DEFINES =

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = -g

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

# Which keyboard are we emulating.
KBD	= 3180.kbd
KBD	= 3270pc.kbd
KBD	= unix.kbd

# The source files...
ALLH =	dctype.h ebc_disp.h hostctlr.h kbdctlr.h options.h screen.h scrnctlr.h

MOSTC =	dctype.c ebc_disp.c inbound.c options.c outbound.c

ALLC = ${MOSTC}
ALLC = ${MOSTC}

MOSTO =	dctype.o ebc_disp.o inbound.o options.o outbound.o

ALLO	= $(MOSTO)

.s.o:
	/lib/cpp -E $< | as -o $@

ctlrlib:	${ALLO}
	ar cr ctlrlib ${ALLO}
	ranlib ctlrlib

clean:
	rm -f $(ALLO) mset tn3270 prt3270 m4.out errs makefile.bak ctlrlib
	rm -f kbd.out TMPfunc.out TMPfunc.c

print:
	${PRINT} ${ALLH} ${M4FILE} default.map
	${PRINT} ${MOSTC}
	${PRINT} ${MSMAIN} ${TNMAIN} ${PRT3270MAIN}

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

kbd.out:	$(KBD) hostctlr.h
	(cd ../tools; make mkhits $(MFLAGS) )
	-/bin/rm $@ TMPfunc.out TMPfunc.c
	/bin/echo \#include \"function.h\" > TMPfunc.c
	$(CC) $(CFLAGS) -E TMPfunc.c > TMPfunc.out
	../tools/mkhits - ../ctlr/TMPfunc.out < $(KBD) > $@
	-/bin/rm TMPfunc.out TMPfunc.c

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

dctype.o: dctype.h
inbound.o: ../general.h function.h hostctlr.h scrnctlr.h screen.h options.h
inbound.o: dctype.h ebc_disp.h ../system/globals.h inbound.ext outbound.ext
inbound.o: ../telnet.ext kbd.out
options.o: options.h ../system/globals.h options.ext
outbound.o: hostctlr.h screen.h ebc_disp.h ../system/globals.h options.ext
outbound.o: ../telnet.ext inbound.ext outbound.ext
