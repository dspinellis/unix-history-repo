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
ALLH =	dctype.h ebc_disp.h function.h hostctlr.h oia.h options.h \
	screen.h scrnctlr.h

ALLC =	dctype.c ebc_disp.c inbound.c oia.c options.c outbound.c

ALLO =	dctype.o ebc_disp.o inbound.o oia.o options.o outbound.o

.s.o:
	/lib/cpp -E $< | as -o $@

ctlr.lib:	${ALLO}
	ar cr ctlr.lib ${ALLO}
	ranlib ctlr.lib

clean:
	rm -f $(ALLO) mset tn3270 prt3270 m4.out errs makefile.bak ctlr.lib
	rm -f kbd.out TMPfunc.out TMPfunc.c

print:
	${PRINT} ${ALLH} ${M4FILE} default.map
	${PRINT} ${ALLC}
	${PRINT} ${PRT3270MAIN}

tags:	${ALLC} ${ALLH}
	ctags -t ${ALLC} ${ALLH}

action:
	${ACTION}

lint:
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 ${ALLC} -lcurses

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
inbound.o: ../general/general.h function.h hostctlr.h oia.h scrnctlr.h screen.h
inbound.o: options.h dctype.h ebc_disp.h ../general/globals.h inbound.ext
inbound.o: outbound.ext ../telnet.ext kbd.out
oia.o: ../general/general.h oia.h ../general/globals.h
options.o: options.h ../general/globals.h options.ext
outbound.o: ../general/general.h hostctlr.h oia.h screen.h ebc_disp.h
outbound.o: ../general/globals.h options.ext ../telnet.ext inbound.ext
outbound.o: outbound.ext ../general/bsubs.ext
