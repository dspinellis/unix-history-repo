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
ALLH =	function.h hostctlr.h inbound.ext oia.h \
	options.ext options.h outbound.ext screen.h scrnctlr.h

ALLC =	inbound.c oia.c options.c outbound.c

ALLO =	inbound.o oia.o options.o outbound.o

ALLPRINT =	3180.kbd 3270pc.kbd unix.kbd ${ALLH} ${ALLC}

ALLSOURCE =	${ALLPRINT} makefile makefile.mak

.s.o:
	/lib/cpp -E $< | as -o $@

ctlrlib.a:	${ALLO}
	ar cr ctlrlib.a ${ALLO}
	ranlib ctlrlib.a

clean:
	rm -f $(ALLO) mset tn3270 prt3270 m4.out errs makefile.bak ctlrlib.a
	rm -f kbd.out TMPfunc.out TMPfunc.c
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
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 ${ALLC} -lcurses

.DEFAULT:
	sccs get $<

kbd.out:	$(KBD) hostctlr.h
	(cd ../tools; make mkhits $(MFLAGS) )
	-/bin/rm -f $@ TMPfunc.out TMPfunc.c
	/bin/echo \#include \"function.h\" > TMPfunc.c
	$(CC) $(CFLAGS) -E TMPfunc.c > TMPfunc.out
	../tools/mkhits - ../ctlr/TMPfunc.out < $(KBD) > $@
	-/bin/rm -f TMPfunc.out TMPfunc.c

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

inbound.o: ../general/general.h function.h hostctlr.h oia.h scrnctlr.h screen.h
inbound.o: options.h ../apilib/dctype.h ../apilib/ebc_disp.h
inbound.o: ../general/globals.h inbound.ext outbound.ext ../telnet.ext kbd.out
oia.o: ../general/general.h oia.h ../general/globals.h
options.o: options.h ../general/globals.h options.ext
outbound.o: ../general/general.h hostctlr.h oia.h screen.h ../apilib/ebc_disp.h
outbound.o: ../general/globals.h options.ext ../telnet.ext inbound.ext
outbound.o: outbound.ext ../general/bsubs.ext
