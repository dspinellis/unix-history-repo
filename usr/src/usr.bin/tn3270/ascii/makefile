CC	= cc
PRINT	= print

DEFINES = 

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = -g

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

KBD	= 3270pc.kbd
KBD	= unix.kbd


# The source files...
ALLH = ascebc.h disp_asc.h state.h

ALLC = map3270.c termin.c asctab.c ebctab.c

ALLO	= map3270.o termin.o asctab.o ebctab.o disp_asc.o

M4FILE = termcodes.m4

.s.o:
	/lib/cpp -E $< | as -o $@

ascii.lib:	$(ALLO)
	ar cr ascii.lib $(ALLO)
	ranlib ascii.lib

clean:
	rm -f $(ALLO) m4.out astosc.out disp_asc.out asc_disp.out \
				errs makefile.bak ascii.lib

print:
	${PRINT} ${ALLH} ${ALLC}

tags:	${ALLC} ${ALLH}
	ctags -t ${ALLC} ${ALLH}

action:
	${ACTION}

lint:
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${ALLC}

.DEFAULT:
	sccs get $<

astosc.out:	../ctlr/function.h ../ctlr/hostctlr.h ../ctlr/$(KBD) m4.out
	(cd ../tools; make mkastosc $(MFLAGS) )
	-/bin/rm $@
	../tools/mkastosc < ../ctlr/$(KBD) > $@

asc_disp.out:	../ctlr/ebc_disp.o
	(cd ../tools; make mkastods $(MFLAGS) )
	-/bin/rm $@
	../tools/mkastods > $@

disp_asc.out:	../ctlr/ebc_disp.o
	(cd ../tools; make mkdstoas $(MFLAGS) )
	-/bin/rm $@
	../tools/mkdstoas > $@

disp_asc.o:	disp_asc.h asc_disp.out disp_asc.out
	-/bin/rm $@ TMPfunc.c
	/bin/echo \#include \"disp_asc.h\" > TMPfunc.c
	/bin/echo \#include \"asc_disp.out\" >> TMPfunc.c
	/bin/echo \#include \"disp_asc.out\" >> TMPfunc.c
	$(CC) $(CFLAGS) -c TMPfunc.c
	-/bin/mv TMPfunc.o $@

m4.out:		termcodes.m4
	/bin/rm -f m4.out
	m4 termcodes.m4 > m4.out
	/bin/chmod 444 m4.out

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

map3270.o: m4.out state.h ../general/globals.h map3270.ext default.map
termin.o: ../general/general.h m4.out state.h ../general/globals.h
termin.o: ../ctlr/function.h ../ctlr/inbound.ext ../ctlr/outbound.ext
termin.o: ../telnet.ext termin.ext astosc.out
asctab.o: ascebc.h
ebctab.o: ascebc.h
