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
ALLH = astosc.h ascebc.h disp_asc.h map3270.ext state.h termin.ext

ALLC = astosc.c map3270.c termin.c asctab.c ebctab.c

ALLO	= astosc.o map3270.o termin.o asctab.o ebctab.o disp_asc.o

ALLPRINT = default.map ${ALLH} ${ALLC}

ALLSOURCE =	${ALLPRINT} makefile makefile.mak

.s.o:
	/lib/cpp -E $< | as -o $@

ascii.lib:	$(ALLO)
	ar cr ascii.lib $(ALLO)
	ranlib ascii.lib

clean:
	rm -f $(ALLO) astosc.out disp_asc.out asc_disp.out \
				errs makefile.bak ascii.lib

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
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${ALLC}

.DEFAULT:
	sccs get $<

astosc.out:	../ctlr/function.h ../ctlr/hostctlr.h ../ctlr/$(KBD)
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

astosc.o: ../general/general.h ../ctlr/function.h astosc.h state.h astosc.out
map3270.o: state.h ../general/globals.h map3270.ext default.map
termin.o: ../general/general.h ../ctlr/function.h ../ctlr/inbound.ext
termin.o: ../ctlr/outbound.ext ../telnet.ext termin.ext astosc.h state.h
termin.o: ../general/globals.h
asctab.o: ascebc.h
ebctab.o: ascebc.h
