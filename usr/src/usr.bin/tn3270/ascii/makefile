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
ALLH = map3270.ext state.h termin.ext

ALLC = map3270.c mset.c termin.c

# We don't include mset.o here...
ALLO	= map3270.o termin.o

ALLPRINT = default.map ${ALLH} ${ALLC}

ALLSOURCE =	${ALLPRINT} makefile makefile.mak

.s.o:
	/lib/cpp -E $< | as -o $@

asciilib.a:	$(ALLO)
	ar cr asciilib.a $(ALLO)
	ranlib asciilib.a

clean:
	rm -f $(ALLO) errs makefile.bak asciilib.a
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
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${ALLC}

.DEFAULT:
	sccs get $<

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

map3270.o: state.h ../general/globals.h map3270.ext default.map
mset.o: ../ctlr/function.h state.h ../apilib/astosc.h ../general/globals.h
mset.o: map3270.ext
termin.o: ../general/general.h ../ctlr/function.h ../ctlr/inbound.ext
termin.o: ../ctlr/outbound.ext ../telnet.ext termin.ext ../apilib/astosc.h
termin.o: state.h ../general/globals.h
