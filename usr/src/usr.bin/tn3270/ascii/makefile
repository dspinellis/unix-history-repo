CC	= cc
PRINT	= print

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = -g

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

# The source files...
ALLH =	ascebc.h

ALLC = asctab.c ebctab.c

ALLO	= asctab.o ebctab.o

.s.o:
	/lib/cpp -E $< | as -o $@

asciilib:	${ALLO}
	ar cr asciilib ${ALLO}
	ranlib asciilib

clean:
	rm -f $(ALLO) errs makefile.bak asciilib

print:
	${PRINT} ${ALLH}
	${PRINT} ${ALLC}

tags:	${ALLC} ${ALLH}
	ctags -t ${ALLC} ${ALLH}

lint:
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 $(ALLC)

.DEFAULT:
	sccs get $<

depend:
	grep '^#include' ${ALLC} | grep -v '<' | \
	sed -e 's/:[^"]*"\([^"]*\)".*/: \1/' \
	    -e 's/\.c/.o/' \
	    -e 's,../[a-zA-Z]*/,,' | \
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

asctab.o: ascebc.h
ebctab.o: ascebc.h
