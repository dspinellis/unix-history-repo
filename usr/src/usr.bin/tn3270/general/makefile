# msdos versus unix defines
O	= .o
PC_O	= .obj

X	=
PC_X	= .exe

CC	= cc
PC_CC	= cl

MV	= mv
PC_MV	= rename

RM	= rm -f
PC_RM= erase

LINT_ARGS =
PC_LINT_ARGS = -DLINT_ARGS

DEBUG_FLAGS = -g
PC_DEBUG_FLAGS = -Zi -Od

AR	= ar
C	=
PC_AR	= lib
PC_C	= +

RANLIB	= ranlib
PC_RANLIB = echo "Done with "

PRINT	= print

DEFINES = ${LINT_ARGS}

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = ${DEBUG_FLAGS}

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz
# How to install the bloody thing...

DESTDIR=

BINDIR		= $(DESTDIR)/usr/ucb
ETCDIR		= $(DESTDIR)/etc
MANDIR		= $(DESTDIR)/usr/man/man

# Names for the terminal libraries...
LIBCURSES	= -lcurses
LIBTERM		= -ltermlib

# The source files...
ALLH =	bsubs.ext general.h globals.h

ALLC =	globals.c

#
#	In a vax environment, we use vaxbsubs.s, which gives us a fair amount
#	of increased performance.  We have provided genbsubs.c, which perform
#	(more or less) the same function.
SUBS = vaxbsubs.s
SUBS = genbsubs.c

SUBSO = vaxbsubs$O
SUBSO = genbsubs$O

ALLS = vaxbsubs.s

ALLPRINT =	${ALLH} ${ALLC} vaxbsubs.s genbsubs.c

ALLSOURCE =	${ALLPRINT} makefile makefile.mak

ALLS =

ALLO	= globals$O ${SUBSO}

.s.o:
	/lib/cpp -E $< | as -o $@

.c.obj:
	${CC} ${CFLAGS} -c $<

generallib.a:	${ALLO}
	${RM} $@
	for i in ${ALLO}; do (${AR} cr $@ $C $$i); done
	${RANLIB} $@

clean:
	for i in $(ALLO) errs makefile.bak generallib.a; \
		do (${RM} $$i); done

sccsclean:
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
	grep '^#include' ${ALLC} ${ALLH} | grep -v '<' | \
	sed -e 's/:[^"]*"\([^"]*\)".*/: \1/' \
	    -e 's/\.c/$$O/' | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '$$r makedep' >>eddep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	-rm -f makefile.bak
	cp makefile makefile.bak
	ed - makefile < eddep
	rm eddep makedep

# DO NOT DELETE THIS LINE

globals$O: ../ctlr/hostctlr.h ../ctlr/oia.h ../ctlr/options.h ../ctlr/screen.h
globals$O: globals.h ../general/general.h
