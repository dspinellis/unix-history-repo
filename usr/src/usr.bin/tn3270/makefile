# Makefile for tn3270 and friends...
# @(#)makefile	3.1  10/29/86
#
#	This is the makefile for tn3270.  Note that we use ../telnet.c
# (compiled with special options; see below) to provide the telnet
# support we need.
#
#	The following are the defines that may be passed (via the cc
# -D option) to the compiler.
#
#	The first group relate only to ../telnet.c:
#
#	TN3270		- 	This is to be linked with tn3270.  Necessary
#				for creating tn3270.
#
#	DEBUG		-	Allow for some extra debugging operations.
#
#	NOT43		-	Allows the program to compile and run on
#				a 4.2BSD system.
#
#	PUTCHAR		-	Within tn3270, on a NOT43 system,
#				allows the use of the 4.3 curses
#				(greater speed updating the screen).
#				You need the 4.3 curses for this to work.
#
#	FD_SETSIZE	-	On whichever system, if this isn't defined,
#				we patch over the FD_SET, etc., macros with
#				some homebrewed ones.
#
#	SO_OOBINLINE	-	This is a socket option which we would like
#				to set to allow TCP urgent data to come
#				to us "inline".  This is NECESSARY for
#				CORRECT operation, and desireable for
#				simpler operation.
#
#	LNOFLSH		-	Detects the presence of the LNOFLSH bit
#				in the tty structure.
#
#
#	Here are some which are used throughout the system:
#
#	SLOWSCREEN	-	If SLOWSCREEN is defined, we generate code
#				that attempts to optimize the line between
#				the computer (on which tn3270 runs) and the
#				user's terminal (which line may be 1200 baud,
#				or some such) at the cost of CPU time on
#				the above referenced computer.
#
#	unix		-	Compiles in unix specific stuff.
#
#	msdos		-	Compiles in msdos specific stuff.
#

CC	= cc
PRINT	= print

DEFINES = -DSLOWSCREEN
DEFINES = -DSLOWSCREEN -DDEBUG

INCLUDES = -I. -I..

OPTIMIZE = -O
OPTIMIZE = -g

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
ALLH =

TNMAIN = ../telnet.c
TNMAIN = telnet.c
MSMAIN = ascii/mset.c

# The places where the various components live...
SUBDIR =	ascii ctlr sys general api
SUBLIB =	ascii/ascii.lib ctlr/ctlr.lib \
			sys/sys.lib general/general.lib api/api.lib


ALLC = ${TNMAIN} ${MSMAIN}

ALLO	= telnet.o mset.o

.s.o:
	/lib/cpp -E $< | as -o $@

all:	${SUBDIR} tn3270.a tn3270 mset

tn3270.a: ${SUBLIB}
	-mkdir tmp
	for i in ${SUBDIR}; do (cd tmp; ar x ../$$i/$${i}.lib); done
	ls tmp/*.o | sort -t/ +1 > t1
	ar cr tn3270.a `lorder \`cat t1\` | tsort`
	rm -f t1 tmp/*
	ranlib tn3270.a
#	for i in ${SUBDIR}; do (cd tmp; ar x ../$$i/$${i}.lib_p); done
#	ls tmp/*.o | sort -t/ +1 > t1
#	ar cr tn3270_p.a `lorder \`cat t1\` | tsort`
#	rm -rf t1 tmp

${SUBDIR}: FRC
	cd $@; make ${MFLAGS} "CFLAGS=${CFLAGS}"

FRC:

tn3270:	telnet.o tn3270.a
	${CC} ${CFLAGS} -o tn3270 telnet.o tn3270.a $(LIBCURSES) $(LIBTERM)

mset:	mset.o ascii/map3270.o
	${CC} ${CFLAGS} -o mset mset.o ascii/map3270.o

telnet.o:	$(TNMAIN)
	$(CC) $(CFLAGS) -DTN3270 -c $(TNMAIN)

mset.o:	$(MSMAIN)
	$(CC) $(CFLAGS) -c $(MSMAIN)

install:
	install -s tn3270 $(BINDIR)
	install -s mset $(BINDIR)
	if [ ! -f ${ETCDIR}/map3270 ]; then \
		install -c -m 444 map3270.dat ${ETCDIR}/map3270; \
	fi
#	install -c -m 444 man/tn3270.1 $(MANDIR)1/tn3270.1
#	install -c -m 444 man/mset.1 $(MANDIR)1/mset.1
#	install -c -m 444 man/map3270.5 $(MANDIR)5/map3270.5

action:
	${ACTION}

everywhere:
	for i in ${SUBDIR} tools; do (echo "[$$i]"; cd $$i; make ${MFLAGS} action "ACTION=${ACTION}"); done

clean:
	rm -f $(ALLO) mset tn3270 errs makefile.bak tn3270.a
	for i in ${SUBDIR} tools; do (cd $$i; make ${MFLAGS} clean); done
	rm -f t1 tmp/*.o
	-rmdir tmp

print:
	${PRINT} ${ALLH} ${MSMAIN} ${TNMAIN}
	for i in ${SUBDIR}; \
			do (cd $$i; make ${MFLAGS} "PRINT=${PRINT}" print); done
	cd tools; make ${MFLAGS} "PRINT=${PRINT}" print

tags:	${ALLC} ${ALLH}
	ctags -t ${ALLC} ${ALLH}

lint:
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 \
				${TNMAIN} -lcurses
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${MSMAIN} map3270.c -lcurses

.DEFAULT:
	sccs get $<

depend:	thisdepend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

thisdepend:
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

telnet.o: ctlr/screen.h general/globals.h telnet.ext ctlr/options.ext
telnet.o: ctlr/outbound.ext ascii/termin.ext general/general.h
ascii/mset.o: state.h m4.out ../general/globals.h map3270.ext
