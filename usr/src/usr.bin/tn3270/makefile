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
#	unix		-	Compiles in unix specific stuff.
#
#	msdos		-	Compiles in msdos specific stuff.
#

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
ACTION = sccs tell

DEFINES = ${LINT_ARGS}

INCLUDES = -I. -I..

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
ALLH = telnet.ext

TNMAIN = ../telnet.c
TNMAIN = telnet.c
MSMAIN = ascii/mset.c

ALLC = ${TNMAIN}

ALLO	= telnet$O mset$O

ALLPRINT =	${ALLH} ${ALLC}

ALLSOURCE =	${ALLPRINT} makefile makefile.mak README

SYS	= sys
PC_SYS	= sys_dos

# The places where the various components live...

SUBDIR =	api apilib ascii ctlr general ${SYS}

# The following are directories we don't do regular make's in, but
# we do make everywhere, print, and sourcelist in.

EXTRADIR =	arpa sys_dos tools utilities

# The libraries we use.  The order here is important.
# ctlrlib.a and syslib.a should come first, then apilib.a
# then the rest.
SUBLIB =	ctlr/ctlrlib.a ${SYS}/syslib.a \
		api/apilib.a \
		ascii/asciilib.a general/generallib.a

.s.o:
	/lib/cpp -E $< | as -o $@

.c.obj:
	${CC} ${CFLAGS} -c $<

all:	FRC tn3270$X mset$X

FRC:
	for i in ${SUBDIR}; \
		do (cd $$i; make ${MFLAGS} "CFLAGS=${CFLAGS}"); done

tn3270$X:	telnet$O ${SUBLIB}
	${CC} ${CFLAGS} -o tn3270 telnet$O \
		${SUBLIB} apilib/apilib.a $(LIBCURSES) $(LIBTERM)

mset$X:	mset$O ascii/map3270$O
	${CC} ${CFLAGS} -o mset mset$O ascii/map3270$O apilib/apilib.a

telnet$O:	$(TNMAIN)
	$(CC) $(CFLAGS) -DTN3270 -c $(TNMAIN)

mset$O:	$(MSMAIN)
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

everywhere:	action
	for i in ${SUBDIR} ${EXTRADIR}; \
		do (echo "[$$i]"; cd $$i; make ${MFLAGS} action \
						"ACTION=${ACTION}"); done

clean:
	for i in $(ALLO) mset tn3270 errs makefile.bak; \
			do (${RM} $$i); done
	for i in ${SUBDIR} ${EXTRADIR}; \
			do (cd $$i; make ${MFLAGS} clean); done

sccsclean:
	-sccs clean
	-sccs get makefile
	for i in ${SUBDIR} ${EXTRADIR}; \
			do (cd $$i; make ${MFLAGS} sccsclean); done

print:
	${PRINT} ${ALLPRINT}
	for i in ${SUBDIR} ${EXTRADIR}; \
			do (cd $$i; make ${MFLAGS} "PRINT=${PRINT}" print); done

tags:	${ALLC} ${ALLH}
	ctags -t ${ALLC} ${ALLH}

sourcelist:	${ALLSOURCE}
	@for i in ${ALLSOURCE}; \
		do (echo ${DIRPATH}$$i); done
	@for i in ${SUBDIR} ${EXTRADIR}; \
		do (cd $$i; make ${MFLAGS} "DIRPATH=${DIRPATH}$$i/" \
							    sourcelist); done

lint:
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 \
				${TNMAIN} -lcurses
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${MSMAIN} map3270.c -lcurses

makefiles.pc:
	for i in . ${SUBDIR} ${EXTRADIR}; \
	do (sed -e "s/lib\.a/.lib/g" -e "s/^PC_//" < $$i/makefile | \
			    mkmake | sed -e "sx/x\\\\x" > $$i/makefile.mak); \
	    done

.DEFAULT:
	sccs get $<

depend:	thisdepend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

thisdepend:
	echo > eddep.c
	grep '^#include' ${ALLC} eddep.c | grep -v '<' | \
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
	rm eddep makedep eddep.c

# DO NOT DELETE THIS LINE

telnet$O: ascii/termin.ext ctlr/screen.h ctlr/oia.h ctlr/options.ext
telnet$O: ctlr/outbound.ext general/globals.h telnet.ext general/general.h
