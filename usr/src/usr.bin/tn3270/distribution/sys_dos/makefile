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

ALLH =	spint.h termout.ext video.h

ALLC = spintc.c system.c termout.c

ALLASM =	spintasm.asm

ALLO = termout$O system$O

ALLPRINT =	${ALLH} ${ALLASM} ${ALLC}

ALLSOURCE =	${ALLPRINT} makefile makefile.mak

.c.obj:
	${CC} ${CFLAGS} -c $<

screenlib:	$(ALLO)
	${RM} $@
	for i in ${ALLO}; do (${AR} cr $@ $C $$i); done
	${RANLIB} $@

clean:
	for i in $(ALLO) errs makefile.bak screenlib; \
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
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} -DTN3270 \
				${TNMAIN} ${MOSTC} -lcurses
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${MSMAIN} map3270.c -lcurses

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

termout$O: ../general/general.h ../telnet.ext ../ascii/disp_asc.h
termout$O: ../ascii/map3270.ext ../ctlr/hostctlr.h ../ctlr/inbound.ext
termout$O: ../ctlr/oia.h ../ctlr/options.ext ../ctlr/outbound.ext
termout$O: ../ctlr/screen.h ../general/globals.h video.h
system$O: ../general/general.h ../api/api.h spint.h ../general/globals.h
