# msdos versus unix defines
O	= .o
#PC_O	= .obj

X	=
#PC_X	= .exe

L	=
#PC_L	= -link

CC	= cc
#PC_CC	= cl

MV	= mv
#PC_MV	= rename

RM	= rm -f
#PC_RM= erase

LINT_ARGS =
#PC_LINT_ARGS = -DLINT_ARGS

DEBUG_FLAGS = -g
#PC_DEBUG_FLAGS = -Zi -Od

AR	= ar
AR1	= cr
AR2	=
AR3	=
#PC_AR	= lib
#PC_AR1	=
#PC_AR2	= +
#PC_AR3	= ";"

RANLIB	= ranlib
#PC_RANLIB = echo "Done with "

PRINT	= print

DEFINES = ${LINT_ARGS}

INCLUDES = -I.

OPTIMIZE = -O
OPTIMIZE = ${DEBUG_FLAGS}

CFLAGS	= $(OPTIMIZE) $(INCLUDES) $(DEFINES)

# Lint flags
LINTFLAGS	= -hbxaz

# The source files...
ALLH =	defines.h externs.h general.h types.h

ALLC =	commands.c main.c network.c sys_bsd.c sys_dos.c telnet.c \
		terminal.c utilities.c

ALLO =	commands$O main$O network$O sys_bsd$O sys_dos$O telnet$O \
		terminal$O utilities$O

ALLPRINT =	${ALLH} ${ALLC}

ALLSOURCE =	${ALLPRINT} Makefile

.s.o:
	/lib/cpp -E $< | as -o $@

.c.obj:
	${CC} ${CFLAGS} -c $<

telnet$X:	${ALLO}
	${CC} ${CFLAGS} -o telnet ${ALLO}

clean:
	for i in $(ALLO) telnet$X errs \
		Makefile.bak; \
			do (${RM} $$i); done

sccsclean:
	-sccs clean
	-sccs get Makefile

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
	lint ${LINTFLAGS} ${INCLUDES} ${DEFINES} ${ALLC} -lcurses

.DEFAULT:
	sccs get $<

depend:
	grep '^#include' ${ALLC} | grep -v '<' | \
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
	-rm -f Makefile.bak
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep

# DO NOT DELETE THIS LINE

commands$O: externs.h defines.h types.h
main$O: externs.h
network$O: externs.h
sys_bsd$O: externs.h
telnet$O: externs.h types.h general.h
terminal$O: externs.h types.h
utilities$O: externs.h
