# Copyright (c) 1982 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.14 (Berkeley) %G%
#
# make file for pdx
#
# For best results edit this file in vi with tabstop=4.
#
# The file "defs.h" is included by all, it has most of the type
# declarations (since you can't have forward type references in C).
# It also defines various things of general use and includes <stdio.h>.
#
# The file "library.c" contains routines which are generally useful
# and are independent from pdx.
#
# N.B.: My version of cerror in "cerror.s" automatically catches certain errors
#	such as out of memory, I/O error.  If you re-make pdx with
#	the standard cerror, the program could fault unexpectedly.
#
#	Also, this makefile contains almost NO HEADER DEPENDENCIES.  So
#	if you modify a header file in a non-trivial way, you need
#	to touch the C files that include it.
#

DESTDIR	=
HOME	= /usr/src/ucb/pdx
LIB	= library.o cerror.o
SRCDIR	= ../src/
INSTALL	= ${DESTDIR}/usr/ucb/pdx

RM	= /bin/rm -f
CFLAGS	= -Disvaxpx -I.. -I../${SRCDIR} -DOBJ -DADDR32 -O
LDFLAGS	= # -i for pdp-11

.c.o:
	@echo "compiling $*.c"
	@csh -f -c ' \
	    set a=$*.c; \
	    if ($$a:h != $$a) chdir $$a:h; \
	    ${CC} ${CFLAGS} -c $$a:t \
	'

.s.o:
	@echo "assembling $*.s"
	@cc -c $*.s

#
# source directory specifications
#

MAIN		= main/
SYMTAB		= symtab/
SOURCE		= source/
COMMAND		= command/
TREE		= tree/
SYM		= sym/
BREAKPOINT	= breakpoint/
RUNTIME		= runtime/
OBJECT		= object/
MAPPINGS	= mappings/
PROCESS		= process/
MACHINE		= machine/

BPOBJ =\
	${BREAKPOINT}bp.o\
	${BREAKPOINT}bpact.o\
	${BREAKPOINT}fixbps.o\
	${BREAKPOINT}status.o\
	${BREAKPOINT}printnews.o\
	${BREAKPOINT}setbps.o\
	${BREAKPOINT}trinfo.o\
	${BREAKPOINT}trcond.o

OBJOBJ =\
	${OBJECT}readobj.o\
	${OBJECT}readsym.o\
	${OBJECT}maketypes.o

MAPOBJ =\
	${MAPPINGS}functab.o\
	${MAPPINGS}objaddr.o\
	${MAPPINGS}srcfile.o\
	${MAPPINGS}srcline.o

CMDOBJ =\
	${COMMAND}y.tab.o\
	${COMMAND}lex.yy.o\
	${COMMAND}remake.o

RUNTIMEOBJ =\
	${RUNTIME}frame.o\
	${RUNTIME}wheredump.o\
	${RUNTIME}isactive.o\
	${RUNTIME}address.o\
	${RUNTIME}callproc.o\
	${RUNTIME}entry.o

MACHOBJ =\
	${MACHINE}nextaddr.o\
	${MACHINE}setbp.o\
	${MACHINE}optab.o\
	${MACHINE}printdata.o\
	${MACHINE}printerror.o\
	${MACHINE}printinst.o\
	${MACHINE}pxerrors.o

PROCOBJ =\
	${PROCESS}runcont.o\
	${PROCESS}pstatus.o\
	${PROCESS}rdwr.o\
	${PROCESS}resume.o\
	${PROCESS}start.o\
	${PROCESS}step.o\
	${PROCESS}ptrace.o

SYMOBJ =\
	${SYM}predicates.o\
	${SYM}attributes.o\
	${SYM}printdecl.o\
	${SYM}which.o\
	${SYM}print.o\
	${SYM}printval.o\
	${SYM}tree.o

TREEOBJ =\
	${TREE}build.o\
	${TREE}eval.o\
	${TREE}opinfo.o\
	${TREE}prtree.o\
	${TREE}tfree.o\
	${TREE}tr_equal.o\
	${TREE}tracestop.o\
	${TREE}misc.o\
	${TREE}assign.o

OBJS =\
	${MAIN}main.o\
	${SYMTAB}symtab.o\
	${SOURCE}source.o\
	${CMDOBJ}\
	${TREEOBJ}\
	${SYMOBJ}\
	${BPOBJ}\
	${RUNTIMEOBJ}\
	${OBJOBJ}\
	${MAPOBJ}\
	${PROCOBJ}\
	${MACHOBJ}

a.out: ${OBJS} ${LIB}
	@echo "linking"
	@cc ${LDFLAGS} ${OBJS} ${LIB}

profile: ${OBJS}
	@echo "linking with -p"
	@cc ${LDFLAGS} -p ${OBJS} ${LIB}

${COMMAND}y.tab.o: ${COMMAND}grammar.yacc
	chdir ${COMMAND}; yacc -d grammar.yacc; cc -c ${CFLAGS} y.tab.c
	${RM} ${COMMAND}y.tab.c

${COMMAND}lex.yy.o: ${COMMAND}token.lex ${COMMAND}y.tab.h
	chdir ${COMMAND}; lex token.lex; cc -c ${CFLAGS} lex.yy.c
	${RM} ${COMMAND}lex.yy.c

${BPOBJ}:	${BREAKPOINT}bp.rep
${RUNTIMEOBJ}:	${RUNTIME}frame.rep
${PROCOBJ}:	${PROCESS}process.rep
${SYMOBJ}:	${SYM}sym.rep
${TREEOBJ}:	${TREE}tree.rep
${OBJOBJ}:	${OBJECT}objsym.rep
${OBJECT}readobj.o ${OBJECT}readsym.o: ${SRCDIR}objfmt.h

install: a.out
	mv a.out ${INSTALL}

clean:
	${RM} a.out */*.o

#
# Warning:  This will produce a very long listing.  You would be better
#           off just looking at things on-line.
#

print:
	pr *.h [a-z]*/*.{h,rep,yacc,lex,c}
