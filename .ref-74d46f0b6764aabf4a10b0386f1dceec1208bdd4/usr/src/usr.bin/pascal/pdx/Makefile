#	@(#)Makefile	1.22 (Berkeley) %G%
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
# N.B.: My version of cerror in "cerror.s" automatically catches certain
#	errors such as out of memory, I/O error.  If you re-make pdx with
#	the standard cerror, the program could fault unexpectedly.

PROG=	pdx
CFLAGS=	-DOBJ -DADDR32 -I. -I${.CURDIR} -I${.CURDIR}/../src
.PATH:	${.CURDIR}/breakpoint ${.CURDIR}/object ${.CURDIR}/mappings \
	${.CURDIR}/main ${.CURDIR}/symtab ${.CURDIR}/process \
	${.CURDIR}/command ${.CURDIR}/source ${.CURDIR}/tree ${.CURDIR}/sym \
	${.CURDIR}/runtime ${.CURDIR}/machine ${.CURDIR}/../src

SRCS=	address.c assign.c attributes.c bp.c bpact.c build.c callproc.c \
	entry.c eval.c fixbps.c frame.c functab.c isactive.c library.c \
	main.c maketypes.c misc.c nextaddr.c objaddr.c opinfo.c optab.c \
	predicates.c print.c printdata.c printdecl.c printerror.c \
	printinst.c printnews.c printval.c prtree.c pstatus.c ptrace.c \
	rdwr.c readobj.c readsym.c remake.c resume.c runcont.c setbp.c \
	setbps.c source.c srcfile.c srcline.c start.c status.c step.c \
	symtab.c tfree.c tr_equal.c tracestop.c trcond.c tree.c trinfo.c \
	wheredump.c which.c
OBJS+=	cerror.o grammar.o token.o
CLEANFILES+=h02opcs.h grammar.o opc token.o y.tab.c y.tab.h

cerror.o:
	${CC} -c ${.CURDIR}/${MACHINE}/${.PREFIX}.s

h02opcs.h: opc ${.CURDIR}/../src/OPnames.h
	./opc > ${.TARGET}

opc: opc.c
	${CC} ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

.include <bsd.prog.mk>

token.o: grammar.o
${OBJS} .depend: h02opcs.h
