#	@(#)Makefile	5.24 (Berkeley) %G%

PROG=	dbx
CFLAGS+=-I. -I${.CURDIR}
HDRS=	asm.h events.h c.h check.h coredump.h debug.h eval.h fortran.h \
	keywords.h languages.h lists.h machine.h main.h mappings.h \
	modula-2.h names.h object.h operators.h pascal.h printsym.h \
	process.h runtime.h scanner.h source.h stabstring.h symbols.h \
	tree.h ops.h
SRCS=	asm.c events.c c.c check.c coredump.c debug.c eval.c fortran.c \
	keywords.c languages.c library.c lists.c main.c mappings.c \
	modula-2.c names.c object.c operators.c pascal.c printsym.c \
	process.c scanner.c source.c stabstring.c symbols.c tree.c \
	${MACHINE}.c runtime.${MACHINE}.c ops.${MACHINE}.c
OBJS+=	cerror.${MACHINE}.o commands.o date.o
LDADD=	-lcompat
DPADD=	${LIBCOMPAT}
CLEANFILES+=mkdate makedefs makedefs.o date.c y.tab.h \
	${HDRS} ${MACHINE}.h runtime.${MACHINE}.h ops.${MACHINE}.h

.c.h:
	./makedefs -f ${.CURDIR}/$*.c $*.h

all: ${HDRS}

machine.h: ${MACHINE}.h
	rm -f machine.h
	ln -s ${MACHINE}.h machine.h

runtime.h: runtime.${MACHINE}.h
	rm -f runtime.h
	ln -s runtime.${MACHINE}.h runtime.h

ops.h: ops.${MACHINE}.h
	rm -f ops.h
	ln -s ops.${MACHINE}.h ops.h

date.c: mkdate
	./mkdate > date.c

${HDRS}: makedefs
makedefs: makedefs.c library.o cerror.${MACHINE}.o
	${CC} ${CFLAGS} \
	    ${.CURDIR}/makedefs.c library.o cerror.${MACHINE}.o -o $@

mkdate: mkdate.c
	${CC} ${CFLAGS} ${.CURDIR}/mkdate.c -o $@

.include <bsd.prog.mk>
