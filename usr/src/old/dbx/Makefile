#	Makefile	4.10	84/08/17
#	@(#)Makefile	4.10 (Berkeley) %G%
#
# make file for debugger "dbx"
#
# The file "defs.h" is included by all.
#
AOUT=	tdbx
DESTDIR=
DEST=	${DESTDIR}/usr/ucb/dbx
LIBS=	
CFLAGS=	-O
YFLAGS=	-d
LDFLAGS=
HDR=	asm.h events.h c.h check.h coredump.h eval.h fortran.h keywords.h \
	languages.h lists.h machine.h main.h mappings.h modula-2.h names.h \
	object.h operators.h pascal.h printsym.h process.h re.h \
	runtime.h scanner.h source.h stabstring.h symbols.h tree.h ops.h
SRCS=	asm.c events.c c.c cerror.s check.c coredump.c debug.c eval.c \
	fortran.c keywords.c languages.c library.c lists.c machine.c main.c \
	mappings.c modula-2.c names.c object.c operators.c pascal.c printsym.c \
	process.c re.c runtime.c scanner.c source.c symbols.c tree.c ops.c
OBJS=	y.tab.o asm.o events.o c.o cerror.o check.o coredump.o \
	debug.o eval.o fortran.o keywords.o languages.o library.o \
	lists.o machine.o main.o mappings.o modula-2.o names.o \
	object.o operators.o pascal.o printsym.o process.o re.o \
	runtime.o scanner.o source.o stabstring.o symbols.o tree.o \
	ops.o
# N.B.:
#    My version of cerror automatically catches certain errors
# such as out of memory, I/O error.  If you re-make this with
# the standard cerror, the program could fault unexpectedly.
#

.SUFFIXES:
.SUFFIXES: .h .c .s .o

.c.o:
	${CC} ${CFLAGS} -c $*.c

.s.o:
	${CC} -c $*.s

.c.h:
	@./makedefs -f $*.c $*.h

${AOUT}: makedefs mkdate ${HDR} ${OBJS}
	@rm -f date.c
	@./mkdate > date.c
	@echo "linking"
	@${CC} ${LDFLAGS} date.c ${OBJS} ${LIBS} -o ${AOUT}

profile: ${HDR} ${OBJS}
	@rm -f date.c
	@./mkdate > date.c
	@echo "linking with -p"
	@${CC} ${LDFLAGS} -p date.c ${OBJS} ${LIBS} -o ${AOUT}

y.tab.c: commands.y
	${YACC} ${YFLAGS} commands.y

makedefs: makedefs.c library.o cerror.o
	${CC} makedefs.c library.o cerror.o -o makedefs

mkdate: mkdate.c
	${CC} mkdate.c -o mkdate

print:
	@echo "don't print it, it's too long"

#
# Don't worry about the removal of header files, they're created from
# the source files.
#
clean:
	rm -f ${HDR} ${OBJ} y.tab.c y.tab.h \
	    ${AOUT} mkdate mkdate.o makedefs makedefs.o date.c core

install:
	install ${AOUT} ${DEST}

tags:	defs.h ${SRCS}
	ctags -w defs.h ${SRCS}

#
# Header dependencies are purposely incomplete since header files
# are "written" every time the accompanying source file changes even if
# the resulting contents of the header don't change.  The alternative is
# to force a "makedefs" to be invoked for every header file each time dbx
# is made.
#
# Also, there should be a dependency of scanner.o and keywords.o on y.tab.h
# but misfortunately silly make does a "makedefs y.tab.c y.tab.h" which
# destroys y.tab.h.
#
symbols.o tree.o check.o eval.o events.o: operators.h
