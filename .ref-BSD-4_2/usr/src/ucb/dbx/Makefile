#	@(#)Makefile	4.6 (Berkeley) 7/15/83
#
# make file for debugger "dbx"
#
# The file "defs.h" is included by all.
#
# N.B.:
#    My version of cerror automatically catches certain errors
# such as out of memory, I/O error.  If you re-make this with
# the standard cerror, the program could fault unexpectedly.
#

.SUFFIXES:
.SUFFIXES: .h .c .s .o

AOUT	= tdbx
DESTDIR =
DEST	= ${DESTDIR}/usr/ucb/dbx

LIBRARIES =

CC	= cc
CFLAGS	= # -g
LDFLAGS	= -g

OBJ = \
    y.tab.o \
    asm.o \
    events.o \
    c.o \
    cerror.o \
    check.o \
    coredump.o \
    debug.o \
    eval.o \
    fortran.o \
    keywords.o \
    languages.o \
    library.o \
    lists.o \
    machine.o \
    main.o \
    mappings.o \
    names.o \
    object.o \
    operators.o \
    pascal.o \
    printsym.o \
    process.o \
    runtime.o \
    scanner.o \
    source.o \
    symbols.o \
    tree.o \
    ops.o

HDR = \
    asm.h \
    events.h \
    c.h \
    check.h \
    coredump.h \
    eval.h \
    fortran.h \
    keywords.h \
    languages.h \
    lists.h \
    machine.h \
    main.h \
    mappings.h \
    names.h \
    object.h \
    operators.h \
    pascal.h \
    printsym.h \
    process.h \
    runtime.h \
    source.h \
    scanner.h \
    symbols.h \
    tree.h \
    ops.h

SRC = \
    defs.h \
    commands.y \
    asm.c \
    events.c \
    c.c \
    cerror.s \
    check.c \
    coredump.c \
    debug.c \
    eval.c \
    fortran.c \
    keywords.c \
    languages.c \
    library.c \
    lists.c \
    machine.c \
    main.c \
    mappings.c \
    names.c \
    object.c \
    operators.c \
    pascal.c \
    printsym.c \
    process.c \
    runtime.c \
    scanner.c \
    source.c \
    symbols.c \
    tree.c \
    ops.c

.c.o:
	@echo "compiling $*.c"
	@${CC} ${CFLAGS} -c $*.c

.s.o:
	@echo "assembling $*.s"
	@${CC}  -c $*.s

.c.h:
	./makedefs -f $*.c $*.h

${AOUT}: makedefs mkdate ${HDR} ${OBJ}
	@rm -f date.c
	@./mkdate > date.c
	@echo "linking"
	@${CC} ${LDFLAGS} date.c ${OBJ} ${LIBRARIES} -o ${AOUT}

profile: ${HDR} ${OBJ}
	@rm -f date.c
	@./mkdate > date.c
	@echo "linking with -p"
	@${CC} ${LDFLAGS} -p date.c ${OBJ} ${LIBRARIES} -o ${AOUT}

y.tab.c: commands.y
	yacc -d commands.y

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

#
# Create a tar file called "tape" containing relevant files.
#

tape:
	tar cfv tape Makefile READ_ME TO_DO ${SRC} makedefs.c mkdate.c

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
