#	@(#)Makefile	4.2 (Berkeley) %G%
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
    eval.o \
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

.c.o:
	@echo "compiling $*.c"
	@${CC} ${CFLAGS} -c $*.c

.s.o:
	@echo "assembling $*.s"
	@${CC}  -c $*.s

.c.h:
	./makedefs -f $*.c $*.h

${AOUT}: makedefs ${HDR} ${OBJ}
	@echo "linking"
	@${CC} ${LDFLAGS} ${OBJ} ${LIBRARIES} -o ${AOUT}

profile: ${HDR} ${OBJ}
	@echo "linking with -p"
	@${CC} ${LDFLAGS} -p ${OBJ} ${LIBRARIES} -o ${AOUT}

y.tab.c: commands.y
	yacc -d commands.y

makedefs: makedefs.c library.o cerror.o
	${CC} makedefs.c library.o cerror.o -o makedefs

print:
	@echo "don't print it, it's too long"

#
# Don't worry about the removal of header files, they're created from
# the source files.
#

clean:
	rm -f ${HDR} ${OBJ} y.tab.c y.tab.h ${AOUT} core

install:
	install -s ${AOUT} ${DEST}

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
