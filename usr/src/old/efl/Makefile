#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1	(Berkeley)	12/12/87
#
CFLAGS=	-O -w
LIBC=	/lib/libc.a
SRCS=	addr.c alloc.c blklab.c dcl.c dclgen.c error.c exec.c field.c free.c \
	gram.c icfile.c init.c io.c lex.c main.c misc.c mk.c namgen.c pass2.c \
	print.c simple.c struct.c symtab.c tailor.c temp.c
OBJS=	addr.o alloc.o blklab.o dcl.o dclgen.o error.o exec.o field.o free.o \
	gram.o icfile.o init.o io.o lex.o main.o misc.o mk.o namgen.o pass2.o \
	print.o simple.o struct.o symtab.o tailor.o temp.o

all: efl

efl: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

# gram.c can no longer be made on a pdp11 because of yacc limits
gram.c: gram.head gram.dcl gram.expr gram.exec tokdefs
	(sed < tokdefs "s/#define/%token/" ; \
	cat gram.head gram.dcl gram.expr gram.exec) > gram.in
	${YACC} ${YFLAGS} gram.in
	(echo "#define YYDEBUG"; cat y.tab.c) > gram.c
	rm -f gram.in y.tab.c

lex.c: fixuplex
	lex lex.l
	sh fixuplex
	mv lex.yy.c lex.c

tokdefs: tokens
	grep -n . < tokens | sed "s/\([^:]*\):\(.*\)/#define \2 \1/" > tokdefs

clean: FRC
	rm -f ${OBJS} core efl gram.c lex.c tokdefs

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 efl ${DESTDIR}/usr/bin/efl

lint: ${SRCS} FRC
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS} FRC
	ctags ${SRCS}

test:
	./efl "system=gcos" efltest/Hard.e >z1 2>z2
	cmp z1 efltest/Hard.out
	./efl "system=gcos" efltest/Band.e >z1 2>z2
	cmp z1 efltest/Band.out
	./efl "system=gcos" efltest/Buram.e >z1 2>z2
	cmp z1 efltest/Buram.out
	./efl "system=gcos" efltest/Dgl.e >z1 2>z2
	cmp z1 efltest/Dgl.out
	rm -f z1 z2
	@echo TEST OK

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

addr.o: addr.c defs /usr/include/stdio.h
alloc.o: alloc.c defs /usr/include/stdio.h
blklab.o: blklab.c defs /usr/include/stdio.h
dcl.o: dcl.c defs /usr/include/stdio.h
dclgen.o: dclgen.c defs /usr/include/stdio.h
error.o: error.c defs /usr/include/stdio.h
exec.o: exec.c defs /usr/include/stdio.h
field.o: field.c defs /usr/include/stdio.h
free.o: free.c defs /usr/include/stdio.h
gram.o: gram.c defs /usr/include/stdio.h
icfile.o: icfile.c defs /usr/include/stdio.h
init.o: init.c defs /usr/include/stdio.h tokdefs
io.o: io.c /usr/include/ctype.h defs /usr/include/stdio.h
lex.o: lex.c /usr/include/stdio.h /usr/include/ctype.h defs
lex.o: /usr/include/stdio.h tokdefs
main.o: main.c defs /usr/include/stdio.h
misc.o: misc.c /usr/include/ctype.h defs /usr/include/stdio.h
mk.o: mk.c defs /usr/include/stdio.h
namgen.o: namgen.c defs /usr/include/stdio.h
pass2.o: pass2.c defs /usr/include/stdio.h /usr/include/ctype.h
print.o: print.c defs /usr/include/stdio.h
simple.o: simple.c /usr/include/ctype.h defs /usr/include/stdio.h
struct.o: struct.c defs /usr/include/stdio.h
symtab.o: symtab.c defs /usr/include/stdio.h
tailor.o: tailor.c defs /usr/include/stdio.h
temp.o: temp.c defs /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
