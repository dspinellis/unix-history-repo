HDRS=	cribbage.h deck.h cribcur.h
OBJS=	extern.o crib.o support.o cards.o score.o io.o addch.o
CFILES=	extern.c crib.c support.c cards.c score.c io.c addch.c
TOBJS=	test.o cards.o score.o io.o
CFLAGS=	-g

all: cribbage crib.instr

.c.o:
		@echo ${CC} -c ${CFLAGS} $*.c
		@cc -E ${CFLAGS} $*.c | xstr -c -
		@cc -c ${CFLAGS} x.c
		@mv x.o $*.o

cribbage:	${OBJS} xs.o
		${CC} ${CFLAGS} -o cribbage xs.o ${OBJS} -lcurses -ltermlib

xs.o: strings
		xstr
		${CC} -c ${CFLAGS} xs.c

crib.o io.o support.o: cribcur.h

test:		${TOBJS}
		${CC} ${CFLAGS} -o test ${TOBJS}

crib.instr: cribbage.n macro
		nroff cribbage.n > crib.instr

tags: ${HDRS} ${CFILES}
		ctags -u $?
		ed - tags < :ctfix
		sort tags -o tags

clean:
		rm -f ${OBJS} ? a.out core crib.instr

crib.o:		deck.h	cribbage.h	cribcur.h
support.o:	deck.h	cribbage.h	cribcur.h
test.o:		deck.h
cards.o:	deck.h
score.o:	deck.h
io.o:		deck.h			cribcur.h
