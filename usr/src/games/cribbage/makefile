OBJS=	crib.o support.o cards.o score.o io.o
TOBJS=	test.o cards.o score.o io.o
CFLAGS=	-O

all: cribbage crib.instr

cribbage:	${OBJS}
		${CC} ${CFLAGS} -o cribbage ${OBJS}

test:		${TOBJS}
		${CC} ${CFLAGS} -o test ${TOBJS}

crib.o:		deck.h  cribbage.h
support.o:	deck.h  cribbage.h
test.o:		deck.h
cards.o:	deck.h
score.o:	deck.h
io.o:		deck.h

crib.instr: cribbage.n macro
		nroff cribbage.n > crib.instr
