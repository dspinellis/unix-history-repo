#
# 	Makefile	1.2	82/10/24
#
DESTDIR=
OBJS=	crib.o support.o cards.o score.o io.o
TOBJS=	test.o cards.o score.o io.o

CSRCS = cards.c crib.c io.c score.c support.c test.c
HSRCS = cribbage.h deck.h
OTHERSRCS = cribbage.n macro
SRCS = $(CSRCS) $(HSRCS) $(OTHERSRCS)

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

install:
	install cribbage ${DESTDIR}/usr/games/cribbage

clean:
	rm -f ${OBJS} ${TOBJS} cibbage test
