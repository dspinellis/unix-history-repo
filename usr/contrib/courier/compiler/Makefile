#	Makefile	4.2	83/07/07
#
OBJS=	courier.o main.o scanner.o sem.o code1.o code2.o
SRCS=	courier.y main.c scanner.l sem.c code1.c code2.c
YFLAGS=	-d
CFLAGS=	-O
DESTDIR=

courier:	${OBJS}
		${CC} ${LDFLAGS} -o courier ${OBJS} -ll

courier.o:	courier.y
scanner.o:	courier.y

${OBJS}:	Courier.h

lint:		y.tab.c
		lint -hnux ${SRCS}

install:	courier
		install -s courier ${DESTDIR}/usr/new/courier

clean:
		rm -f ${OBJS} y.tab.[ch] lex.yy.c courier
