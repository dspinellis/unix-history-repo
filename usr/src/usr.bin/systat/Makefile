#       Makefile     1.2     83/10/02
DESTDIR=
CFLAGS= -O
OBJS=   main.o fetch.o pigs.o swap.o mbufs.o
LIBS=   -lcurses -ltermlib -lm

systat: ${OBJS}
	${CC} -o systat ${OBJS} ${LIBS}

main.o:	systat.h
fetch.o:systat.h
pigs.o:	systat.h
swap.o:	systat.h
mbufs.o:systat.h

install: systat
	install -s systat ${DESTDIR}/usr/lfl/bin/systat

clean:
	rm -f ${OBJS} systat core a.out errs
