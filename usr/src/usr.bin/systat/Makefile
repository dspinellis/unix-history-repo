#       Makefile     1.1     83/10/01
DESTDIR=
CFLAGS= -O
OBJS=   main.o fetch.o pigs.o swap.o
LIBS=   -lcurses -ltermlib -lm

systat: ${OBJS}
	${CC} -o systat ${OBJS} ${LIBS}

main.o:	systat.h
fetch.o:systat.h
pigs.o:	systat.h
swap.o:	systat.h

install: systat
	install -s systat ${DESTDIR}/usr/lfl/bin/systat

clean:
	rm -f ${OBJS} systat core a.out errs
