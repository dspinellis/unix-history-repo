#	Makefile	4.1	83/07/06
#
DESTDIR=
CFLAGS=	-O
OBJS=	main.o init.o subr.o gettytab.o

all:	getty

getty:	${OBJS}
	${CC} -o getty ${OBJS}

install: all
	install -s getty ${DESTDIR}/etc/getty

clean:
	rm -f getty ${OBJS} a.out core errs

main.o: gettytab.h
init.o: gettytab.h
subr.o: gettytab.h
