#	Makefile	4.2	85/02/05
#
DESTDIR=
CFLAGS=	-O
OBJS=	main.o init.o subr.o gettytab.o get_date.o

all:	getty

getty:	${OBJS}
	${CC} -o getty ${OBJS}

install: all
	install -s getty ${DESTDIR}/etc/getty
	install -c -m 444 gettytab ${DESTDIR}/etc/gettytab

clean:
	rm -f getty ${OBJS} a.out core errs

main.o: gettytab.h
init.o: gettytab.h
subr.o: gettytab.h
