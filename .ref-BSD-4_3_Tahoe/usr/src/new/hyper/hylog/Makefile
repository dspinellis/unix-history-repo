#
# $Header: Makefile,v 2.8 84/10/10 04:43:51 steveg Exp $
# $Locker:  $
#
CFLAGS=-O
USRINCLUDE=/usr/include

all: hylog hyelog

.l.o:
	lex -v $*.l
	mv lex.yy.c $*.c
	${CC} -c ${CFLAGS} $*.c

.c.o:
	${CC} -c ${CFLAGS} $*.c

hyelog: hyelog.c ${USRINCLUDE}/vaxif/if_hyreg.h ${USRINCLUDE}/vaxif/if_hy.h
	${CC} ${CFLAGS} -o hyelog hyelog.c

hylog: hylog.c ${USRINCLUDE}/vaxif/if_hyreg.h ${USRINCLUDE}/vaxif/if_hy.h
	${CC} ${CFLAGS} -o hylog hylog.c

install: all
	install -s hylog ${DESTDIR}/usr/new/hylog
	install -s hyelog ${DESTDIR}/usr/new/hyelog

clean:
	-rm -f *.o hylog hyelog
