#
# $Header: Makefile,v 2.8 84/10/10 04:56:16 steveg Exp $
# $Locker:  $
#
CFLAGS=-O
LPR=lprw
USRINCLUDE=/usr/include

all: hyroute

.l.o:
	lex -v $*.l
	mv lex.yy.c $*.c
	${CC} -c ${CFLAGS} $*.c

.c.o:
	${CC} -c ${CFLAGS} $*.c

hyroute: hyr_main.o hyr_lex.o
	${CC} ${CFLAGS} -o hyroute hyr_main.o hyr_lex.o -ll

install: hyroute
	install -s hyroute ${DESTDIR}/usr/new/hyroute

hyr_main.o:	hyr_main.c hyr_sym.h ${USRINCLUDE}/vaxif/if_hy.h
hyr_lex.o:	hyr_lex.l hyr_sym.h

clean:
	-rm -f *.o hyroute hyr_lex.c

print:
	cat -n Makefile | fold -128 | pr -h `pwd`/Makefile >/usr/tmp/hyr_print
	cat -n README | fold -128 | pr -h `pwd`/README >>/usr/tmp/hyr_print
	cat -n test.input | fold -128 | pr -h `pwd`/test.input >>/usr/tmp/hyr_print
	cat -n hyr_sym.h | fold -128 | pr -h `pwd`/hyr_sym.h >>/usr/tmp/hyr_print
	cat -n hyr_lex.l | fold -128 | pr -h `pwd`/hyr_lex.l >>/usr/tmp/hyr_print
	cat -n hyr_main.c | fold -128 | pr -h `pwd`/hyr_main.c >>/usr/tmp/hyr_print
	${LPR} /usr/tmp/hyr_print
	rm /usr/tmp/hyr_print
