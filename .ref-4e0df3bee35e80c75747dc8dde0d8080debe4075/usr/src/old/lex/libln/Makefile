#	@(#)Makefile	4.1	%G%
#
CFLAGS = -O

all: libln.a

libln.a:
	cc -c -O allprint.c main.c reject.c yyless.c
	cc -c -O yywrap.c
	rm -f libln.a
	ar rvc libln.a allprint.o main.o reject.o yyless.o yywrap.o
	rm allprint.o main.o reject.o yyless.o yywrap.o

install: all
	install libln.a ${DESTDIR}/usr/lib
	rm -f ${DESTDIR}/usr/lib/libl.a
	ln ${DESTDIR}/usr/lib/libln.a ${DESTDIR}/usr/lib/libl.a
	ranlib ${DESTDIR}/usr/lib/libln.a

clean:
	rm -f *.o libln.a
