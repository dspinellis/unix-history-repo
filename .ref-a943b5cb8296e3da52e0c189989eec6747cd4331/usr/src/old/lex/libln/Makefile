#	@(#)Makefile	4.2	%G%
#
CFLAGS = -O
SRCS=	allprint.c main.c reject.c yyless.c yywrap.c
OBJS=	allprint.o main.o reject.o yyless.o yywrap.o
TAGSFILE=tags

all: libln.a

libln.a: ${OBJS}
	ar rc libln.a ${OBJS}

install: all
	install libln.a ${DESTDIR}/usr/lib
	rm -f ${DESTDIR}/usr/lib/libl.a
	ln ${DESTDIR}/usr/lib/libln.a ${DESTDIR}/usr/lib/libl.a
	ranlib ${DESTDIR}/usr/lib/libln.a

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f ${OBJS} libln.a tags
