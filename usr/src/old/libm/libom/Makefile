#	@(#)Makefile	4.2	%G%
#
SCCSID = "@(#)Makefile 4.2 %G%"

CFLAGS=-O

DESTDIR=

INSTALL= install

SRCS=	asin.c atan.c hypot.c jn.c j0.c j1.c pow.c gamma.c \
	log.c sin.c sqrt.c tan.c tanh.c sinh.c erf.c exp.c floor.c

FILES= asin.o atan.o hypot.o jn.o j0.o j1.o pow.o gamma.o \
	log.o sin.o sqrt.o tan.o tanh.o sinh.o erf.o exp.o floor.o

TAGSFILE=tags

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

libom.a libom_p.a : $(FILES)
	cd profiled; ar cru ../libom_p.a $(FILES)
	ar cru libom.a $(FILES)

install: libom.a libom_p.a
	${INSTALL} libom.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/usr/lib/libom.a
	${INSTALL} libom_p.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/usr/lib/libom_p.a

tags:
	cwd=`pwd`; \
 	for i in ${SRCS}; do \
 		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
 	done

clean:
	rm -f *.o profiled/*.o libom.a libom_p.a tags
