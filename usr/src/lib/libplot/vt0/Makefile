#	Makefile	4.1	83/06/27
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o dot.o erase.o label.o \
	line.o linmod.o move.o open.o point.o space.o subr.o

../libtvt0.a:	${OBJS}
	ar cu ../libtvt0.a ${OBJS}

clean:
	rm -f ${OBJS} errs a.out core
