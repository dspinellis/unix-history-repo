#	Makefile	4.2	83/11/10
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o cont.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o subr.o

../libplot2648:	${OBJS}
	ar cu ../libplot2648 ${OBJS}

${OBJS}:	hp2648.h

clean:
	rm -f ${OBJS} errs a.out core
