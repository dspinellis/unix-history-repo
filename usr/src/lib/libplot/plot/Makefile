#	Makefile	4.1	83/06/27
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o cont.o dot.o erase.o label.o \
	line.o linmod.o move.o open.o point.o putsi.o space.o

../libplot.a:	${OBJS}
	ar cu ../libplot.a ${OBJS}

clean:
	rm -f ${OBJS} errs a.out core
