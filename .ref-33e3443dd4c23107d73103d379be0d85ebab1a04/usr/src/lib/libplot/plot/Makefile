#	Makefile	4.2	83/07/09
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o cont.o dot.o erase.o label.o \
	line.o linmod.o move.o open.o point.o putsi.o space.o

../libplot:	${OBJS}
	ar cu ../libplot ${OBJS}

clean:
	rm -f ${OBJS} errs a.out core
