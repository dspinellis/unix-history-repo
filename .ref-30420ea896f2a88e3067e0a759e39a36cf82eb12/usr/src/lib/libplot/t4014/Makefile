#	Makefile	4.4	83/09/21
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o subr.o

../lib4014:	${OBJS}
	ar cu ../lib4014 ${OBJS}

clean:
	rm -f ${OBJS} errs a.out core
