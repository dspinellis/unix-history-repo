#	Makefile	4.3	83/09/21
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o dot.o erase.o label.o \
	line.o linmod.o move.o open.o point.o space.o subr.o

../lib300s:	${OBJS}
	ar cu ../lib300s ${OBJS}

clean:
	rm -f ${OBJS} errs a.out core
