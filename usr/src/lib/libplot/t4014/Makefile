#	Makefile	4.3	83/07/09
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o subr.o

../libt4014:	${OBJS}
	ar cu ../libt4014 ${OBJS}

clean:
	rm -f ${OBJS} errs a.out core
