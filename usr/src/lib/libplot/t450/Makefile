#	Makefile	4.2	83/07/09
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o dot.o erase.o label.o \
	line.o linmod.o move.o open.o point.o space.o subr.o

../libt450:	${OBJS}
	ar cu ../libt450 ${OBJS}

clean:
	rm -f ${OBJS} errs a.out core
