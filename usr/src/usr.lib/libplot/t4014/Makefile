#	Makefile	4.5	85/01/02
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o subr.o

../lib4014:	${OBJS}
	ar cu ../lib4014 `lorder ${OBJS} | tsort`

clean:
	rm -f ${OBJS} errs a.out core
