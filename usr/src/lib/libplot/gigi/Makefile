#	Makefile	4.1	83/11/10
#
CFLAGS=	-O
OBJS=	arc.o box.o circle.o close.o cont.o dot.o erase.o label.o \
	line.o linemod.o move.o open.o point.o space.o

../libplotgigi:	${OBJS}
	ar cu ../libplotgigi ${OBJS}

${OBJS}:	gigi.h

clean:
	rm -f ${OBJS} errs a.out core
