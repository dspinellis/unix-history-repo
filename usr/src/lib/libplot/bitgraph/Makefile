#	Makefile	4.4	83/11/11
#
CFLAGS=	-O
OBJS=	box.o circle.o arc.o cont.o dot.o erase.o line.o linemod.o point.o \
	label.o move.o open.o close.o space.o

../libplotbg:	${OBJS}
	ar cu ../libplotbg ${OBJS}

${OBJS}:	bg.h

clean:
	rm -f ${OBJS} errs a.out core
