#	Makefile	4.3	83/11/10
#
CFLAGS=	-O
OBJS=	box.o circle.o arc.o dot.o erase.o line.o linemod.o point.o \
	label.o move.o open.o close.o space.o

../libplotbg:	${OBJS}
	ar cu ../libplotbg ${OBJS}

${OBJS}:	bg.h

clean:
	rm -f ${OBJS} errs a.out core
