#	Makefile	4.2	83/11/10
#
CFLAGS=	-O
OBJS=	box.o circle.o arc.o dot.o erase.o line.o linemod.o point.o \
	label.o move.o open.o close.o space.o

../libbg:	${OBJS}
	ar cu ../libbg ${OBJS}

${OBJS}:	bg.h

clean:
	rm -f ${OBJS} errs a.out core
