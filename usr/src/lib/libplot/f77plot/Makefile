#	Makefile	1.1	85/01/02
#
CFLAGS=	-O

OBJS = arc.o box.o circle.o clospl.o cont.o dot.o erase.o label.o \
	line.o linemd.o move.o open.o point.o space.o

../libf77plot:	${OBJS}
	ar cu ../libf77plot `lorder ${OBJS} | tsort`

clean:
	rm -f ${OBJS} errs a.out core
