#	Makefile	5.2	87/05/31
#
DESTDIR=
CFLAGS=	-O
SRCS=	access.c command.c expr.c format.c input.c main.c message.c \
	opset.c optab.c output.c pcs.c print.c runpcs.c setup.c sym.c
OBJS=	access.o command.o expr.o format.o input.o main.o message.o \
	opset.o optab.o output.o pcs.o print.o runpcs.o setup.o sym.o

adb:	${OBJS}
	${CC} -o adb ${LDFLAGS} ${OBJS}

defs.h:		mac.h mode.h
mode.h:		machine.h

access.o:	defs.h access.c
command.o:	defs.h command.c
expr.o:		defs.h expr.c
format.o:	defs.h format.c
input.o:	defs.h input.c
main.o:		defs.h main.c
message.o:	mac.h mode.h message.c
	${CC} -R ${CFLAGS} -c message.c
opset.o:	defs.h optab.h opset.c
optab.o:	defs.h optab.h instrs optab.c
	${CC} -R ${CFLAGS} -c optab.c
output.o:	defs.h output.c
pcs.o:		defs.h pcs.c
print.o:	defs.h print.c
runpcs.o:	defs.h runpcs.c
setup.o:	defs.h setup.c
sym.o:		defs.h sym.c

clean:
	rm -f adb ${OBJS} core t.c errs

depend:

lint:
	lint -bx ${SRCS}

install: adb
	install -s adb ${DESTDIR}/bin/adb
