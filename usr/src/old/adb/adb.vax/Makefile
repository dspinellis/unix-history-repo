#
#	Makefile	4.1	81/05/14
#
DESTDIR= 
CFLAGS=	-O -w
CFILES= access.c command.c expr.c format.c input.c main.c message.c \
	opset.c optab.c output.c pcs.c print.c runpcs.c setup.c sym.c
OFILES= access.o command.o expr.o format.o input.o main.o message.o \
	opset.o optab.o output.o pcs.o print.o runpcs.o setup.o sym.o

adb:	${OFILES}
	${CC} -o adb ${OFILES}

defs.h:		mac.h mode.h
mode.h:		machine.h

access.o:	defs.h access.c
command.o:	defs.h command.c
expr.o:		defs.h expr.c
format.o:	defs.h format.c
input.o:	defs.h input.c
main.o:		defs.h main.c
message.o:	mac.h mode.h message.c
	${CC} -c -R message.c
opset.o:	defs.h opset.c
optab.o:	defs.h optab.c
	${CC} -S -R  optab.c
output.o:	defs.h output.c
pcs.o:		defs.h pcs.c
print.o:	defs.h print.c
runpcs.o:	defs.h runpcs.c
setup.o:	defs.h setup.c
sym.o:		defs.h sym.c

install:
	install -s adb ${DESTDIR}/bin

clean:
	rm -f adb ${OFILES}

print:
	@ls -l | pr
	@pr -f Makefile *.h ${CFILES}
