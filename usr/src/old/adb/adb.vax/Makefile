#
#	Makefile	5.1	87/06/03
#
DESTDIR= 
DFLAGS = -DADB
CFLAGS=	-O -w ${DFLAGS}
CFILES= access.c command.c expr.c format.c input.c main.c message.c \
	opset.c optab.c output.c pcs.c print.c runpcs.c setup.c sym.c
OFILES= access.o command.o expr.o format.o input.o main.o message.o \
	opset.o optab.o output.o pcs.o print.o runpcs.o setup.o sym.o

#
#	path to the directory containing as
#
AS =	../../as/as.vax

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
pcs.o:		defs.h pcs.c
print.o:	defs.h print.c
runpcs.o:	defs.h runpcs.c
setup.o:	defs.h setup.c
sym.o:		defs.h sym.c

opset.o:	defs.h opset.c
	${CC} ${CFLAGS} -c    -I. -I$(AS) opset.c
optab.o:	defs.h optab.c instrs.adb
	${CC} ${CFLAGS} -c -R -I. -I$(AS) optab.c
instrs.adb: $(AS)/instrs
	(echo FLAVOR ADB; cat $(AS)/instrs) | awk -f $(AS)/instrs > instrs.adb

install:
	install -s adb ${DESTDIR}/bin

clean:
	rm -f adb instrs.adb errs ${OFILES}

depend:

print:
	@ls -l | pr
	@pr -f Makefile *.h ${CFILES}
