DESTDIR = 
CFLAGS=-O -d2 -w
adb:	access.o command.o expr.o
adb:	format.o input.o opset.o optab.o main.o
adb:	message.o output.o
adb:	pcs.o
adb:	print.o
adb:	runpcs.o
adb:	setup.o sym.o
adb:;	$(CC) -o adb *.o 

defs.h:		mac.h mode.h
mode.h:		machine.h

access.o:	defs.h access.c
command.o:	defs.h command.c
expr.o:		defs.h expr.c
format.o:	defs.h format.c
input.o:	defs.h input.c
main.o:		defs.h main.c
message.o:	mac.h mode.h message.c
	$(CC) -S message.c
	ed <:rofix message.s
	as -o message.o message.s
	rm message.s
opset.o:	defs.h opset.c
optab.o:	defs.h optab.c
	$(CC) -S optab.c
	ed <:rofix optab.s
	as -o optab.o optab.s
	rm optab.s
output.o:	defs.h output.c
pcs.o:		defs.h pcs.c
print.o:	defs.h print.c
runpcs.o:	defs.h runpcs.c
setup.o:	defs.h setup.c
sym.o:		defs.h sym.c

install :
	install -s adb $(DESTDIR)/bin

clean :
	rm *.o 
