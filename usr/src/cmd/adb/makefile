CFLAGS = -i -O

all:	adb

cmp:	adb
	cmp adb /bin/adb
	rm adb *.o

cp:	adb
	cp adb /bin/adb
	rm adb *.o

adb:	access.o command.o expr.o findfn.o
adb:	format.o input.o opset.o main.o
adb:	message.o output.o pcs.o print.o
adb:	runpcs.o setup.o sym.o
adb:;	cc -o adb -i *.o


access.o:	defs.h
command.o:	defs.h
expr.o:		defs.h
findrtn.o:	defs.h
format.o:	defs.h
input.o:	defs.h
main.o:		defs.h
message.o:	mac.h mode.h
output.o:	defs.h
pcs.o:		defs.h
print.o:	defs.h
runpcs.o:	defs.h
setup.o:	defs.h
sym.o:		defs.h
