CFLAGS = -O -n -s

all:	c0 c1 c2

cmp:	all
	cmp c0 /lib/c0
	cmp c1 /lib/c1
	cmp c2 /lib/c2
	rm c0 c1 c2 *.o

cp:	all
	cp c0 /lib/c0
	cp c1 /lib/c1
	cp c2 /lib/c2
	rm c0 c1 c2 *.o

fcp:	fc1
	cp fc1 /lib/fc1
	rm fc1 c1*.o

c0: c00.o c01.o c02.o c03.o c04.o c05.o
	cc $(CFLAGS) -o c0 c00.o c01.o c02.o c03.o c04.o c05.o

c00.o c01.o c02.o c03.o c04.o c05.o: c0.h

c1: c10.o c11.o c12.o c13.o table.o
	cc $(CFLAGS) -o c1 c10.o c11.o c12.o c13.o table.o

fc1: c10.o c11.o c12.o c13.o table.o
	cc $(CFLAGS) -f -o fc1 c10.o c11.o c12.o c13.o table.o

c10.o c11.o c12.o c13.o: c1.h

table.o: table.s cvopt
	cvopt <table.s >table.i
	as -o table.o table.i
	rm table.i

c2: c20.o c21.o
	cc -i -O -s -o c2 c20.o c21.o

c20.o c21.o: c2.h

cvopt:	cvopt.c
	cc $(CFLAGS) -o cvopt cvopt.c
