CFLAGS = -n -O -s

m4:	m4.o m4y.o
	cc -n -s -o m4 m4.o m4y.o

all:	m4

cmp:	m4
	cmp m4 /bin/m4
	rm m4 *.o

cp:	m4
	cp m4 /bin/m4
	rm m4 *.o
