CFLAGS = -n -O

all:	sed
	:

cp:	all
	cp sed /bin
	rm sed *.o

cmp:	all
	cmp sed /bin/sed
	rm sed *.o

sed:	sed0.o sed1.o; cc -s -o sed -n *.o

sed0.o: sed0.c sed.h
sed1.o: sed1.c sed.h
