CFLAGS = -n -O

all:	prep
	:

cp:	all
	cp prep /bin
	rm prep *.o

cmp:	all
	cmp prep /bin/prep
	rm prep *.o

prep:	prep0.o prep1.o prep2.o; cc -s -o prep -n *.o

prep0.o: prep0.c prep.h
prep1.o: prep1.c prep.h
prep2.o: prep2.c
