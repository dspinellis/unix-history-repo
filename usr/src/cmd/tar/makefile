V6LIB=/usr/src/libc/v6
CFLAGS=-O

all:	tar

cp:	all
	cp tar /bin
	rm -f tar *.o

cmp:	all
	cmp tar /bin/tar
	rm -f tar *.o

tar:	tar.c
	cc -i -s -O tar.c -o tar

v6tar:	tar.o access.o chown.o execl.o ftime.o gtty.o lseek.o stat.o syscall.o time.o
	cc -i -s -O *.o -o v6tar

access.o:	$(V6LIB)/access.c
	cc -c -O $?

chown.o:	$(V6LIB)/chown.c
	cc -c -O $?

execl.o:	$(V6LIB)/execl.c
	cc -c -O $?

ftime.o:	$(V6LIB)/ftime.c
	cc -c -O $?

gtty.o:	$(V6LIB)/gtty.c
	cc -c -O $?

lseek.o:	$(V6LIB)/lseek.c
	cc -c -O $?

stat.o:	$(V6LIB)/stat.c
	cc -c -O $?

syscall.o:	$(V6LIB)/syscall.s
	cc -c -O $?

time.o:	$(V6LIB)/time.s
	cc -c -O $?
