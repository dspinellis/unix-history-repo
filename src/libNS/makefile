# modified i/o library to to concurrent r/w access
STDSRC = /usr/src/libc/stdio
DESTDIR = /usr/lib
CFLAGS  = -O -I.
libNS.a: work stdio.h
	cp ${STDSRC}/*.[cs] work
	cp *.c work
	cc -c -O -I. work/*.c
	cp work/doprnt.s doprnt.c
	cc -E doprnt.c | as -o doprnt.o
	-for i in *.o ; do ld -x -r $$i; mv a.out $$i; done
	ar cr libNS.a `lorder *.o | tsort`
	touch stdio.h

install:
	cp stdio.h /usr/include/nstdio.h
	cp libNS.a ${DESTDIR}

clean:
	rm -r work
	rm -f *.o libNS.a

work:
	mkdir work

doprint.o:
	cp work/doprnt.s doprnt.c
	cc -E doprnt.c | as -o doprnt.o

final:
	-for i in *.o ; do ld -x -r $$i; mv a.out $$i; done
	ar cr libNS.a `lorder *.o | tsort`
	rm doprnt.c
