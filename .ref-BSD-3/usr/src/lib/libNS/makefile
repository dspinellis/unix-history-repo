# modified i/o library to do concurrent r/w access
STDSRC = /usr/src/libc/stdio
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
	install -c stdio.h ${DESTDIR}/usr/include/nstdio.h
	cp libNS.a ${DESTDIR}/usr/lib

clean:
	rm -rf work
	rm -f *.o libNS.a doprnt.c

work:
	mkdir work

doprint.o:
	cp work/doprnt.s doprnt.c
	cc -E doprnt.c | as -o doprnt.o

final:
	-for i in *.o ; do ld -x -r $$i; mv a.out $$i; done
	ar cr libNS.a `lorder *.o | tsort`
	rm doprnt.c
