#	@(#)Makefile	4.2	(Berkeley)	82/11/15
#
COPT=
CFLAGS=-O

fsck: fsck.o ufs_subr.o ufs_tables.o
	cc ${COPT} -o fsck fsck.o ufs_subr.o ufs_tables.o

fsck.o: fsck.c
	cc ${COPT} ${CFLAGS} -S fsck.c
	/lib/c2 fsck.s | sed -f asm.sed | as -o tmp.o
	ld -r -o fsck.o tmp.o -lc
	rm tmp.o

ufs_subr.o: /sys/sys/ufs_subr.c
	cc ${COPT} -c /sys/sys/ufs_subr.c

ufs_tables.o: /sys/sys/ufs_tables.c
	cc ${COPT} -c /sys/sys/ufs_tables.c

install: fsck
	install fsck ${DESTDIR}/etc/fsck

clean:
	rm -f fsck fsck.o ufs_subr.o ufs_tables.o
	rm -f fsck.s tmp.o errs a.out core
