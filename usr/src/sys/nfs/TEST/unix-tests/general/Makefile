#
#       @(#)Makefile	1.3 90/01/03 NFS Rev 2 Testsuite
#      1.3 Lachman ONC Test Suite  source
#

DESTDIR=/no/such/path
CFLAGS=-O $(SYSTYPE)
CC=cc

stat: stat.c
	$(CC) $(CFLAGS) -o stat stat.c -lm

clean:
	rm -f *.time *.o stat

copy dist:
	cp Makefile runtests runtests.wrk *.sh *.c mkdummy rmdummy \
	   nroff.in makefile.tst \
	   $(DESTDIR)
