#
#       @(#)Makefile	1.3 90/01/03 NFS Rev 2 Testsuite
#      1.4 Lachman ONC Test Suite  source
#

RUNTESTS= open-unlk open-chmod dupreq excltest negseek rename holey
TESTS= $(RUNTESTS) nstat stat stat2 touchn fstat
DESTDIR=/no/such/path
CC=cc
CFLAGS=-O $(SYSTYPE)

rall: $(RUNTESTS)
all: $(TESTS)

$(TESTS):
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBS)

open-unlk: open-unlk.c
open-chmod: open-chmod.c
dupreq: dupreq.c
excltest: excltest.c
fstat: fstat.c
negseek: negseek.c
nstat: nstat.c
rename: rename.c
stat: stat.c
stat2: stat2.c
touchn: touchn.c
holey: holey.c

clean:
	rm -f *.o $(TESTS) testfile exctest.file holeyfile

copy: $(TESTS)
	cp runtests runtests.wrk $(TESTS) $(DESTDIR)

dist:
	cp runtests runtests.wrk Makefile *.c $(DESTDIR)
