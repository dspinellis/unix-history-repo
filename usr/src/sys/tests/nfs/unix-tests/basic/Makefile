#
#       @(#)Makefile	1.7 90/01/03 NFS Rev 2 Testsuite
#      1.5 Lachman ONC Test Suite  source
#
# to make tests, use 'make'
# to copy tests to another directory, use 'make copy DESTDIR=dir'
# to copy source to another directory, use 'make dist DESTDIR=dir'

TESTS = test1 test2 test3 test4 test5 test6 test7 test8 test9
AUXTESTS = test4a test5a test5b test7a test7b
DESTDIR = /no/such/path

CFLAGS = -O $(COMPAT)

all: origtests auxtests

origtests: $(TESTS)
auxtests: $(AUXTESTS)

test1: test1.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test2: test2.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test3: test3.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test4: test4.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test4a: test4a.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test5: test5.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test5a: test5a.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test5b: test5b.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test6: test6.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test7: test7.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test7a: test7a.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test7b: test7b.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test8: test8.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

test9: test9.c subr.o
	$(CC) $(CFLAGS) -o $@ $@.c subr.o $(LIBS)

clean:
	rm -f *.o $(TESTS) $(AUXTESTS)

copy: $(TESTS)
	cp runtests $(TESTS) $(AUXTESTS) $(DESTDIR)

dist:
	cp runtests Makefile *.c *.h $(DESTDIR)
