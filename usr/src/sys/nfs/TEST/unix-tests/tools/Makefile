#
#       @(#)Makefile	1.4 90/01/03 NFS Rev 2 Testsuite
#      1.5 Lachman ONC Test Suite  source
#
#
# 'make all'                    makes test programs
# 'make clean'                  cleans directories
# 'make copy DESTDIR=path'      copies test programs to path
# 'make dist DESTDIR=path'      copies sources to path

TESTS = tcp tcpd udp udpd dirdmp dirprt pmaptst pmapbrd
CFLAGS = -O $(SYSTYPE)
LIBES = $(LIBS) -lsocket

all: $(TESTS)

tcp: tcp.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBES)
tcpd: tcpd.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBES)
udp: udp.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBES)
udpd: udpd.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBES)
dirdmp: dirdmp.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBES)
dirprt: dirprt.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBES)
pmaptst: pmaptst.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBES)
pmapbrd: pmapbrd.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBES)

clean:
	rm -f *.o $(TESTS)

copy: $(TESTS)
	cp $(TESTS) $(DESTDIR)

dist:
	cp Makefile README *.c $(DESTDIR)
