# Makefile for mp2res
#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
#	Makefile for "mp2res" module
#
# HISTORY
#
#

DESTBIN=../../../bin
DESTINCLUDE=../../../include
DESTLIB=../../../lib

mp2res: mp2res.o $(DESTLIB)/libip.a
	$(CC) $(CFLAGS) -o mp2res mp2res.o $(DESTLIB)/libip.a  \
		$(DESTLIB)/libgetopt.a

mp2res.o: mp2res.c $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c mp2res.c

lint:
	lint -ab -I$(DESTINCLUDE) mp2res.c $(DESTLIB)/llib-lip.ln \
		$(DESTLIB)/llib-lgetopt.ln -lc

test:
	(cd Test; make test)

install: $(DESTBIN)/mp2res

$(DESTBIN)/mp2res: mp2res
	install -c -s mp2res $(DESTBIN)

clean:
	rm -f mp2res *.o
	(cd Test; make clean)
