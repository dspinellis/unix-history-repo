# Makefile for Unix Interpress utilities
#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
# HISTORY
#

DESTBIN=../../../bin
DESTINCLUDE=../../../include
DESTLIB=../../../lib

plot2ip: plot2ip.o $(DESTLIB)/libip.a $(DESTLIB)/libgetopt.a
	$(CC) $(CFLAGS) -o plot2ip plot2ip.o $(DESTLIB)/libip.a $(DESTLIB)/libgetopt.a

plot2ip.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c plot2ip.c

lint:
	lint -ab -I$(DESTINCLUDE) plot2ip.c $(DESTLIB)/llib-lip.ln -lc $(DESTLIB)/llib-lgetopt.ln

test:
	(cd Test; make test)


install: $(DESTBIN)/plot2ip

$(DESTBIN)/plot2ip: plot2ip
	install -c -s plot2ip $(DESTBIN)

clean:
	rm -f plot2ip *.o
	(cd Test; make clean)
