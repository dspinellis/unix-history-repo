# Makefile for Unix Interpress utilities
#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
# HISTORY
# 03-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
#	Added "-lm" to the load line as a favor to Pyramids.
#
#

DESTBIN=../../../bin
DESTINCLUDE=../../../include
DESTLIB=../../../lib

maha: maha.o $(DESTLIB)/libip.a
	$(CC) $(CFLAGS) -o maha maha.o $(DESTLIB)/libip.a -lm

maha.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c maha.c

# auxilary functions

lint:
	lint -ab -I$(DESTINCLUDE) maha.c lint.c $(DESTLIB)/llib-lip.ln -lc

install: $(DESTBIN)/maha

$(DESTBIN)/maha: maha
	install -c -s maha $(DESTBIN)/maha

clean:
	rm -f maha *.o
