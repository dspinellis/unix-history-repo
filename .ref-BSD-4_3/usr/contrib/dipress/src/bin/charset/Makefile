# Makefile for Unix Interpress utilities
#
# Copyright (c) 1984, 1985 Xerox Corp.
#
# HISTORY
#     Dec 1, 1985	Lee Moore, Xerox WRC
#	added lint
#

DESTBIN=../../../bin
DESTINCLUDE=../../../include
DESTLIB=../../../lib

charset: charset.o $(DESTLIB)/libip.a
	$(CC) $(CFLAGS) -o charset charset.o $(DESTLIB)/libip.a

charset.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c charset.c

lint:
	lint -ab -I$(DESTINCLUDE) charset.c $(DESTLIB)/llib-lip.ln -lc
	

install: $(DESTBIN)/charset

$(DESTBIN)/charset: charset
	install -c -s charset $(DESTBIN)

clean:
	rm -f charset *.o
